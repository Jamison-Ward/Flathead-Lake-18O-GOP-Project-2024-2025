# JMW adapted code from FLBS Hall Lab modified by KAL in Aug 2024

## Program runs assuming an excel sheet with the following columns ## 
# X28, X29, X32, X34, X40, N2.Ar, O2.Ar, X32.28, X29.28, X34.32
# Column names are not case sensitive

################################################################################
## Initialize variables for file input and output paths ##

# Import libraries 
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

# EDIT: Set your working directory
setwd("C:/Users/jamis/Documents/Flathead Lake Biological Station/Church Lab/Instrumentation Manuals and Data/Membrane Inlet Mass Spectrometer/MIMS Code")

# EDIT: Specify your metadata and raw data files within the working directory
raw_data_file_path <- "MIMS_Raw_Data_Log_2024-10-25.csv"
metadata_file_path <- "MIMS_Sample_Metadata_2024-10-25_new.xlsx"

# EDIT: Specify your output file path and name
output_file_path <- paste0("C:/Users/jamis/Documents/Flathead Lake Biological Station/Church Lab/Instrumentation Manuals and Data/Membrane Inlet Mass Spectrometer/MIMS Code/")
save_file_name <- "MIMS_Data_Log_2024-11-08_stats.csv"

# EDIT: Specify your pressure units by commenting out the incorrect unit
pressure_unit <- "inHg"
# pressure_unit <- "mmHg"

# Import gas functions from "mims_gas_functions.R" (must be in the working directory)
source("C:/Users/jamis/Documents/Flathead Lake Biological Station/Church Lab/Instrumentation Manuals and Data/Membrane Inlet Mass Spectrometer/MIMS Code/mims_gas_functions.R")

################################################################################
## Read in the raw file and format data to pass to the gather_data function ##

read_raw_file <- function(raw_file_path) {
  
  # Count maximum number of columns in any row
  n_cols <- max(count.fields(raw_file_path, sep = ','))
  
  # Read the CSV file, assuming no header, naming columns V1, V2, etc., and fill missing fields with NA
  first_read <- read.csv(raw_file_path, header = FALSE, col.names = paste0("V", seq_len(n_cols)), 
                        fill = TRUE, stringsAsFactors = FALSE)
  
  # Remove leading empty rows from the data
  trimmed_read <- first_read[cumsum(complete.cases(first_read)) != 0,]
  
  # Remove first row (header) from the data
  header <- as.character(trimmed_read[1,])
  colnames(trimmed_read) <- header
  trimmed_read <- trimmed_read[-1, ]
  
  # Convert the 'Index' column to numeric type
  trimmed_read$Index <- as.numeric(as.character(trimmed_read$Index))
  
  # Calculate the range of the Index column
  range <- c(min(trimmed_read$Index), max(trimmed_read$Index))
  
  # Return the trimmed raw data along with the Index range in a list
  return(list(trimmed_read, range))
}

################################################################################
## Parse data from raw_data_trimmed using the associated metadata ##

gather_data <- function(raw_file, metadata_file, pressure_unit) {
  
  # Read in metadata
  metadata <- read_excel(metadata_file)
  
  # Remove rows with NA in SampleID column
  metadata <- metadata[!is.na(metadata$SampleID), ]
  
  # Determine the minimum and maximum metadata Index values
  meta_range <- c(min(metadata$Index), max(metadata$Index))
  
  # Check raw file contents to see if it includes all relevant metadata indices
  raw_data <- raw_file[[1]]
  raw_range <- raw_file[[2]]
  if (raw_range[1] > meta_range[1] || raw_range[2] < meta_range[2]) {
    print(paste0(raw_file, " does not contain all of the metadata indices.
                 Check to ensure this is the correct raw file for the associated metadata file."))
  }
  
  # Initialize new date data frame to contain parsed raw data 
  parsed_df <- data.frame()
  
  # Use metadata indices to parse raw data from the raw data file
  for (j in unique(metadata$Samp)) {
    
    # Extract metadata for individual sample
    sub_samp <- metadata[(metadata$Samp == j), ]

    # Check if metadata was found for individual sample
    if (nrow(sub_samp) == 0) {
      print(paste("No metadata found for Samp:", j))
      next
    }
    
    # Collect beginning and end indices for individual sample
    indices <- c(min(sub_samp$Index), max(sub_samp$Index))
    
    # Gather raw data including and between individual sample indices
    parsed_data <- raw_data[(raw_data$Index >= indices[1]) & (raw_data$Index <= indices[2]), ]

    # Check if raw data was found for individual sample
    if (nrow(parsed_data) == 0) {
      print(paste("No raw data found for Samp:", j))
      next
    }
    
    # Remove 'Index' column from parsed data frame
    parsed_data <- parsed_data[, -which(names(parsed_data) %in% c("Index"))]
    
    # Remove 'Time' column from parsed data frame
    parsed_data <- parsed_data[, -which(names(parsed_data) %in% c("Time"))]
    
    # Convert all columns in parsed data frame to numeric type
    parsed_data[] <- lapply(parsed_data, as.numeric)
    
    # Convert all columns in parsed data frame to character type
    new_data <- data.frame(lapply(parsed_data, function(x) type.convert(as.character(x), as.is = TRUE)), stringsAsFactors = FALSE)

    # Suppress all warnings for the subsequent code
    options(warn = -1)
    
    # Average individual sample temperatures
    temp_avg <- mean(sub_samp$Temp, na.rm = TRUE)
    
    # Average individual sample pressures, apply the inHg to mmHg pressure correction (if required)
    if (pressure_unit == "inHg") {
      press_avg <- mean(sub_samp$Pressure, na.rm = TRUE) * 25.4
      } 
    else if (pressure == "mmHg") {
      press_avg <- mean(sub_samp$Pressure, na.rm = TRUE)
      }
    else {
      print("Choose your pressure setting at the beginning of the code script")
      }
    
    # Create a data frame with calculated values (e.g., water density, gas saturation)
    # using functions from mims_gas_functions.R script
    calc_data <- data.frame(
      "Samp" = sub_samp[["Samp"]][1], 
      "SampleID" = sub_samp[["SampleID"]][1], 
      "Pressure" = press_avg, 
      "Temp" = temp_avg, 
      "Calibnum" = sub_samp[["Calibnum"]][1],
      "Depth" = sub_samp[["Depth"]][1],
      "WatDens" = watdens(temp_avg), 
      "N2Sat" = nsat(temp_avg, press_avg), 
      "O2Sat" = osat1(temp_avg, press_avg),
      "ArSat" = arsat(temp_avg, press_avg),
      "N2.ArSat" = nsat(temp_avg, press_avg) / arsat(temp_avg, press_avg),
      "O2.ArSat" = osat1(temp_avg, press_avg) / arsat(temp_avg, press_avg)
      )

    # Enable all warnings for the subsequent code
    options(warn = 0)
    
    # Replace NA values in 'Calibnum' column of 'calc_data' data frame using 'Sampnum' value from 'sub_samp' data frame
    calc_data$Calibnum[is.na(calc_data$Calibnum)] <- as.numeric(as.character(sub_samp[["Sampnum"]][1][is.na(sub_samp[["Calibnum"]][1])]))
    
    # Merge calculated data with parsed raw data
    temp_df <- merge(calc_data, new_data)
    
    # Append processed data to parsed data frame
    parsed_df <- rbind(parsed_df, temp_df)
  }
  
  return(parsed_df)
}

################################################################################
## Calculate sample in situ N2 concentration using linear model of standards ##

get_N2_conc <- function(parsed_data) {
  
  # Define the target column (targ_col) and saturation column (sat_col)
  targ_col <- "N2.Ar"
  sat_col <- "N2.ArSat"
  
  # Create new column for calculated data
  new_col <- "N2.Calc"
  
  # Loop through each calibration set in the parsed data
  for (i in 1:length(unique(parsed_data$Calibnum))) {
    
    # Sub-sample data from the current and next calibration number
    sub_samp <- parsed_data[parsed_data$Calibnum == i | parsed_data$Calibnum == i + 1,]
    
    # Extract data for standards from the sub-sampled data
    calibs <- sub_samp[grep("std", tolower(sub_samp$SampleID)),]
    
    # Create linear regression using standard current ratios and saturation/ratios
    lin_reg <- lm(calibs[[sat_col]] ~ calibs[[targ_col]])
    
    # Extract intercept and slope coefficients from linear regression
    coeff1 <- coef(summary(lin_reg))[1]   # Intercept
    coeff2 <- coef(summary(lin_reg))[2]   # Slope
    
    # Use linear regression coefficients to calculate in situ N2.Ar ratio
    calc_N2.Ar <- coeff1 + sub_samp[[targ_col]] * coeff2
    
    # Multiply in situ N2.Ar ratio by ArSat to calculate in situ N2 concentration
    sub_samp[[new_col]] <- sub_samp[["ArSat"]] * calc_N2.Ar
    
    # Append new data to existing parsed data frame
    parsed_data[[new_col]][!is.na(base::match(parsed_data$Samp, sub_samp$Samp))] <- sub_samp[[new_col]]
  }
  
  return(parsed_data)
}

################################################################################
## Calculate sample in situ O2 concentration using linear model of standards ##

get_O2_conc <- function(parsed_data) {
  
  # Define the target column (targ_col) and saturation column (sat_col)
  targ_col <- "O2.Ar"
  sat_col <- "O2.ArSat"
  
  # Create new column for calculated data
  new_col <- "O2.Calc"
  
  # Loop through each calibration set in the parsed data
  for (i in 1:length(unique(parsed_data$Calibnum))) {
    
    # Sub-sample data from the current and next calibration number
    sub_samp <- parsed_data[parsed_data$Calibnum == i | parsed_data$Calibnum == i + 1,]
    
    # Extract data for standards from the sub-sampled data
    calibs <- sub_samp[grep("std", tolower(sub_samp$SampleID)),]
    
    # Create linear regression using standard current ratios and saturation/ratios
    lin_reg <- lm(calibs[[sat_col]] ~ calibs[[targ_col]])
    
    # Extract intercept and slope coefficients from linear regression
    coeff1 <- coef(summary(lin_reg))[1]   # Intercept
    coeff2 <- coef(summary(lin_reg))[2]   # Slope
    
    # Use linear regression coefficients to calculate in situ O2.Ar ratio
    calc_O2.Ar <- coeff1 + sub_samp[[targ_col]] * coeff2
    
    # Multiply in situ O2.Ar ratio by ArSat to calculate in situ O2 concentration
    sub_samp[[new_col]] <- sub_samp[["ArSat"]] * calc_O2.Ar
    
    # Append new data to existing parsed data frame
    parsed_data[[new_col]][!is.na(base::match(parsed_data$Samp, sub_samp$Samp))] <- sub_samp[[new_col]]
  }
  
  return(parsed_data)
}

################################################################################
## Calculates sample del_15N using sample and standard X29.28 ratios ##

get_del_15N <- function(parsed_data) {
  
  # Define the target column (targ_col) and saturation column (sat_col)
  targ_col <- "X29.28"
  
  # Create new column for calculates data
  new_col <- "del_15N.Calc"
  
  # Loop through each calibration number
  for (i in 1:length(unique(parsed_data$Calibnum))) {
    
    # Sub-sample data from the current and next calibration number
    sub_samp <- parsed_data[parsed_data$Calibnum == i | parsed_data$Calibnum == i + 1,]
    
    # Collect data for standards from the sub-sampled data
    calibs <- sub_samp[grep("std", tolower(sub_samp$SampleID)),]
    
    # Average 34.32 ratios for standards
    X29.28_std <- mean(calibs[["X29.28"]])
    
    # Calculate del_18O value for samples
    sub_samp[[new_col]] <- ((sub_samp[[targ_col]] / X29.28_std) - 1) * 1000
    
    # Append new data to existing parsed data frame
    parsed_data[[new_col]][!is.na(base::match(parsed_data$Samp, sub_samp$Samp))] <- sub_samp[[new_col]]
  }
  
  return(parsed_data)
}

################################################################################
## Calculates sample del_18O using sample and standard X34.32 ratios ##

get_del_18O <- function(parsed_data) {
  
  # Define the target column (targ_col) and saturation column (sat_col)
  targ_col <- "X34.32"

  # Create new column for calculates data
  new_col <- "del_18O.Calc"

  # Loop through each calibration number
  for (i in 1:length(unique(parsed_data$Calibnum))) {

    # Sub-sample data from the current and next calibration number
    sub_samp <- parsed_data[parsed_data$Calibnum == i | parsed_data$Calibnum == i + 1,]

    # Collect data for standards from the sub-sampled data
    calibs <- sub_samp[grep("std", tolower(sub_samp$SampleID)),]

    # Average 34.32 ratios for standards
    X34.32_std <- mean(calibs[["X34.32"]])

    cat('X34.32_std:\n')
    print(X34.32_std)
    cat('\n')
    
    # Calculate del_18O value for samples
    sub_samp[[new_col]] <- ((sub_samp[[targ_col]] / X34.32_std) - 1) * 1000

    # Append new data to existing parsed data frame
    parsed_data[[new_col]][!is.na(base::match(parsed_data$Samp, sub_samp$Samp))] <- sub_samp[[new_col]]
  }
  
  return(parsed_data)
}

################################################################################
## Calculate data statistics for each variable in the parsed data ##
## Calculates sample mean, SD, SEM, and upper + lower 95% CIs     ##

get_data_stats <- function(parsed_data) {
  
  # Identify columns in parsed data containing sample metadata and sample measurements
  meta_cols <- c("SampleID", "Pressure", "Temp", "Calibnum", "Depth", "WatDens",
                 "O2Sat", "N2Sat", "ArSat", "O2.ArSat", "N2.ArSat")
  meas_cols <- setdiff(colnames(parsed_data), c("Samp", meta_cols))
  
  # Calculate data statistics for all variables and include sample metadata
  results <- parsed_data %>%
    group_by(Samp) %>%
    summarise(
      across(all_of(meta_cols), ~first(.)),  # Include metadata columns
      across(all_of(meas_cols), 
             list(num = ~sum(!is.na(.)),
                  mean = ~mean(., na.rm = TRUE),
                  sd = ~sd(., na.rm = TRUE),
                  sem = ~sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))),
                  ci_lower = ~mean(., na.rm = TRUE) - qt(0.975, df = sum(!is.na(.)) - 1) * (sd(., na.rm = TRUE) / sqrt(sum(!is.na(.)))),
                  ci_upper = ~mean(., na.rm = TRUE) + qt(0.975, df = sum(!is.na(.)) - 1) * (sd(., na.rm = TRUE) / sqrt(sum(!is.na(.))))
             ))
    )
  
  # Add statistics suffixes for new measurement columns
  meas_col_names <- colnames(results)[grepl("_num$|_mean$|_sd$|_sem$|_ci_lower$|_ci_upper$", colnames(results))]
  new_names <- paste0(rep(meas_cols, each = 6), 
                      c("_num", "_mean", "_sd", "_sem", "_95ci_lo", "_95ci_up"))
  colnames(results)[which(colnames(results) %in% meas_col_names)] <- new_names
  
  # Reorder columns: Samp, metadata columns, then measurement columns with their stats
  results <- results %>%
    select(Samp, all_of(meta_cols), everything())
  
  return(results)
}

################################################################################
## Main (run program functions) ##

# Trim raw data
raw_data_trimmed <- read_raw_file(raw_data_file_path)

# Parse raw data
raw_data_parsed <- gather_data(raw_data_trimmed, metadata_file_path, pressure_unit)

# Rename parsed data set
raw_data_processed <- raw_data_parsed

## Call data processing functions ##
# EDIT: Comment out what you don't need

# Get sample in situ N2 concentration
raw_data_processed <- get_N2_conc(raw_data_processed)

# Get sample in situ O2 concentration
raw_data_processed <- get_O2_conc(raw_data_processed)

# Get sample del_15N values
# raw_data_processed <- get_del_15N(raw_data_processed)

# Get sample del_18O values
raw_data_processed <- get_del_18O(raw_data_processed)

##

# Get sample data statistics
raw_data_stats <- get_data_stats(raw_data_processed)

# Save data as designated output file name
# write_csv(raw_data_stats, paste0(output_file_path, save_file_name))
