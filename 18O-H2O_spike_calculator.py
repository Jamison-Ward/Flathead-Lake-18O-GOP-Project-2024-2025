# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 14:32:59 2024

@author: jamis
"""

# Known constants
R_VSMOW = 2.0052e-3  # 18O/16O isotope ratio of VSMOW
F18O_VSMOW = (((R_VSMOW) ** -1) + 1) ** -1  # Fractional abundance of 18O in VSMOW

def get_data():  # Get user input data
    V_spl = float(input("\nEnter the desired sample volume (ml): "))  # Sample volume (ml)
    δ18O_spl = float(input("Enter the desired sample δ18O of water (‰): "))  # Desired sample 18O-H2O isotope enrichment (VSMOW)
    F18O_spk = float(input("Enter the purity of the δ18O spike (%): "))  # Fractional abundance of 18O in spike
    return V_spl, δ18O_spl, F18O_spk


def calculate(V_spl, δ18O_spl, F18O_spk):  # Isotope calculations
    F18O_spk_dec = F18O_spk / 100  # Convert user purity to decimal
    R_spl = R_VSMOW * (1 + (δ18O_spl / 1000))  # 18O/16O isotope ratio of sample

    SF_num = R_spl * (1 - F18O_VSMOW) - F18O_VSMOW
    SF_den = F18O_spk_dec - R_spl * (1 - F18O_spk_dec)
    SF = SF_num / SF_den  # Numerical scaling factor

    V_spk = V_spl * (SF / (1 + SF))  # Volume of spike to add (ml)
    V_spk *= 1000  # Convert value to microliters
    
    return V_spk

def output(V_spl, δ18O_spl, F18O_spk, V_spk):  # Print output
    print("\nTo prepare a {} ml water sample at {}‰ δ18O(VSMOW), spike with {:.4}"
          " µl of {}% purity 18O-H2O".format(V_spl, δ18O_spl, V_spk, F18O_spk))

def main():
    V_spl, δ18O_spl, F18O_spk = get_data()
    V_spk = calculate(V_spl, δ18O_spl, F18O_spk)
    output(V_spl, δ18O_spl, F18O_spk, V_spk)
    
if __name__ == '__main__':
    main()