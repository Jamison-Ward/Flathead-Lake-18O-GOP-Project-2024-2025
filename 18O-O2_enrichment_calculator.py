# -*- coding: utf-8 -*-
"""
Created on Fri Aug 23 13:13:02 2024

@author: jamis
"""

import math

# Constants
R_VSMOW = 2.0052e-3  # 18O/16O isotope ratio of VSMOW

# Variables
GOP = 2.55  # Gross oxygen production [umol O / L * d]
Δt = 4  # Incubation length [hr]
O2_i = 309  # Initial oxygen concentration [umol O / L]
δ18O_H2O = 4114.343  # Oxygen isotope composition of water [per mil] (relative to VSMOW)
δ18O_O2_i = 17  # Initial oxygen isotope composition of dissolved O2 [per mil] (relative to VSMOW)

# Modules
EXP = math.e ** ((GOP/24) * Δt / O2_i)
# print(x1)
R_H2O = ((δ18O_H2O / 1000) + 1) * R_VSMOW
# print(x2)
R_O2_i = ((δ18O_O2_i / 1000) + 1) * R_VSMOW
# print(x3)

# Calculation
Δδ18O_O2 = 1000 * ((R_O2_i - R_H2O) / R_VSMOW) * ((1 - EXP) / EXP)

print(Δδ18O_O2)
