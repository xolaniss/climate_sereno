# This script runs the entire workflow for the analysis of climate shocks and their impact on inflation and production networks.
# Xolani Sibande - August 2025

# Preliminaries -----------------------------------------------------------
library(here)
library(tictoc)


# Source the necessary scripts to run the entire workflow ----
source(here("Scripts", "01_climate_data.R"))
source(here("Scripts", "02_climate_shocks.R"))
source(here("Scripts", "03_climate_shocks_stationarity.R"))
source(here("Scripts", "04_cpi_data.R"))
source(here("Scripts", "05_inflation_rates.R"))
source(here("Scripts", "06_inflation_stationarity.R"))
source(here("Scripts", "07_yearly_production_network.R"))
source(here("Scripts", "08_monthly_production_network.R"))
gc() # clean memory
source(here("Scripts", "09_network_shocks.R"))
gc() # clean memory
source(here("Scripts", "10_combined_baseline_data.R"))
gc() # clean memory
source(here("Scripts", "11_baseline_regressions.R"))
source(here("Scripts", "12_inflation_seasonality_regressions.R"))
source(here("Scripts", "13_standardised_shocks_regressions.R"))
source(here("Scripts", "14_controls_prep.R"))
source(here("Scripts", "15_seasons_regs.R"))
source(here("Scripts", "16_income_classification_regs.R"))
source(here("Scripts", "17_inflation_targeting_regs.R"))
source(here("Scripts", "18_baseline_regs_plots.R"))
source(here("Scripts", "19_inflation_seasonality_regs_plots.R"))
source(here("Scripts", "20_standardised_shocks_regs_plots.R"))
source(here("Scripts", "21_seasonality_regs_plots.R"))
source(here("Scripts", "22_income_classification_regs_plots.R"))
source(here("Scripts", "23_inflation_targeting_regs_plots.R"))
