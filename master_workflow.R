# This script runs the entire workflow for the analysis of climate shocks and their impact on inflation and production networks.
# Xolani Sibande - August 2025

# Preliminaries -----------------------------------------------------------
library(here)


# Source the necessary scripts to run the entire workflow ----
source(here("Scripts", "01_climate_data.R"))
source(here("Scripts", "02_climate_shocks.R"))
source(here("Scripts", "03_climate_shocks_stationarity.R"))
source(here("Scripts", "04_cpi_data.R"))
source(here("Scripts", "05_inflation_rates.R"))
source(here("Scripts", "06_inflation_stationarity.R"))
source(here("Scripts", "07_yearly_production_network.R"))
rm(list = ls()) # clean environment
source(here("Scripts", "08_monthly_production_network.R"))
rm(list = ls()) # clean environment
source(here("Scripts", "09_network_shocks.R"))
rm(list = ls()) # clean environment
source(here("Scripts", "10_combined_baseline_data.R"))
rm(list = ls()) # clean environment
source(here("Scripts", "11_baseline_regressions.R"))
