# Description
#  Standardised regression plots - Xolani Sibande August 2025
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)
library(qs2)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "list_cleanup.R"))
source(here("Functions", "my_model_plots.R"))
source(here("Functions", "my_model_plot.R"))

# Import -------------------------------------------------------------
standardised_regs_tbl <- read_rds(here("Outputs", "artifacts_standardised_shocks_regs.rds")) |>
  list_cleanup()

# Argument list -----------------------------
args_list <- list(
  # Inflation
  "inflation_temp_10_90" = c(
    "lag(inflation_rate)",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "inflation_temp_5_95" = c(
    "lag(inflation_rate)",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  #  Domestic agricultural temperature and precipitation shocks
  "dom_agric_temp_10_90" = c(
    "domestic_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "dom_agric_precip_10_90" = c(
    "domestic_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "dom_agric_temp_5_95" = c(
    "domestic_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "dom_agric_precip_5_95" = c(
    "domestic_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  # Domestic non agricultural temperature and precipitation shocks
  "dom_non_agric_temp_10_90" = c(
    "domestic_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "dom_non_agric_precip_10_90" = c(
    "domestic_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "dom_non_agric_temp_5_95" = c(
    "domestic_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "dom_non_agric_precip_5_95" = c(
    "domestic_non_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  # Foreign agricultural temperature and precipitation shocks
  "foreign_agric_temp_10_90" = c(
    "foreign_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "foreign_agric_temp_5_95" = c(
    "foreign_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "foreign_agric_precip_10_90" = c(
    "foreign_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "foreign_agric_precip_5_95" = c(
    "foreign_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  # Foreign non agricultural temperature and precipitation shocks
  "foreign_non_agric_temp_10_90" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "foreign_non_agric_temp_5_95" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "foreign_non_agric_precip_10_90" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "foreign_non_agric_precip_5_95" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  )
)


# Multiple regressions plots ---------------------------
standardised_regs_plots_list <-
  my_model_plots(
  args_list = args_list,
  data = standardised_regs_tbl,
  model = "Standardised"
)

# Export ---------------------------------------------------------------
artifacts_standardised_regs_plots <- list (
  standardised_regs_plots_list = standardised_regs_plots_list
)

write_rds(artifacts_standardised_regs_plots, file = here("Outputs", "artifacts_standardised_regs_plots.rds"))


