# Description
# Inflation seasonality plots - Xolani Sibande August 2025
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
inflation_seasonality_regs_tbl <- read_rds(here("Outputs", "artifacts_inflation_seasonality_regs.rds")) |>
  list_cleanup()

unique(inflation_seasonality_regs_tbl$sector)

# Argument list -----------------------------
args_list <- list(
  #Inflation
  "inflation_temp_10_90_lag1" = c(
    "lag(inflation_rate)",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "inflation_temp_10_90_lag2" = c(
    "lag(inflation_rate, 2)",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "inflation_temp_10_90_lag3" = c(
    "lag(inflation_rate, 3)",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "inflation_temp_5_95_lag1" = c(
    "lag(inflation_rate)",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "inflation_temp_5_95_lag2" = c(
    "lag(inflation_rate, 2)",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "inflation_temp_5_95_lag3" = c(
    "lag(inflation_rate, 3)",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "inflation_precip_10_90_lag1" = c(
    "lag(inflation_rate)",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "inflation_precip_10_90_lag2" = c(
    "lag(inflation_rate, 2)",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "inflation_precip_10_90_lag3" = c(
    "lag(inflation_rate, 3)",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "inflation_precip_5_95_lag1" = c(
    "lag(inflation_rate)",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  "inflation_precip_5_95_lag2" = c(
    "lag(inflation_rate, 2)",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  "inflation_precip_5_95_lag3" = c(
    "lag(inflation_rate, 3)",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  # Domestic agricultural temperature and precipitation shocks
  "dom_agric_temp_10_90" = c(
    "domestic_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "dom_agric_temp_5_95" = c(
    "domestic_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "dom_agric_precip_10_90" = c(
    "domestic_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "dom_agric_precip_5_95" = c(
    "domestic_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),
  # Domestic non agricultural temperature and precipitation shocks
  "dom_non_agri_temp_10_90" = c(
    "domestic_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "dom_non_agri_temp_5_95" = c(
    "domestic_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "dom_non_agri_precip_10_90" = c(
    "domestic_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "dom_non_agri_precip_5_95" = c(
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
  "foreign_non_agri_temp_10_90" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "foreign_non_agri_temp_5_95" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "foreign_non_agri_precip_10_90" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "foreign_non_agri_precip_5_95" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  )
)

# Multiple model plots -----------------------------
inflation_seasonality_plots_list <-
  my_model_plots(
  args_list = args_list,
  data = inflation_seasonality_regs_tbl,
  model_type = "Inflation seasonality")

# Export ---------------------------------------------------------------
artifacts_inflation_seasonality_plot <- list (
  inflation_seasonality_plots_list = inflation_seasonality_plots_list
)

write_rds(artifacts_inflation_seasonality_plot, file = here("Outputs", "artifacts_inflation_seasonality_plot.rds"))


