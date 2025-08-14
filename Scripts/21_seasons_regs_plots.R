# Description
# Hemisphere seasons regressions plots - Xolani Sibande August 2025
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
hemisphere_reg_tbl <-
  read_rds(here("Outputs", "artifacts_hemisphere_regs.rds")) |>
  list_cleanup()

# Args list ------------------
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
  "inflation_precip_10_90" = c(
    "lag(inflation_rate)",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "inflation_precip_5_95" = c(
    "lag(inflation_rate)",
    "precip_baseline_5",
    "precip_baseline_95"
  ),

  # Domestic agricultural temperature and precipitation shocks with seasons
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

  # Domestic non agricultural temperature and precipitation shocks with seasons
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

  # Foreign agricultural temperature and precipitation shocks with seasons
  "foreign_agric_temp_10_90" = c(
    "foreign_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "foreign_agric_precip_10_90" = c(
    "foreign_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "foreign_agric_temp_5_95" = c(
    "foreign_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "foreign_agric_precip_5_95" = c(
    "foreign_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),

  # Foreign non agricultural temperature and precipitation shocks with seasons
  "foreign_non_agric_temp_10_90" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "foreign_non_agric_precip_10_90" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "foreign_non_agric_temp_5_95" = c(
    "foreign_non_agricultural_temperature_shock",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "foreign_non_agric_precip_5_95" = c(
    "foreign_non_agricultural_precipitation_shock",
    "precip_baseline_5",
    "precip_baseline_95"
  ),

  # Seasons
  "season_winter_temp_10_90" = c(
    "seasonwinter",
    "temp_baseline_10",
    "temp_baseline_90"
  ),
  "season_winter_temp_5_95" = c(
    "seasonwinter",
    "temp_baseline_5",
    "temp_baseline_95"
  ),
  "season_winter_precip_10_90" = c(
    "seasonwinter",
    "precip_baseline_10",
    "precip_baseline_90"
  ),
  "season_winter_precip_5_95" = c(
    "seasonwinter",
    "precip_baseline_5",
    "precip_baseline_95"
  )
)

# Plots ---------------------------------------------------------------
hemisphere_reg_plot_list <-
  my_model_plots(
  hemisphere_reg_tbl,
  args_list = args_list,
  model = "Hemisphere seasons"
)

# Export ---------------------------------------------------------------
artifacts_hermisphere_reg_plots <- list (
  hemisphere_reg_plot_list = hemisphere_reg_plot_list
)

write_rds(artifacts_hermisphere_reg_plots, file = here("Outputs", "artifacts_hermisphere_reg_plots.rds"))


