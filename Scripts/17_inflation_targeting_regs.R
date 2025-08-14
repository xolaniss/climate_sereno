# Description
# Inflation targeting analysis - Xolani Sibande August 2025
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
source(here("Functions", "reg.R"))

# Import -------------------------------------------------------------
combined_data <- qd_read(here("Outputs", "artifacts_baseline_reg_data.qs2"))
temp_list <- combined_data |> pluck(1)
precip_list <- combined_data |> pluck(2)
industry_names <- combined_data |> pluck(3)
inflation_targeting_tbl <-
  read_rds(here("Outputs", "artifacts_controls_prep.rds")) |>
  pluck(3) |>
  rename(inflation_targeting_country = country) |>
  dplyr::select(inflation_targeting_country, inflation_targeting)

# Merging inflation targeting data -----------------------------------
inflation_targeting_merge <- function(data){
  data |>
  map( ~ {
    .x |>
      left_join(inflation_targeting_tbl,
                by = join_by(col_country == inflation_targeting_country))
  })
}

temp_inflation_list <- inflation_targeting_merge(temp_list)
precip_inflation_list <- inflation_targeting_merge(precip_list)

# Inflation targeting regressions ---------------------------------------------------------------
## Temp regressions ----
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_temperature_shock +
    foreign_agricultural_temperature_shock +
    domestic_non_agricultural_temperature_shock +
    foreign_non_agricultural_temperature_shock +
    inflation_targeting +
    col_country +
    year")

tic("Temperature Inflation Targeting Regressions")
temp_inflation_regs_list <-
  temp_inflation_list |>
  map(~ reg(.x, rows_keep = 8))
toc()

## Precip regressions ----
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_precipitation_shock +
    foreign_agricultural_precipitation_shock +
    domestic_non_agricultural_precipitation_shock +
    foreign_non_agricultural_precipitation_shock +
    inflation_targeting +
    col_country +
    year")

tic("Precipitation Inflation Targeting Regressions")
precip_inflation_regs_list <-
  precip_inflation_list |>
  map(~ reg(.x, rows_keep = 8))
toc()

# Export ---------------------------------------------------------------
artifacts_inflation_targeting_regs <- list (
  temp_inflation_regs_list = temp_inflation_regs_list,
  precip_inflation_regs_list = precip_inflation_regs_list
)

write_rds(artifacts_inflation_targeting_regs,
          file = here("Outputs", "artifacts_inflation_targeting_regs.rds"))


