# Description
# income classification analysis - Xolani Sibande August 2025
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
income_classification_tbl <-
  read_rds(here("Outputs", "artifacts_controls_prep.rds")) |>
  pluck(2) |>
  rename(income_country = country) |>
  dplyr::select(income_country, income_classification)

# Merging income classification data -----------------------------------
income_classification_merge <- function(data){
  data |>
    map(~{
      .x |>
        left_join(income_classification_tbl,
                  by = join_by(col_country == income_country))
    })
}

temp_income_classification_list <- income_classification_merge(temp_list)
precip_income_classification_list <- income_classification_merge(precip_list)

# Running income regressions -----------------------------------
# Temp regressions ----
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_temperature_shock +
    foreign_agricultural_temperature_shock +
    domestic_non_agricultural_temperature_shock +
    foreign_non_agricultural_temperature_shock +
    income_classification +
    col_country +
    year")

tic("Temperature Income Classification Regressions")
temp_income_reg_list <-
  temp_income_classification_list |>
  map(~ reg(.x))
toc()

# Precip regressions ----
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_precipitation_shock +
    foreign_agricultural_precipitation_shock +
    domestic_non_agricultural_precipitation_shock +
    foreign_non_agricultural_precipitation_shock +
    income_classification +
    col_country +
    year")

tic("Precipitation Income Classification Regressions")
precip_income_reg_list <-
  precip_income_classification_list |>
  map(~ reg(.x))
toc()

# Export ---------------------------------------------------------------
artifacts_income_regs <- list (
  temp_income_reg_list = temp_income_reg_list,
  precip_income_reg_list = precip_income_reg_list
)

write_rds(artifacts_income_regs, file = here("Outputs", "artifacts_income_regs.rds"))


