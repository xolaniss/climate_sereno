# Description
# Calculating climate shocks stationarity - Xolani Sibande July 2025

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(dtplyr)
library(arrow)
library(data.table)
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
country_extreme_function <- function(data, variable, extreme_level) {
  data |>
    group_by(country, year) |>
    mutate("{{variable}}_extreme_p_{extreme_level}" :=
             ifelse({{variable}} >= quantile({{variable}}, extreme_level), "Yes", "No")) |>
    ungroup()
}

clean_data <- function(data, variable, extreme_level, variable_string = "population_weighted_temp") {
  data |>
    country_extreme_function(variable = {{variable}}, extreme_level = extreme_level) |>
    dplyr::select(date, country, {{variable}}, ends_with(glue("{extreme_level}"))) |>
    dplyr::filter(!!sym(glue("{variable_string}_extreme_p_{extreme_level}")) == "Yes") |>
    dplyr::select(-ends_with(glue("{extreme_level}")))
}

stationarity_test <- function(data, variable) {
  plm::purtest(
    as.formula(glue("{variable} ~ 1")),
    data = data,
    index = c("country", "date"),
    test = "ips",
    pmax = 0,
    exo = "trend"
  )
}

# Import -------------------------------------------------------------
climate_clean_tbl <- read_rds(here("Outputs", "artifacts_climate_shocks.rds")) |>
  pluck(2) |>
  mutate(year = as.character(year(date)))

glimpse(climate_clean_tbl)

# Testing for stationarity --------------------------------------------

extremes_list <- list(
  c("population_weighted_temp", "0.01"),
  c("population_weighted_temp", "0.99"),
  c("population_weighted_temp", "0.05"),
  c("population_weighted_temp", "0.95"),
  c("population_weighted_temp", "0.1"),
  c("population_weighted_temp", "0.9"),
  c("population_weighted_precip", "0.01"),
  c("population_weighted_precip", "0.99"),
  c("population_weighted_precip", "0.05"),
  c("population_weighted_precip", "0.95"),
  c("population_weighted_precip", "0.1"),
  c("population_weighted_precip", "0.9")
)

variable_names <- c(
  "population_weighted_temp_0.01",
  "population_weighted_temp_0.99",
  "population_weighted_temp_0.05",
  "population_weighted_temp_0.95",
  "population_weighted_temp_0.1",
  "population_weighted_temp_0.9",
  "population_weighted_precip_0.01",
  "population_weighted_precip_0.99",
  "population_weighted_precip_0.05",
  "population_weighted_precip_0.95",
  "population_weighted_precip_0.1",
  "population_weighted_precip_0.9"
)

stationarity_list <-
  extremes_list |>
  set_names(variable_names) |>
  map(
    ~ clean_data(
      data = climate_clean_tbl,
      variable = !!sym(.x[1]),
      extreme_level = as.numeric(.x[2]),
      variable_string = .x[1]
    ) |>
      stationarity_test(variable = .x[1])
  )



# Export ---------------------------------------------------------------
artifacts_shocks_stationarity <- list (
  stationarity_list = stationarity_list
)

write_rds(artifacts_shocks_stationarity, file = here("Outputs", "artifacts_shocks_stationarity.rds"))
