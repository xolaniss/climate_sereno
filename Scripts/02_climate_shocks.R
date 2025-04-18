# Description
# Calculating climate shocks - Xolani Sibande April 2025
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
    group_by(country) |>
    mutate("{{variable}}_extreme_p_{extreme_level}" :=
             ifelse({{variable}} >= quantile({{variable}}, extreme_level), "Yes", "No")) |>
    mutate("{{variable}}_extreme_p_{1 - extreme_level}" :=
             ifelse({{variable}} <= quantile({{variable}}, 1 - extreme_level), "Yes", "No")) |>
    ungroup()
}
country_extreme_gg_function <- function(data, variable = "temp", extreme_variable) {
  data |>
    group_by(country) |>
    filter({{extreme_variable}} == "Yes") |>
    fx_plot(
      col = "country",
      value = variable,
      facet_var = "country"
    )
}
# Import -------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(5)

# Climate data EDA ---------------------------------------------------------------
climate_data_tbl |> skim()

# removing missing data
climate_data_clean_tbl <-
  climate_data_tbl |>
  filter(!is.na(land_weighted_temp)) |>
  filter(!is.na(population_weighted_temp)) |>
  filter(!is.na(land_weighted_precip)) |>
  filter(!is.na(population_weighted_precip))

# Climate shocks ---------------------------------------------------------------
climate_shocks_tbl <- climate_data_clean_tbl  |>
  country_extreme_function(land_weighted_temp, 0.90) |>
  country_extreme_function(population_weighted_temp, 0.90) |>
  country_extreme_function(land_weighted_precip, 0.90) |>
  country_extreme_function(population_weighted_precip, 0.90) |>
  country_extreme_function(land_weighted_temp, 0.95) |>
  country_extreme_function(population_weighted_temp, 0.95) |>
  country_extreme_function(land_weighted_precip, 0.95) |>
  country_extreme_function(population_weighted_precip, 0.95) |>
  country_extreme_function(land_weighted_temp, 0.99) |>
  country_extreme_function(population_weighted_temp, 0.99) |>
  country_extreme_function(land_weighted_precip, 0.99) |>
  country_extreme_function(population_weighted_precip, 0.99)

glimpse(climate_shocks_tbl)

# Graphing ---------------------------------------------------------------

# plot land weighted temperature shocks for all countries
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "land_weighted_temp",
                              extreme_variable = land_weighted_temp_extreme_p_0.99)

climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "land_weighted_temp",
                              extreme_variable = land_weighted_temp_extreme_p_0.01)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "population_weighted_temp",
                              extreme_variable = population_weighted_temp_extreme_p_0.95)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "population_weighted_temp",
                              extreme_variable = population_weighted_temp_extreme_p_0.05)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "land_weighted_precip",
                              extreme_variable = land_weighted_precip_extreme_p_0.95)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "land_weighted_precip",
                              extreme_variable = land_weighted_precip_extreme_p_0.05)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "population_weighted_precip",
                              extreme_variable = population_weighted_precip_extreme_p_0.95)
climate_shocks_tbl |>
  filter(country %in% c("CAN", "ZAF", "USA", "ABW")) |>
  country_extreme_gg_function(variable = "population_weighted_precip",
                              extreme_variable = population_weighted_precip_extreme_p_0.05)

# Export ---------------------------------------------------------------
artifacts_climate_shocks <- list (
  climate_shocks_tbl = climate_shocks_tbl,
  climate_data_clean_tbl = climate_data_clean_tbl
)

write_rds(artifacts_climate_shocks, file = here("Outputs", "artifacts_climate_shocks.rds"))
