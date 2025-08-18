# Description
# Hemisphere seasons analysis - Xolani Sibande 25 August 2025
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
hemisphere_tbl <- read_rds(here("Outputs", "artifacts_controls_prep.rds")) |>
  pluck(1)

# Merging hemisphere data -----------------------------------
hemisphere_merge <- function(data){
  data |>
    map(~{
      .x |>
      left_join(hemisphere_tbl, by = c("country")) |>
      mutate(season= case_when(
        # Northern Hemisphere
        hemisphere == 1 & lubridate::month(date) %in% c(6, 7, 8) ~ "summer",
        hemisphere == 1 & lubridate::month(date) %in% c(12, 1, 2) ~ "winter",
        # Southern Hemisphere
        hemisphere == 2 & lubridate::month(date) %in% c(12, 1, 2) ~ "summer",
        hemisphere == 2 & lubridate::month(date) %in% c(6, 7, 8) ~ "winter"
      ))})
}

tic("Merging the data")
temp_hemisphere_list <-
  temp_list |>
  hemisphere_merge()
precip_hemisphere_list <-
  precip_list |>
  hemisphere_merge()
toc()

# Hemisphere temp regressions --------------------------------------------------------
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_temperature_shock +
    foreign_agricultural_temperature_shock +
    domestic_non_agricultural_temperature_shock +
    foreign_non_agricultural_temperature_shock +
    season +
    col_country +
    year")

tic("hemisphere temp regressions")
temp_hemisphere_reg_list <- temp_hemisphere_list |> map( ~ reg(.x, rows_keep = 7))
toc()


# Hemisphere precip regressions --------------------------------------------------------
formula <- as.formula(
  "inflation_rate ~
    lag(inflation_rate) +
    domestic_agricultural_precipitation_shock +
    foreign_agricultural_precipitation_shock +
    domestic_non_agricultural_precipitation_shock +
    foreign_non_agricultural_precipitation_shock +
    season +
    col_country +
    year")

tic("hemisphere precip regressions")
precip_hemisphere_reg_list <- precip_hemisphere_list |> map( ~ reg(.x, rows_keep = 7))
toc()


# Export ---------------------------------------------------------------
artifacts_hemisphere_regs <- list(
  temp_hemisphere_reg_list = temp_hemisphere_reg_list,
  precip_hemisphere_reg_list = precip_hemisphere_reg_list
)

write_rds(artifacts_hemisphere_regs, file = here("Outputs", "artifacts_hemisphere_regs.rds"))


