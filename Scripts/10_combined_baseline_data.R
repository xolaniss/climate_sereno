# Description
# combining climate and sectoral data (extreme) - Xolani Sibande July 2025
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

mem.maxVSize(100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
combined_weight_focused <- function(data){
  data |>
    relocate(
      year, .before = date,
    ) |>
    relocate(
      shock_type, .after = col_industry
    ) |>
    mutate(
      shock_source = ifelse(domestic== 1, "Domestic", "Foreign")
    ) |>
    tidytable::unite(
      "shock_type",
      c("shock_source", "shock_type"),
      sep = " "
    ) |>
    tidytable::select(-c("domestic", "foreign")) |>
    drop_na(value) |>
    tidytable::pivot_wider(
      id_cols = c("date", "country", "row_industry", "col_country", "col_industry"),
      names_from = shock_type,
      values_from = value,
      values_fn = mean,
      values_fill = 0
    ) |>
    mutate(
      direct_shock = if_else(country == col_country & row_industry == col_industry, 1, 0),
      not_direct_shock = if_else(country != col_country | row_industry != col_industry, 1, 0)
    )
}
extreme_combine_temp <- function(agric_data, non_agric_data){
  agric_data |>
    bind_rows(non_agric_data, .id = "shock_type") |>
    mutate(
      shock_type = case_when(
        shock_type == 1 ~ "Agricultural Temperature Shock",
        shock_type == 2 ~ "Non-Agricultural Temperature Shock"
      )
    ) |>
    relocate(
      date,
      .before = shock_type
    ) |>
    mutate(year = as.character(year(date))) |>
    combined_weight_focused() |>  # changing to weights focus
    left_join(inflation_rate_tbl,
              by = join_by(date,
                           col_country == country,
                           col_industry == industry),
              relationship = "many-to-many")

}
extreme_combine_precip <- function(agric_data, non_agric_data){
  agric_data |>
    bind_rows(non_agric_data, .id = "shock_type") |>
    mutate(
      shock_type = case_when(
        shock_type == 1 ~ "Agricultural Precipitation Shock",
        shock_type == 2 ~ "Non-Agricultural Precipitation Shock"
      )
    ) |>
    relocate(
      date,
      .before = shock_type
    ) |>
    mutate(year = as.character(year(date))) |>
    combined_weight_focused() |>  # changing to weights focus
    left_join(inflation_rate_tbl,
              by = join_by(date,
                           col_country == country,
                           col_industry == industry),
              relationship = "many-to-many")

}

# Import  -------------------------------------------------------------
## Network shocks data ----
network_shocks <-
  qd_read(here("Outputs", "artifacts_network_shocks_extremes.qs2"))

## Inflation data ---------------------------------------------------------------
inflation_rate_tbl <- read_rds(here("Outputs", "artifacts_inflation_rate.rds")) |>
  pluck(1)

# Extracting shocks ---------------------------------------------------------------
## Temp at 5% level ----
agric_temp_shocks_baseline_5_tbl <- network_shocks |> pluck(1,1)
non_agric_temp_shocks_baseline_5_tbl <- network_shocks |> pluck(1,2)

## Temp at 95% level ----
agric_temp_shocks_baseline_95_tbl <- network_shocks |> pluck(2,1)
non_agric_temp_shocks_baseline_95_tbl <- network_shocks |> pluck(2,2)

## Temp at 10% level ----
agric_temp_shocks_baseline_10_tbl <- network_shocks |> pluck(3,1)
non_agric_temp_shocks_baseline_10_tbl <- network_shocks |> pluck(3,2)

## Temp at 90% level ----
agric_temp_shocks_baseline_90_tbl <- network_shocks |> pluck(4,1)
non_agric_temp_shocks_baseline_90_tbl <- network_shocks |> pluck(4,2)

## Precipitation at 5% level ----
agric_precip_shocks_baseline_5_tbl <- network_shocks |> pluck(1,3)
non_agric_precip_shocks_baseline_5_tbl <- network_shocks |> pluck(1,4)

## Precipitation at 95% level ----
agric_precip_shocks_baseline_95_tbl <- network_shocks |> pluck(2,3)
non_agric_precip_shocks_baseline_95_tbl <- network_shocks |> pluck(2,4)

## Precipitation at 10% level ----
agric_precip_shocks_baseline_10_tbl <- network_shocks |> pluck(3,3)
non_agric_precip_shocks_baseline_10_tbl <- network_shocks |> pluck(3,4)

## Precipitation at 90% level ----
agric_precip_shocks_baseline_90_tbl <- network_shocks |> pluck(4,3)
non_agric_precip_shocks_baseline_90_tbl <- network_shocks |> pluck(4,4)


# Combining temp data ---------------------------------------------------------------
## temp 5% level ---
tic("Combining temperature shocks at 5% level")
combined_temp_baseline_5_data_tbl <-
  agric_temp_shocks_baseline_5_tbl |>
  extreme_combine_temp(non_agric_temp_shocks_baseline_5_tbl)
toc()

## temp 95% level ----
tic("Combining temperature shocks at 95% level")
combined_temp_baseline_95_data_tbl <-
  agric_temp_shocks_baseline_95_tbl |>
  extreme_combine_temp(non_agric_temp_shocks_baseline_95_tbl)
toc()

## temp 10% level ----
tic("Combining temperature shocks at 10% level")
combined_temp_baseline_10_data_tbl <-
  agric_temp_shocks_baseline_10_tbl |>
  extreme_combine_temp(non_agric_temp_shocks_baseline_10_tbl)
toc()

## temp 90% level ----
tic("Combining temperature shocks at 90% level")
combined_temp_baseline_90_data_tbl <-
  agric_temp_shocks_baseline_90_tbl |>
  extreme_combine_temp(non_agric_temp_shocks_baseline_90_tbl)
toc()

# Combining precipitation data ---------------------------------------------------------------
## precip 5% level ----
tic("Combining precipitation shocks at 5% level")
combined_precip_baseline_5_data_tbl <-
  extreme_combine_precip(agric_precip_shocks_baseline_5_tbl,
                 non_agric_precip_shocks_baseline_5_tbl)
toc()

## precip 10% level ----
tic("Combining precipitation shocks at 10% level")
combined_precip_baseline_10_data_tbl <-
  extreme_combine_precip(agric_precip_shocks_baseline_10_tbl,
                 non_agric_precip_shocks_baseline_10_tbl)
toc()

## precip 90% level ----
tic("Combining precipitation shocks at 90% level")
combined_precip_baseline_90_data_tbl <-
  extreme_combine_precip(agric_precip_shocks_baseline_90_tbl,
                 non_agric_precip_shocks_baseline_90_tbl)
toc()

## precip 95% level ----
tic("Combining precipitation shocks at 95% level")
combined_precip_baseline_95_data_tbl <-
  extreme_combine_precip(agric_precip_shocks_baseline_95_tbl,
                 non_agric_precip_shocks_baseline_95_tbl)
toc()

# Export ---------------------------------------------------------------
artifacts_combined_data <- list (
  combined_temp_baseline_5_tbl = combined_temp_baseline_5_data_tbl,
  combined_temp_baseline_10_tbl = combined_temp_baseline_10_data_tbl,
  combined_temp_baseline_90_tbl = combined_temp_baseline_90_data_tbl,
  combined_temp_baseline_95_tbl = combined_temp_baseline_95_data_tbl,
  combined_precip_baseline_5_tbl = combined_precip_baseline_5_data_tbl,
  combined_precip_baseline_10_tbl = combined_precip_baseline_10_data_tbl,
  combined_precip_baseline_90_tbl = combined_precip_baseline_90_data_tbl,
  combined_precip_baseline_95_tbl = combined_precip_baseline_95_data_tbl

)

qd_save(artifacts_combined_data, file = here("Outputs",
                                               "artifacts_combined_baseline_data.qs2"))
