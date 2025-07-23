# Description
# combining climate and sectoral data - Xolani Sibande April 2025
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
      id_cols = c("date", "row_country", "row_industry", "col_country", "col_industry"),
      names_from = shock_type,
      values_from = value,
      values_fn = mean,
      values_fill = 0
    ) |>
    mutate(
      direct_shock = if_else(row_country == col_country & row_industry == col_industry, 1, 0),
      not_direct_shock = if_else(row_country != col_country | row_industry != col_industry, 1, 0)
    )
}

# Import and clean -------------------------------------------------------------
## Network shocks data ---------------------------------------------------------------
network_shocks <-
  read_rds(here("Outputs", "artifacts_network_shocks_average.rds"))

agric_temp_shocks_tbl <- network_shocks |> pluck(1,1)
non_agric_temp_shocks_tbl <- network_shocks |> pluck(1,2)

agric_precip_shocks_tbl <- network_shocks |> pluck(1,3)
non_agric_precip_shocks_tbl <- network_shocks |> pluck(1,4)

## Inflation data ---------------------------------------------------------------
inflation_rate_tbl <- read_rds(here("Outputs", "artifacts_inflation_rate.rds")) |>
  pluck(1)

inflation_rate_tbl |>
  filter(country == "ZAF" & industry == "Transport" & date >= "2016-01-01")

# Combining data ---------------------------------------------------------------
tic()
combined_temp_data_tbl <-
  agric_temp_shocks_tbl |>
  bind_rows(non_agric_temp_shocks_tbl, .id = "shock_type") |>
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
toc()

tic()
combined_precip_data_tbl <-
  agric_precip_shocks_tbl |>
  bind_rows(non_agric_precip_shocks_tbl, .id = "shock_type") |>
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
toc()

# Export ---------------------------------------------------------------
artifacts_combined_data <- list (
  combined_temp_data_tbl = combined_temp_data_tbl,
  combined_precip_data_tbl = combined_precip_data_tbl
)

write_rds(artifacts_combined_data, file = here("Outputs",
                                               "artifacts_combined_data.rds"))
