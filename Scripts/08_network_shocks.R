# Description
# Calculating network shocks - Xolani Sibande May 2025
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

options(scipen = 999)
mem.maxVSize(100000)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
sec_anomaly <- function(data, anomaly, source_sector,  type) {
  tbl <-
    data |>
    filter(industry == source_sector) |>
    left_join(climate_shocks |>
                dplyr::select(date, country, {{anomaly}}),
              by = c("country", "date")) |>
    mutate(
      across(contains("."), ~ .x * {{anomaly}})) |>
    dplyr::select(- {{anomaly}}) |>
    pivot_longer(
      cols = -c(date, country, industry ),
      names_to = "country.industry",
      values_to = "value"
    ) |>
    tidytable::separate(col = "country.industry",
                        into = c("dest_country", "dest_industry"),
                        sep = ".") |>
    relocate(c("dest_country", "dest_industry"), .before = "value") |>
    mutate(sort_dummy = ifelse(
      country == dest_country, "domestic", "foreign")) |>
    dplyr::filter(sort_dummy == type) |>
    dplyr::select(-sort_dummy) |>
    as_tibble()
  gc()
  return(tbl)
}
multi_sec_anomaly <- function(args, list_names) {
  args |>
    set_names(list_names) |>
    map(
      ~ sec_anomaly(
        input_output_tbl,
        !!sym(.x[1]),
        .x[2],
        .x[3]
      )
    )
}

# Import -------------------------------------------------------------
input_output_tbl <- read_rds(here("Outputs",
                                  "artifacts_monthly_production_network.rds")) |>
  pluck(2) |>
  tidytable::separate("row_industry",
                      into = c("country", "industry"), sep = ".") |>
  as_tibble() |>
  relocate(c("country", "industry"), .after = "date")

climate_shocks <- read_rds(here("Outputs", "artifacts_climate_shocks.rds")) |>
  pluck(1)


# Calculating network shocks ---------------------------------------------------
## Domestic shocks ---------------------------------------------------
list_names <- c("agric_temp",
                "non_agric_temp",
                "agric_precip",
                "non_agric_precip")


domestic_args_list <- list(
  c("land_weighted_temp_anomaly", "Agrifood", "domestic"),
  c("population_weighted_temp_anomaly", "Downstream", "domestic"),
  c("land_weighted_precip_anomaly", "Agrifood", "domestic"),
  c("population_weighted_precip_anomaly", "Downstream", "domestic")
)

tic()
domestic_shocks_list <- multi_sec_anomaly(domestic_args_list, list_names)
toc()

## Foreign shocks ---------------------------------------------------
foreign_args_list <- list(
  c("land_weighted_temp_anomaly", "Agrifood", "foreign"),
  c("population_weighted_temp_anomaly", "Downstream", "foreign"),
  c("land_weighted_precip_anomaly", "Agrifood", "foreign"),
  c("population_weighted_precip_anomaly", "Downstream", "foreign")
)

tic()
foreign_shocks_list <- multi_sec_anomaly(foreign_args_list, list_names)
toc()

# Visualization example ---------------------------------------------------
climate_shocks |>
  filter(country == "USA") |>
  ggplot(aes(x = date, y = land_weighted_temp_anomaly)) +
  geom_line() +
  theme_minimal()

WD_non_agric_temp_tbl |>
  filter(dest_country == "ZAF") |>
  ggplot(aes(x = date, y = value, col = industry)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~ industry, scales = "free_y")


# Export ---------------------------------------------------------------
artifacts_network_shocks <- list (
  domestic_shocks_list = domestic_shocks_list,
  foreign_shocks_list = foreign_shocks_list
)

write_rds(artifacts_network_shocks,
          file = here("Outputs", "artifacts_network_shocks.rds"))

