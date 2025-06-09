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
sec_anomaly <- function(data, anomaly, source_sector) {
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
    mutate(domestic = ifelse(country == dest_country, 1, 0)) |>
    mutate(foreign= ifelse(country != dest_country, 1, 0)) |>
    dplyr::select(-country, -industry) |>
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
        .x[2]
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
## Shocks ---------------------------------------------------
list_names <- c("agric_temp_tbl",
                "non_agric_temp_tbl",
                "agric_precip_tbl",
                "non_agric_precip_tbl")


args_list <- list(
  c("land_weighted_temp_anomaly", "Agrifood"),
  c("population_weighted_temp_anomaly", "Downstream"),
  c("land_weighted_precip_anomaly", "Agrifood"),
  c("population_weighted_precip_anomaly", "Downstream")
)

tic()
shocks_list <- multi_sec_anomaly(args_list, list_names)
toc()


# Visualization example ---------------------------------------------------
climate_shocks |>
  filter(country == "ZAF") |>
  ggplot(aes(x = date, y = land_weighted_temp_anomaly)) +
  geom_line() +
  theme_minimal()

shocks_list |>
  pluck("non_agric_temp_tbl") |>
  filter(dest_country == "ZAF") |>
  ggplot(aes(x = date, y = value, col = dest_industry)) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~ dest_industry, scales = "free_y")


# Export ---------------------------------------------------------------
artifacts_network_shocks <- list (
  shocks_list = shocks_list
)

write_rds(artifacts_network_shocks,
          file = here("Outputs", "artifacts_network_shocks.rds"))

