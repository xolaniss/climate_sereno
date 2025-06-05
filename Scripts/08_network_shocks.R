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
sec_anomaly <- function(data, anomaly, type, source) {
  if(type == "domestic"){
   tbl <-
     data |>
      filter(industry == source) |>
      left_join(climate_shocks |>
                  dplyr::select(date, country, {{anomaly}}),
                by = c("country", "date")) |>
      mutate(
        across(contains("."), ~ .x * {{anomaly}})
      ) |>
      # dplyr::select(- country, - industry,  - {{anomaly}}) |>
      # pivot_longer(
      #   cols = -c(date),
      #   names_to = "country.industry",
      #   values_to = "value"
      # ) |>
      # tidytable::separate(col = "country.industry",
      #                     into = c("country", "industry"),
      #                     sep = ".") |>
      # relocate(c("country", "industry"), .after = "date") |>
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
      as_tibble()
      gc()
      return(tbl)

  } else {
  tbl <-
    data |>
    filter(industry == source) |>
    left_join(climate_shocks |>
                  dplyr::select(date, country, {{anomaly}}),
                by = c("country", "date")) |>
    dplyr::select(-contains(".Agrifood")) |>
    mutate(
      across(contains("."), ~ .x * {{anomaly}})
    ) |>
    # dplyr::select(- country, - industry, - {{anomaly}}) |>
    # pivot_longer(
    #   cols = -c(date),
    #   names_to = "country.industry",
    #   values_to = "value"
    # ) |>
    # tidytable::separate(col = "country.industry",
    #                     into = c("country", "industry"),
    #                     sep = ".") |>
    # relocate(c("country", "industry"), .after = "date") |>
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
    as_tibble()
    gc()
    return(tbl)
  }
}


## Domestic shocks ---------------------------------------------------
WD_agric_temp_tbl <- sec_anomaly(input_output_tbl,
                     land_weighted_temp_anomaly,
                     "domestic",
                     "Agrifood"
                     ) |>
  head(n = 100) # just taking the first 100 rows for now


WD_non_agric_temp_tbl <- sec_anomaly(input_output_tbl,
                     population_weighted_temp_anomaly,
                     "domestic",
                     "Downstream"
                     ) |>
  head(n = 100) # just taking the first 100 rows for now


WD_agric_precip_tbl <- sec_anomaly(input_output_tbl,
                     land_weighted_precip_anomaly,
                     "domestic",
                     "Agrifood"
                     ) |>
  head(n = 100) # just taking the first 100 rows for now

WD_non_agric_precip_tbl <- sec_anomaly(input_output_tbl,
                     population_weighted_precip_anomaly,
                     "domestic",
                     "Downstream"
                     ) |>
  head(n = 100) # just taking the first 100 rows for now

## Foreign shocks ---------------------------------------------------
WD_foreign_agric_temp_tbl <- sec_anomaly(input_output_tbl,
                               land_weighted_temp_anomaly,
                               "foreign",
                               "Agrifood"
                               ) |>
  head(n = 100) # just taking the first 100 rows for now

WD_foreign_non_agric_temp_tbl <- sec_anomaly(input_output_tbl,
                               population_weighted_temp_anomaly,
                               "foreign",
                               "Downstream"
                               ) |>
  head(n = 100) # just taking the first 100 rows for now


WD_foreign_agric_precip_tbl <- sec_anomaly(input_output_tbl,
                               land_weighted_precip_anomaly,
                               "foreign",
                               "Agrifood"
                               ) |>
  head(n = 100) # just taking the first 100 rows for now


WD_foreign_non_agric_precip_tbl <- sec_anomaly(input_output_tbl,
                               population_weighted_precip_anomaly,
                               "foreign",
                               "Downstream"
                               ) |>
  head(n = 100) # just taking the first 100 rows for now

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
  WD_agric_temp_tbl = WD_agric_temp_tbl,
  WD_non_agric_temp_tbl = WD_non_agric_temp_tbl,
  WD_agric_precip_tbl = WD_agric_precip_tbl,
  WD_non_agric_precip_tbl = WD_non_agric_precip_tbl,
  WD_foreign_agric_temp_tbl = WD_foreign_agric_temp_tbl,
  WD_foreign_non_agric_temp_tbl = WD_foreign_non_agric_temp_tbl,
  WD_foreign_agric_precip_tbl = WD_foreign_agric_precip_tbl,
  WD_foreign_non_agric_precip_tbl = WD_foreign_non_agric_precip_tbl
)

write_rds(artifacts_network_shocks,
          file = here("Outputs", "artifacts_network_shocks.rds"))
