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

# Import and clean -------------------------------------------------------------
## Eora26 data ---------------------------------------------------------------
eora_ma_tbl <- read_rds(here("Outputs", "artifacts_combined_eoro26.rds")) |>
  pluck(1)

## CPI data ---------------------------------------------------------------
cpi_tbl <- read_rds(here("Outputs", "artifacts_cpi.rds")) |>
  pluck(1) |>
  filter(date > "1990-01-01") |>
  dplyr::select(-country) |>
  rename(country = country_abs) |>
  mutate(category = str_to_lower(category)) |>
  mutate(category = str_replace_all(category, "hh goods", "household_goods")) |>
  mutate(category = str_replace_all(category, "food & bev", "agrifood")) |>
  filter(category != "other") |>
  filter(category != "alcbev") |>
  filter(category != "recreation")

cpi_industry_tbl <-
  cpi_tbl |>
  filter(category != "headline") |>
  rename(cpi_industry = cpi)

cpi_headline_tbl <-
  cpi_tbl |>
  filter(category == "headline") |>
  rename(cpi_headline = cpi)


## Climate data ---------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(5)


# EDA -------------------------------------------------------------------
climate_data_tbl |> skim()
eora_ma_tbl |> skim()
cpi_headline_tbl |> skim()
cpi_industry_tbl |> skim()


# Combining data ---------------------------------------------------------------
combined_data_tbl <-
  eora_ma_tbl |>
  left_join(climate_data_tbl, by = join_by(date == date,
                                            column_country == country)) |>
  left_join(cpi_industry_tbl, by = join_by(date == date,
                                          column_country == country,
                                          column_industry == category
                                          )) |>
  left_join(cpi_headline_tbl, by = join_by(date == date,
                                           column_country == country,
                                           column_industry == category)) # check matching criteria

# Export ---------------------------------------------------------------
artifacts_combined_data <- list (
  combined_data_tbl = combined_data_tbl
)

write_rds(artifacts_combined_data, file = here("Outputs",
                                               "artifacts_combined_data.rds"))
