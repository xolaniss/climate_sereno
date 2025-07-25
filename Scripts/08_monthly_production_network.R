# Description
# Expanding production network to monthly - Xolani Sibande April 2025
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(dtplyr)
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
library(data.table)
library(tidyfast)

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

library(tictoc)

mem.maxVSize(100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
eora_tbl <- read_rds(here("Outputs", "artifacts_eoro26.rds")) |>
  pluck(1)
eora_tbl |> distinct(row_country)
# Expanding to monthly frequency ------------------------------------------
expanded_dates_tbl <- rep(seq(
  from = as.Date("1990-01-01"),
  to = as.Date("2017-12-01"),
  by = "month"
), 189) |>
  as_tibble() # expanded dates for monthly

country_names_vec <- eora_tbl |> distinct(row_country)
country_names_tbl <- tibble("country" = rep(country_names_vec, times = 336)) |>
  unnest(country) |>
  arrange(country) |>
  tibble("date" = expanded_dates_tbl$value) |>
  relocate(country, .after = date) |> # creating monthly date and name combination
  filter(date >= "2000-01-01")

## Expanding agrifood ------------------------------------------------------
eora_monthly_agri_food_tbl <-
  country_names_tbl |>
  dplyr::left_join(
    eora_tbl |>
      filter(row_industry == "Agrifood"),
    by = join_by(date == year, country == row_country)
  ) |>
  mutate(industry  = "Agrifood") |>
  fill(c(3:1704), .direction = "down")   # monthly for agrifood

## Expanding downstream ----------------------------------------------------
eora_monthly_downstream_tbl <-
  country_names_tbl |>
  dplyr::left_join(
    eora_tbl |>
      filter(row_industry == "Downstream"),
    by = join_by(date == year, country == row_country)
  ) |>
  mutate(industry  = "Downstream") |>
  fill(c(3:1704), .direction = "down") # monthly for downstream

## Combining back to full tbl ----------------------------------------------
eora_monthly_tbl <-
  eora_monthly_agri_food_tbl |>
  bind_rows(eora_monthly_downstream_tbl) |>
  arrange(country) |>
  rename(row_country = country)

## Pivoting longer -------------------------------------------------------
# tic()
# eora_monthly_long_tbl <-
#   eora_monthly_tbl |>
#   # pivot_longer using tidytable
#   tidytable::pivot_longer(
#     cols = -c(date, row_country, row_industry),
#     names_to = "col_country",
#     values_to = "shares"
#   ) |>
#   tidytable::separate(col = "col_country",
#                       into = c("col_country", "col_industry"),
#                       sep = "[A-Z]{3}_")
#   dplyr::select(date,
#                 row_country,
#                 row_industry,
#                 col_country,
#                 col_industry,
#                 shares) |>
#   as_tibble()
# toc()


# Export ---------------------------------------------------------------
artifacts_monthly_production_network <- list (
  eora_monthly_tbl = eora_monthly_tbl
)

write_rds(artifacts_monthly_production_network,
          file = here("Outputs", "artifacts_monthly_production_network.rds"))

