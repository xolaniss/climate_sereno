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
## Network shocks data ---------------------------------------------------------------
network_shocks <-
  read_rds(here("Outputs", "artifacts_network_shocks.rds"))

domestic_network_shocks_tbl <- network_shocks |>
  pluck(1)

foreign_network_shocks_tbl <- network_shocks |>
  pluck(2)

## Inflation data ---------------------------------------------------------------
inflation_rate_tbl <- read_rds(here("Outputs", "artifacts_inflation_rate.rds")) |>
  pluck(1) |>
  mutate(
    date = as.Date(date)
  ) |>
  rename(
    industry = category
  ) |>
  mutate(industry = str_to_title(industry)) |>
  filter(date >= "2000-01-01")
inflation_rate_tbl

## Climate data ---------------------------------------------------------------
climate_data_tbl <- read_rds(here("Outputs", "artifacts_climate_data.rds")) |>
  pluck(5)


# Combining data ---------------------------------------------------------------
combined_data_tbl <-
  inflation_rate_tbl |>
  filter(industry != "headline") |>
  inner_join(domestic_network_shocks_tbl,
            by = join_by(date,
                        country,
                        industry)) |>
  drop_na()
combined_data_tbl |> print(n = 100)
domestic_network_shocks_tbl |> print(n = 100)
combined_data_tbl |>
  group_by(country, industry) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  print(n = Inf)

# Export ---------------------------------------------------------------
artifacts_combined_data <- list (
  combined_data_tbl = combined_data_tbl
)

write_rds(artifacts_combined_data, file = here("Outputs",
                                               "artifacts_combined_data.rds"))
