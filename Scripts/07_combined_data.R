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
eora_ma_tbl <- read_rds(here("Outputs", "artifacts_combined_eoro26.rds")) |>
  pluck(1) |>
  mutate(year = as.numeric(year))

cpi_tbl <- read_rds(here("Outputs", "artifacts_cpi.rds")) |>
  pluck(1) |>
  filter(date > "1990-01-01") |>
  group_by(category, country_abs) |>
  summarise_by_time(
    .date_var = date,
    .by = "year",
    cpi = mean(cpi, na.rm = FALSE)
  ) |>
  rename(
    year = date
  ) |>
  ungroup() |>
  relocate(
    year,
    .before = category
  ) |>
  mutate(year = as.numeric(year(year))) |>
  rename(
    country = country_abs
  )

precipitation_tbl <- read_rds(here("Outputs",
                                   "Precipitation",
                                   "yearly_precip.rds")) |>
  relocate(
    year,
    .before = country
  )

temp_tbl <- read_rds(here("Outputs",
                          "Temperature",
                          "yearly_temp.rds")) |>
  relocate(
    year,
    .before = country
  )


# Combining data ---------------------------------------------------------------
combined_data_tbl <-
  eora_ma_tbl |>
  left_join(
    cpi_tbl,
    by = c("year", "country")
  ) |>
  left_join(
    precipitation_tbl,
    by = c("year", "country")
  ) |>
  left_join(
    temp_tbl,
    by = c("year", "country")
  )

combined_data_tbl |> glimpse()
# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
