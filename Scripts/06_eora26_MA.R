# Description
# MA data from Serena - Xolani Sibande April 2025
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

# parallel processing
library(furrr)
library(parallel)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
years <- c(1990:2017)
path_list <-
  years |>
  map(
    ~ glue(
      "/Users/xolanisibande/Papers/climate_sereno/Data/Eora26/moving_averages/MA_{.}.txt"
    )
  )

n_cores <- detectCores() - 1 # leave 2 cores for OS
plan(multisession, workers = n_cores) # setting multisession plan

tic()
eora_ma_tbl <-
  path_list |>
  future_map(
    ~ read_delim(
      .x,
      col_names = TRUE,
      show_col_types = FALSE
    )
  ) |>
  set_names(years) |>
  bind_rows(.id = "year") |>
  janitor::clean_names() |>
  pivot_longer(cols = -c(year, country, industry),
             names_to = "sector",
             values_to = "ma") |>
  mutate(sector = str_replace_all(sector, "household_goods","householdgoods" )) |>
  separate(sector, sep = "_", into = c("source_country", "sector")) |>
  mutate(source_country  = str_to_upper(source_country))

toc()
plan(sequential)


# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_ma_tbl = eora_ma_tbl
)

write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_combined_eoro26.rds"))




