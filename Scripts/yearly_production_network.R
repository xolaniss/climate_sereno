# Description
# Calculating shares over time - Xolani Sibande May 2025
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

options(scipn = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
years <- c(1990:2017)
path_list <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/Eora_transaction/Eora26_{.}_bp_T.txt"
    )
  )

n_cores <- detectCores() - 1 # leave 1 core for OS
plan(multisession, workers = n_cores) # setting multisession plan

tic()
eora_ma_yearly_tbl <-
  path_list |>
  future_map(~ read_delim(.x, col_names = FALSE, show_col_types = FALSE)) |>
  set_names(years)
toc()
plan(sequential)

tic()
eora_ma_yearly_shares_list <-
  eora_ma_yearly_tbl |>
  # remove last col from each tibble
  map(~ .x |> dplyr::select(-last_col())) |>
  # remove last row from each tibble
  map(~ .x |> dplyr::slice(-4915)) |>
  # add row sum col
  map(~ .x |> dplyr::mutate(row_sum = rowSums(.))) |>
 # calculate shares as a proportion of the row sum
  map(~ .x |> dplyr::mutate(across(everything(), ~ .x / row_sum), .keep = "none")) |>
  # remove row sum col
  map(~ .x |> dplyr::select(-row_sum))
toc()

tic()
eora_ma_average_shares_list <-
  eora_ma_yearly_shares_list |>
  reduce(
    .init = eora_ma_yearly_shares_list[[1]],
    .f = function(x, y) {
      (x + y)
    } |>
      as_tibble()
  ) |>
  mutate(across(everything(), ~ .x / length(eora_ma_yearly_shares_list))) |>
  mutate(across(everything(), ~ round(.x, 3)))
toc()

eora_ma_average_shares_list


# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_ma_average_shares_list = eora_ma_average_shares_list
)
write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_eoro26_average_shares.rds"))
