# Description
# MA data from Serena - Xolani Sibande April 2025
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(tidyfast)
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
library(leontief)

# parallel processing
library(furrr)
library(parallel)
library(tictoc)

mem.maxVSize(100000)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
years <- c(1990:2017)
path_list <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/moving_averages/MA_{.}.txt"
    )
  )

n_cores <- detectCores() - 1 # leave 1 core for OS
plan(multisession, workers = n_cores) # setting multisession plan

tic()
eora_ma_yearly_tbl <-
  path_list |>
  future_map(~ read_delim(.x, col_names = TRUE, show_col_types = FALSE)) |>
  set_names(years) |>
  bind_rows(.id = "year") |>
  # # janitor::clean_names() |>
  mutate(year = as.Date(year, format = "%Y")) |>
  mutate(year = year  %-time% "3months") |>
  mutate(year = year  %-time% "16days")
toc()
plan(sequential)



# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_ma_yearly_tbl = eora_ma_yearly_tbl
)

write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_combined_eoro26.rds"))




