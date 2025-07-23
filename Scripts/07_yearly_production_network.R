# Description
# IOs from serena - Xolani Sibande Jul 2025
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
mem.maxVSize(100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
## IO data ------------------------------------
years <- c(1990:2017)
path_list_IO <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/IO/IO_{.}.txt"
    )
  )

n_cores <- detectCores() - 1 # leave 1 core for OS
plan(multisession, workers = n_cores) # setting multisession plan

tic()
combined_aggregated_shares_tbl <-
  path_list_IO |>
  future_map(~ read_delim(.x, col_names = TRUE, show_col_types = FALSE)) |>
  set_names(years) |>
  bind_rows(.id = "year") |>
  mutate(industry =
           tidytable::case_when(
             industry == "agri" ~ "Agrifood",
             .default = "Downstream")) |>
  rename("row_country" = country,
         "row_industry" = industry) |>
  mutate(year = parse_date(year, format = "%Y"))
toc()
plan(sequential)


# Export ---------------------------------------------------------------
artifacts_eoro26 <- list (
  combined_aggregated_shares_tbl = combined_aggregated_shares_tbl
)
write_rds(artifacts_eoro26,
          file = here("Outputs", "artifacts_eoro26.rds"))











