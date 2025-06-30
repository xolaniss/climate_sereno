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

path_list_final_demand <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/Eora_final_demand/Eora26_{.}_bp_FD.txt"
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
  mutate(year = year(year)) |>
  mutate(year = as.Date(paste0(year, "-01-01")))
toc()


tic()
eora_final_demand_list <-
  path_list_final_demand |>
  future_map(~ read_delim(.x, col_names = FALSE, show_col_types = FALSE)) |>
  set_names(years)

toc()
plan(sequential)

eora_final_demand_names_tbl <-
  read_delim(here("Data", "Eora26", "indices", "labels_FD.txt"),
             col_names = FALSE, show_col_types = FALSE) |>
  dplyr::select(-last_col()) |>
  # remove last 6 rows
  slice(-c(1135:1140)) |>
  mutate(names = paste(X2, X3, sep = ".")) |>
  mutate(names = paste(names, X4, sep = "_")) |>
  # remove spaces in names col
  mutate(names = str_replace_all(names, " ", "_")) |>
  dplyr::select(names)

eora_transaction_names_tbl <-
  read_delim(here("Data", "Eora26", "indices", "labels_T.txt"),
             col_names = FALSE, show_col_types = FALSE) |>
  dplyr::select(-last_col()) |>
  slice(-4915) |>
  mutate(names = paste(X1, X4, sep = ".")) |>
  mutate(names = str_replace_all(names, " ", "_")) |>
  dplyr::select(X1, X4, names)

# Clean final demand -----------------------------------------
eora_final_demand_tbl <-
  eora_final_demand_list |>
  map(~ .x |> dplyr::slice(-4915)) |>
  map(~ .x |> dplyr::select(-c(1135:1140))) |>
  map(~ .x |> dplyr::rename_with(~ eora_final_demand_names_tbl$names))
  map(~ .x |> dplyr::mutate(industry = eora_transaction_names_tbl$X4)) |>
  map(~.x |> relocate(industry, .before = 1)) |>
  map(~ .x |> pivot_longer(
    cols = -industry,
    names_to = "col_name",
    values_to = "final_demand")) |>
  map(~ .x |>
        mutate(country = str_sub(col_name, 1, 3)) |>
        dplyr::select(-col_name)) |>
  map(~ .x |> group_by(country, industry) |>
        summarise(final_demand = sum(final_demand, na.rm = TRUE),
                  .groups = "drop")) |>
  bind_rows(.id = "year")




# Turning to shares -----------------------------------
tic()
eora_ma_shares_yearly_tbl <-
  eora_ma_yearly_tbl |>
  mutate(
    across(
      c(-year, -country, -industry),
      ~ .x / rowSums(across(c(-year, -country, -industry)), na.rm = TRUE)
    )
  ) |>
  mutate(
    across(
      c(-year, -country, -industry),
      ~ round(.x, 4)
    )
  )
toc()
# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_ma_shares_yearly_tbl = eora_ma_shares_yearly_tbl
)

write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_combined_eoro26.rds"))




