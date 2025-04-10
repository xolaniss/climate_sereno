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
  # janitor::clean_names() |>
  mutate(year = as.Date(year, format = "%Y")) |>
  mutate(year = year  %-time% "3months") |>
  mutate(year = year  %-time% "9days")
toc()
plan(sequential)


# Expanding to monthly frequency ------------------------------------------
expanded_dates_tbl <- rep(seq(
  from = as.Date("1990-01-01"),
  to = as.Date("2017-12-01"),
  by = "month"
), 189) |>
  as_tibble() # expanded dates for monthly

country_names_vec <- eora_ma_yearly_tbl |> distinct(country)
country_names_tbl <- tibble("country" = rep(country_names_vec, times = 336)) |>
  unnest(country) |>
  arrange(country) |>
  tibble("date" = expanded_dates_tbl$value) |>
  relocate(country, .after = date) # creating monthly date and name combination


## Expanding agrifood ------------------------------------------------------
eora_ma_monthly_agri_food_tbl <-
  country_names_tbl |>
  left_join(
    eora_ma_yearly_tbl |>
      filter(industry == "agrifood"),
    by = join_by(date == year, country == country)
  ) |>
  fill(c(3:1893), .direction = "down") # monthly for agrifood


## Expanding downstream ----------------------------------------------------
eora_ma_monthly_downstream_tbl <-
  country_names_tbl |>
  left_join(
    eora_ma_yearly_tbl |>
      filter(industry == "downstream"),
    by = join_by(date == year, country == country)
  ) |>
  fill(c(3:1893), .direction = "down") # monthly for downstream


## Combining back to full tbl ----------------------------------------------
eora_ma_monthly_tbl <-
  eora_ma_monthly_agri_food_tbl |>
  bind_rows(eora_ma_monthly_downstream_tbl) |>
  arrange(country) |>
  rename(row_country = country) |>
  rename(row_industry = industry)


## Pivoting longer using dt -------------------------------------------------------
tic()
eora_ma_monthly_long_tbl <-
  as.data.table(eora_ma_monthly_tbl) |>
  dt_pivot_longer(
    cols = -c(date, row_country, row_industry),
    names_to = "sector",
    values_to = "ma"
  ) |>
  dt_separate(sector,
              sep = ".",
              into = c("column_country", "column_industry")
              ) |>
  as_tibble() |>
  mutate(column_industry  = str_to_lower(column_industry))
toc()

# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_ma_monthly_long_tbl = eora_ma_monthly_long_tbl
)

write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_combined_eoro26.rds"))




