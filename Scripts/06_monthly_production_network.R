# Description
# Expanding production network to monthly - Xolani Sibande April 2025
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
eora_ma_tbl <- read_rds(here("Outputs", "artifacts_combined_eoro26.rds")) |>
  pluck(1)

# Expanding to monthly frequency ------------------------------------------
expanded_dates_tbl <- rep(seq(
  from = as.Date("1990-01-01"),
  to = as.Date("2017-12-01"),
  by = "month"
), 189) |>
  as_tibble() # expanded dates for monthly

country_names_vec <- eora_ma_tbl |> distinct(country)
country_names_tbl <- tibble("country" = rep(country_names_vec, times = 336)) |>
  unnest(country) |>
  arrange(country) |>
  tibble("date" = expanded_dates_tbl$value) |>
  relocate(country, .after = date) # creating monthly date and name combination


## Expanding agrifood ------------------------------------------------------
eora_ma_monthly_agri_food_tbl <-
  country_names_tbl |>
  left_join(
    eora_ma_tbl |>
      filter(industry == "agrifood"),
    by = join_by(date == year, country == country)
  ) |>
  fill(c(3:1893), .direction = "down") # monthly for agrifood


## Expanding downstream ----------------------------------------------------
eora_ma_monthly_downstream_tbl <-
  country_names_tbl |>
  left_join(
    eora_ma_tbl |>
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
artifacts_monthly_production_network <- list (

)

write_rds(artifacts_monthly_production_network,
          file = here("Outputs", "artifacts_monthly_production_network.rds"))


