# Description
# Temperature and precipitation aggregation- Xolani Sibande January 2025
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

# Temperature Aggregation --------------------------------------------------
path_list <- list.files(here::here("Outputs", "Temperature"),
                        full.names = TRUE,
                        pattern = "pop_weighted_temp_day_")
list_names <- path_list |>
  map(~ str_extract(.x, "[0-9]{4}")) |>
  map(~ as.numeric(.x)) |>
  unlist()

daily_weighted_temp_tbl <-
  path_list |>
  set_names(list_names) |>
  map(~ read_rds(.x)) |>
  bind_rows(.id = "year")


daily_weighted_temp_gg <-
  daily_weighted_temp_tbl |>
  filter(country == "ZAF", year == 2019) |>
  ggplot(aes(date, temp)) +
  geom_line() +
  labs(title = "Daily weighted temperature",
       x = "Date",
       y = "Temperature (Celsius)") +
  theme_minimal()

daily_weighted_temp_gg


# Precipitation Aggregation --------------------------------------------------
path_list <- list.files(here::here("Outputs", "Precipitation"),
                        full.names = TRUE,
                        pattern = "pop_weighted_precip_day_")
list_names <- path_list |>
  map(~ str_extract(.x, "[0-9]{4}")) |>
  map(~ as.numeric(.x)) |>
  unlist()

daily_weighted_precip_tbl <-
  path_list |>
  set_names(list_names) |>
  map(~ read_rds(.x)) |>
  bind_rows(.id = "year")

daily_weighted_precip_gg <-
  daily_weighted_precip_tbl |>
  filter(country == "ZAF") |>
ggplot(aes(date, precip)) +
  geom_line() +
  labs(title = "Daily weighted precip",
       x = "Date",
       y = "Precipitation (mm)") +
  theme_minimal()

daily_weighted_precip_gg

# EDA ---------------------------------------------------------------------
daily_weighted_temp_tbl |> skim()
daily_weighted_precip_tbl |> skim()

# Export ------------------------------------------------------------------
daily_weighted_temp_tbl |>
  write_rds(here::here("Outputs", "Temperature", paste0("pop_weighted_temp_day", ".rds")))
daily_weighted_precip_tbl |>
  write_rds(here::here("Outputs", "Precipitation", paste0("pop_weighted_precip_day", ".rds")))

