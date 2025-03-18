# Description
# Creating yearly time series from daily data - Xolani Sibande January 2025
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
daily_weighted_temp_tbl <- read_rds(here("Outputs", "Temperature", "pop_weighted_temp_day.rds"))
daily_weighted_precip_tbl <- read_rds(here("Outputs", "Precipitation", "pop_weighted_precip_day.rds"))

# Temperature ---------------------------------------------------------------
monthly_temp_tbl <-
  daily_weighted_temp_tbl |>
  dplyr::select(-year) |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  group_by(country, year, month) |>
  summarise(temp = mean(temp, na.rm = TRUE),
            temp2 = mean(temp2, na.rm = TRUE),
            .groups = "drop") |>
  mutate(date = as.Date(paste0(year, "-", month, "-01"), format = "%Y-%b-%d")) |>
  relocate(date, .before = year)

monthly_temp_tbl |>
  filter(country == "USA") |>
  ggplot(aes(date, temp)) +
  geom_line() +
  labs(title = "Monthly weighted temperature",
       x = "Month",
       y = "Temperature (Celsius)") +
  theme_minimal()

yearly_temp_tbl <-
  daily_weighted_temp_tbl |>
  dplyr::select(-year) |>
  mutate(year = lubridate::year(date)) |>
  group_by(country, year) |>
  summarise(temp = mean(temp, na.rm = TRUE),
            temp2 = mean(temp2, na.rm = TRUE),
            .groups = "drop")


yearly_temp_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(year, temp)) +
  geom_line() +
  labs(title = "Yearly weighted temperature",
       x = "Year",
       y = "Temperature (Celsius)")+
  theme_minimal()

# Precipitation ---------------------------------------------------------------
monthly_precip_tbl <-
  daily_weighted_precip_tbl |>
  dplyr::select(-year) |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  group_by(country, year, month) |>
  summarise(precip = sum(precip, na.rm = TRUE),
            precip2 = sum(precip2, na.rm = TRUE),
            .groups = "drop") |>
  mutate(date = as.Date(paste0(year, "-", month, "-01"), format = "%Y-%b-%d")) |>
  relocate(date, .before = year)

monthly_precip_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(date, precip)) +
  geom_line() +
  labs(title = "Monthly weighted precipitation",
       x = "Month",
       y = "Precipitation (mm)") +
  scale_y_continuous(breaks = seq(0, 1000, by = 50)) +
  theme_minimal()


yearly_precip_tbl <-
  daily_weighted_precip_tbl |>
  dplyr::select(-year) |>
  mutate(year = lubridate::year(date)) |>
  group_by(country, year) |>
  summarise(precip = sum(precip, na.rm = TRUE),
            precip2 = sum(precip2, na.rm = TRUE),
            .groups = "drop")


yearly_precip_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(year, precip)) +
  geom_line() +
  labs(title = "Yearly weighted precipitation",
       x = "Year",
       y = "Precipitation (mm)") +
  # set the y scale in increments of 50
  scale_y_continuous(breaks = seq(0, 1000, by = 50)) +
  theme_minimal()

yearly_precip_tbl |> tail()


# Export ------------------------------------------------------------------
monthly_temp_tbl |>
  write_rds(here("Outputs", "Temperature", "monthly_temp.rds"))
monthly_precip_tbl |>
  write_rds(here("Outputs", "Precipitation", "monthly_precip.rds"))
yearly_temp_tbl |>
  write_rds(here("Outputs", "Temperature", "yearly_temp.rds"))
yearly_precip_tbl |>
  write_rds(here("Outputs", "Precipitation", "yearly_precip.rds"))

