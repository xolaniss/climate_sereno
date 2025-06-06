# Description
# Weighted climate data from https://www.santannapisa.it/en/istituto/economia
# and https://weightedclimatedata.streamlit.app/Download_Data
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------

## Temperature ---------------------
land_weighted_temp_tbl <-
  read_csv(here("Data", "_data_gadm0_era_tmp_cropland_2000_monthly.csv")) |>
  dplyr::select(-1) |>
  rename(
    date = 1,
    country = 2,
    temp = 3
  )

population_weighted_temp_tbl <-
  read_csv(here("Data", "_data_gadm0_era_tmp_pop_2000_monthly.csv")) |>
  dplyr::select(-1) |>
  rename(
    date = 1,
    country = 2,
    temp = 3
  )

## Precipitation ---------------------
land_weighted_precip_tbl <-
  read_csv(here("Data", "_data_gadm0_era_pre_cropland_2000_monthly.csv")) |>
  dplyr::select(-1) |>
  rename(
    date = 1,
    country = 2,
    precip = 3
  )

population_weighted_precip_tbl <-
  read_csv(here("Data", "_data_gadm0_era_pre_pop_2000_monthly.csv")) |>
  dplyr::select(-1) |>
  rename(
    date = 1,
    country = 2,
    precip = 3
  )


# Graphing ---------------------------------------------------------------
## Plotting the temperature -------
### Plotting the land weighted temperature -------
land_weighted_temp_gg <-
  land_weighted_temp_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(x = date, y = temp)) +
  geom_line() +
  labs(
    title = "Land Weighted Temperature",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Plotting the population weighted temperature -------
population_weighted_temp_gg <-
  population_weighted_temp_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(x = date, y = temp)) +
  geom_line() +
  labs(
    title = "Population Weighted Temperature",
    x = "Date",
    y = "Temperature (C)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plotting the precipitation -------
### Plotting the land weighted precipitation -------
land_weighted_precip_gg <-
  land_weighted_precip_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(x = date, y = precip)) +
  geom_line() +
  labs(
    title = "Land Weighted Precipitation",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Plotting the population weighted precipitation -------
population_weighted_precip_gg <-
  population_weighted_precip_tbl |>
  filter(country == "ZAF") |>
  ggplot(aes(x = date, y = precip)) +
  geom_line() +
  labs(
    title = "Population Weighted Precipitation",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combined data --------------------
combined_climate_data_tbl <-
  land_weighted_temp_tbl |>
  left_join(population_weighted_temp_tbl, by = c("date", "country")) |>
  rename(
    land_weighted_temp = temp.x,
    population_weighted_temp = temp.y
  ) |>
  left_join(land_weighted_precip_tbl, by = c("date", "country")) |>
  left_join(population_weighted_precip_tbl, by = c("date", "country")) |>
  rename(
    land_weighted_precip = precip.x,
    population_weighted_precip = precip.y
  )

# Export ---------------------------------------------------------------
artifacts_climate_data <- list (
  land_weighted_temp_tbl = land_weighted_temp_tbl,
  population_weighted_temp_tbl = population_weighted_temp_tbl,
  land_weighted_precip_tbl = land_weighted_precip_tbl,
  population_weighted_precip_tbl = population_weighted_precip_tbl,
  combined_climate_data_tbl = combined_climate_data_tbl
)

write_rds(artifacts_climate_data, file = here("Outputs",
                                              "artifacts_climate_data.rds"))
