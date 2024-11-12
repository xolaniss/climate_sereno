# Description
# Global drought data - Xolani Sibande November 2024
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

# Spatial
library(sf)
library(stars)
library(tidyverse)
library(raster)
library(terra)
library(furrr)

# Functions ---------------------------------------------------------------


# Import -------------------------------------------------------------

files_19_months <- list.files(path = here::here("Data", "Global_drought"), pattern = "*.nc")[1:19]
files_48_months <- list.files(path = here::here("Data", "Global_drought"), pattern = "*.nc")[48]

plan(sequential)
data_19_months <- future_map(files_19_months, ~terra::rast(here::here("Data", "Global_drought", .x)))
terra::plot(data_19_months[[5]])

data_19_months_tbl <- future_map(data_19_months, ~terra::as.data.frame(.x, xy = TRUE) %>% as_tibble())

data_19_months_tbl %>% glimpse()

data_48_months <- future_map(files_48_months, ~terra::rast(here::here("Data", "Global_drought", .x)))
terra::plot(data_48_months[[9]])
map(data_48_months, ~terra::describe(.x))



data_48_months_tbl <- future_map(data_48_months, ~terra::as.data.frame(.x, xy = TRUE) %>% as_tibble())


# Write to csv ---------------------------------------------------------------
# data_tbl %>% head() %>% write_csv(here("Outputs", "Drought", "data_tbl_sample.csv"))


# Export ---------------------------------------------------------------
artifacts_drought <- list (
  data_tbl = data_tbl
)

write_rds(artifacts_drought , file = here("Outputs", "Drought", "artifacts_drought.rds"))


