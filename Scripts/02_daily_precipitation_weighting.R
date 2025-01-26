# Description
# Precipitation weighting - Xolani Sibande January 2025
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
library(tmap)
library(tidyfast)
library(data.table)
library(exactextractr)
library(stars)
library(tigris)
library(stagg)

# Parralel processing
library(future.apply)
library(future)
library(tictoc)
library(doFuture)
options(scipen = 999)
mem.maxVSize(v = 100000)
# Functions ---------------------------------------------------------------
weighting_precip <- function(year) {
  # Import
  data_sr <- stack(here::here("/Users/xolanisibande-dev/Desktop/precipitation", paste0("precipitation_day_", year, ".nc")))

  # SHP file
  world_shp <- st_read(
    here(
      "Data",
      "world-administrative-boundaries",
      "world-administrative-boundaries.shp"
    )
  )

  # Country Weights
  country_weights <- stagg::overlay_weights(
    polygons = world_shp,
    polygon_id_col = "iso3",
    grid = era5_grid,
    secondary_weights = population_weights
  )

  # print(country_weights)

  # Aggregation
  data <-
    stagg::staggregate_polynomial(
      data = data_sr,
      overlay_weights = country_weights,
      daily_agg = "none",
      time_agg = "day",
      time_interval = '1 day',
      start_date = paste0(year, "-01-01"),
      degree = 2
    )

  data_tbl <-
    data |>
    as_tibble() |>
    arrange(poly_id) |>
    rename(country = poly_id, precip = order_1 , precip2 = order_2) |>
    mutate(date = as.Date(paste0(year, "-", month, "-", day))) |>
    dplyr::select(-c(month, day)) |>
    relocate(date, .before = country) |>
    drop_na()

  data_tbl |>
    write_rds(here::here("Outputs", "Precipitation", paste0("pop_weighted_precip_day_", year, ".rds")))
  gc()
}

# Population Weights -----------------------------------------------------------
landscan <- rast(
  here::here(
    "Data",
    "landscan-global-2000-assets",
    "landscan-global-2000.tif"
  )
)

population_weights <- secondary_weights(
  secondary_raster = landscan,
  grid = era5_grid,
  extent = "full"
)


# Daily weighted temps ----------------------------------------------------
tic()
weighting_precip(1942)
toc()

# tic()
# for (year in 2003:2023) {
#   tryCatch({
#     weighting_precip(year)
#   }, error = function(e) {})
# }
# toc()



