# Description
# Agriciultural land temp weighting - Xolani Sibande April 2025

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
library(parallel)

mem.maxVSize(v = 100000)
# Functions ---------------------------------------------------------------
weighting_temp <- function(year) {
  # Import
  data_sr <- stack(here::here("/Users/xolanisibande-dev/Desktop/Temperature",
                              paste0("2m_temp_day_", year, ".nc")))

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
    secondary_weights = land_weights
  )

  print(country_weights)

  # Aggregation
  data <-
    stagg::staggregate_polynomial(
      data = data_sr - 273.15,
      overlay_weights = country_weights,
      daily_agg = "none",
      time_agg = "day",
      time_interval = '1 day',
      start_date = paste0(year, "-01-01"),
      degree = 2
    )

  # data_tbl <-
    data |>
    as_tibble() |>
    arrange(poly_id) |>
    rename(country = poly_id, temp = order_1 , temp2 = order_2) |>
    mutate(date = as.Date(paste0(year, "-", month, "-", day))) |>
    dplyr::select(-c(month, day)) |>
    relocate(date, .before = country) |>
    drop_na()

  data_tbl |>
    write_rds(here::here("Outputs", "Temperature",
                         paste0("land_weighted_temp_day_", year, ".rds")))
  gc()
}

# Population Weights -----------------------------------------------------------
global_land <- rast(
  here::here(
    "Data",
    "gl_cropland_geotif",
    "cropland.tif"
  )
)

land_weights <- secondary_weights(
  secondary_raster = global_land,
  grid = era5_grid,
  extent = "full"
)


# Daily weighted temps ----------------------------------------------------
# tic()
# weighting_temp(1990)
# toc()

tic()
for (year in 1991:2017) {
  tryCatch({
    weighting_temp(year)
  }, error = function(e) {})
}
toc()




