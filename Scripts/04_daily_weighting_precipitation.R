# Description

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

mem.maxVSize(v = 100000)
# Functions ---------------------------------------------------------------
weighting <- function(year) {
  # Import
  data_sr <- stack(here::here("/Users/xolanisibande-dev/Desktop/Data", paste0("precipitation_day_", year, ".nc")))

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
      data = data_sr - 273.15,
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
# tic()
# weighting(1980)
# toc()

tic()
for (year in 1990:1999) {
  tryCatch({
    weighting(year)
  }, error = function(e) {})
}
toc()


# Aggregation --------------------------------------------------
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
  daily_weighted_precip_tbl |>
  filter(country == "USA", year == 2000) |>
  ggplot(aes(date, precip)) +
  geom_line() +
  labs(title = "Daily weighted precip",
       x = "Date",
       y = "Precipitation") +
  theme_minimal()

daily_weighted_precip_gg
# Export ------------------------------------------------------------------
daily_weighted_precip_tbl |>
  write_rds(here::here("Outputs", "Precipitation", paste0("pop_weighted_precip_day", ".rds")))
