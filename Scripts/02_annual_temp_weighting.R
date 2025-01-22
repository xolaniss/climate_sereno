# Purpose -----------------------------------------------------------------
# Calculating yearly weighted temperature data - Xolani Sibande January 2025

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
weighting <- function(year){
  # Import
  data_sr <- stack(here::here("/Users/xolanisibande-dev/Desktop/Data", paste0("2m_temp_year_", year, ".nc")))

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

  print(country_weights)

  # Aggregation
  data <-
    stagg::staggregate_polynomial(
      data = data_sr - 273.15,
      overlay_weights = country_weights,
      daily_agg = "none",
      time_agg = "year",
      degree = 2
    )

  return(data)
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

# Yearly weighted data ----------------------------------------------------
year <- 1940:2023
numberOfCores <- parallel::detectCores()

tic()
weighted_temp_yearly_dt <-
  parallel::mclapply(year, weighting, mc.cores = numberOfCores, mc.preschedule = FALSE)
toc()

weighted_temp_yearly_tbl <-
  weighted_temp_yearly_dt |>
  set_names(year) %>%
  rbindlist(idcol = "year") |>
  dplyr::select(-2) |>
  mutate(year = as.numeric(year)) |>
  as_tibble() |>
  arrange(poly_id) |>
  rename(country = poly_id, temp = order_1 , temp2 = order_2) |>
  drop_na()


# Sample graph ------------------------------------------------------------
weighted_temp_yearly_tbl %>%
  filter(country == "ZAF") %>%
  ggplot(aes(year, temp)) +
  geom_line() +
  labs(title = "Yearly weighted temperature for South Africa",
       x = "Year",
       y = "Temperature (Celsius)") +
  theme_minimal()



# Export ------------------------------------------------------------------
weighted_temp_yearly_tbl |>
  write_rds(here::here("Outputs", "Temperature", paste0("pop_weighted_temp_yearly", ".rds")))
