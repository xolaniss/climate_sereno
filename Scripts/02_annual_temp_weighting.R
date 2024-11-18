# Load Libraries -------------------------------------------------------------
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


weighting <- function(year){
# Import
data_sr <- stack(here::here("API", paste0("2m_temp_year_", year, ".nc")))

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

data_tbl <-
  data %>%
  as_tibble() %>%
  arrange(poly_id) %>%
  rename(country = poly_id, temp = order_1 , temp2 = order_2)

return(data_tbl)
gc()
}

years <- 2000:2023

tic()
weighted_temp_yearly_tbl <-
  parallel::mclapply(years, weighting, mc.cores = 9, mc.preschedule = FALSE) %>%
  set_names(years) %>%
  keep(str_detect(., "Error", negate = TRUE)) %>%
  bind_rows(.id = "year") %>%
  mutate(year = as.numeric(year))
toc()

weighted_temp_yearly_tbl %>% drop_na() %>% filter(country == "ZAF")


