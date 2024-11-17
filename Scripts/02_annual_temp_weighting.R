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

weighting <- function(year){
# Import -------------------------------------------------------------
data_sr <- stack(here::here("API", "2m_temp_year_2005.nc")
)

# SHP file -------------------------------------------------------------
world_shp <- st_read(
  here(
    "Data",
    "world-administrative-boundaries",
    "world-administrative-boundaries.shp"
  )
)

# Population Weights -------------------------------------------------------------
# landscan <- raster(
#   here::here(
#     "Data",
#     "Landscan",
#     "landscan-global-2022-assets",
#     "landscan-global-2022.tif"
#   )
# )
#
#
# population_weights <- secondary_weights(
#   secondary_raster = landscan,
#   grid = era5_grid,
#   extent = "full"
# )

# Country Weights -------------------------------------------------------------
country_weights <- stagg::overlay_weights(
  polygons = world_shp,
  polygon_id_col = "iso3",
  grid = era5_grid
  # secondary_weights = population_weights
)

print(country_weights)

# Aggregation -------------------------------------------------------------
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
}

years <- 2005:2008

weighted_temp_yearly_tbl <-
  parallel::mclapply(years, weighting, mc.cores = 9) %>%
  set_names(years) %>%
  bind_rows(.id = "year") %>%
  mutate(year = as.numeric(year))

weighted_temp_yearly_tbl
