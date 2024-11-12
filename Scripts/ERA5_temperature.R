# Description
# ERA data analysis - Xolani Sibande Novemebr 2024
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

# Parralel processing
library(future.apply)
library(future)
library(tictoc)

# Functions ---------------------------------------------------------------
source(here("Functions", "temp_nc_extraction.R"))

# Import -------------------------------------------------------------
data_sr <- terra::rast(here::here("API", "2m_temp_2023.nc"))
data_nc <- ncdf4::nc_open(here::here("API", "2m_temp_2023.nc"))
world_shp <- st_read(here("Data",
                          "world-administrative-boundaries",
                          "world-administrative-boundaries.shp")) # From:https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/

# Data extraction -------------------------------------------------------------
plan(sequential)
tic()
world_2m_temp_shp %<-% {temp_nc_extraction(data_sr, sf_shp = world_shp)}
toc()

# Plotting ---------------------------------------------------------------
tm_shape(world_shp, alpha = 0.2) +
  tm_polygons() +
  tm_shape(world_2m_temp_shp %>%  filter(date == "2023-08-05")) + # for a specific day
  tm_polygons(col = "t2m", style = "cont", palette = "viridis") +
  tm_layout(legend.outside.position  =  "right", frame = NA, legend.outside = TRUE)

# Write to rds ---------------------------------------------------------------
world_2m_temp_tbl <-
  world_2m_temp_shp %>%
  terra::as.data.frame() %>%
  dplyr::select(-geometry) %>%
  as_tibble()

write_rds(world_2m_temp_tbl,
           file = here("Outputs", "Temperature", "world_2tm.rds"))



