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
library(stars)

# Parralel processing
library(future.apply)
library(future)
library(tictoc)
library(doFuture)

# Functions ---------------------------------------------------------------
source(here("Functions", "agg.R"))
terra::gdalCache(10000)
temp_workflow <- function(data, shp){

  extraction_dt <- parallel::mclapply(1:nrow(shp),
                                      \(x) agg_coverage(x),
                                      mc.cores = 9,
                                      mc.preschedule = FALSE)

  extraction_tbl <-
    extraction_dt %>%
    keep(str_detect(., "Error", negate = TRUE)) |>
    rbindlist(idcol = "id") |>
    as_tibble()

  # Joining sf data
  temp_shp <- world_shp %>%
    left_join(extraction_tbl, by = "id") %>%
    dplyr::select(id, date, name, region, t2m)

  # Export
  temp_tbl <-
    temp_shp %>%
    terra::as.data.frame() %>%
    dplyr::select(-geometry) %>%
    as_tibble()

  temp_tbl
}

# Workflow ---------------------------------------------------------------
data_sr <- terra::rast(here::here("/Users/xolanisibande-dev/Desktop/Data", paste0("2m_temp_day_", 2000, ".nc")))
world_shp <- st_read(
  here(
    "Data",
    "world-administrative-boundaries",
    "world-administrative-boundaries.shp"
  )
) |>
  st_transform(crs = st_crs(data_sr)) |>
  mutate(id = row_number())

temp_workflow(data = data_sr, shp = world_shp)











