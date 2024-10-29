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

# Functions ---------------------------------------------------------------


# Import -------------------------------------------------------------
data <- terra::rast(here::here("Data", "Global_drought", "spei01.nc"))


# Transforming to tibble --------------------------------------------------------
data_tbl <- terra::as.data.frame(data, xy = TRUE) %>% as_tibble()


# Write to csv ---------------------------------------------------------------
data_tbl %>% head() %>% write_csv(here("Outputs", "Drought", "data_tbl_sample.csv"))


# Export ---------------------------------------------------------------
artifacts_drought <- list (
  data_tbl = data_tbl
)

write_rds(artifacts_drought , file = here("Outputs", "Drought", "artifacts_drought.rds"))


