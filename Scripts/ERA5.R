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

# Functions ---------------------------------------------------------------


# Import -------------------------------------------------------------
data <- terra::rast(here::here("API", "2m_temp_2023.nc"))

# Transforming to tibble -----------------------------------------------------------------
data_tbl <- terra::as.data.frame(data, xy = TRUE) %>% as_tibble()

# Write to csv ---------------------------------------------------------------
data_tbl %>% head(800) %>% write_csv(here("Outputs", "ERA5", "data_tbl_sample.csv"))


# Export ---------------------------------------------------------------
artifacts_2m_2023 <- list (
  data_tbl = data_tbl
)

write_rds(artifacts_2m_2023, file = here("Outputs", "ERA5",  "artifacts_2m_2023.rds"))


