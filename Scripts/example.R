# Description
# Attempting the ERA5 data - 14 October 2024
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

# Spatial data
library(ncdf4)
library(terra)
library(sf)

# Parallel processing
library(tictoc)
library(future)
library(doFuture)
library(parallel)
library(data.table)
library(ff)
library(stars)
library(ncmeta)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
conversion_function <- function(data){
  data_tbl <- as.data.frame(ncin, xy = TRUE) %>% tibble()
  data_tbl
}

registerDoFuture()

n_cores <- parallel::detectCores()
plan(
  strategy = cluster,
  workers = parallel::makeCluster(n_cores)
)

read_ncdf(here("API", "2m_temp_2023.nc")) %>%
  dplyr::select(t2m)
  slice(index = 1, along = "valid_time") %>%
  plot()





tic()
ncin <- rast(here("API", "2m_temp_2023.nc"))
data_table <-  as.data.table(ncin, xy = TRUE)
toc()




tic()
ncin <- rast(here("API", "2m_temp_2023.nc"))
data_tbl <-  as.data.frame(ncin, xy = TRUE) %>% tibble()
# data_tbl %>%
#   pivot_longer(
#     cols = -c(x, y),
#     names_to = "time",
#     values_to = "temp"
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x = x, y = y, fill = temp)) +
#   facet_wrap(~time) +
#   coord_equal() +
#   scale_fill_viridis_c() +
#   theme_void() +
#   theme(
#     legend.position = "right"
#   )
toc()

data_tbl %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "time",
    values_to = "temp"
  )

write.csv(data_tbl, here("API", "data_tbl.csv"))

write_rds(data_tbl, here("API", "data_tbl.rds"))

# plan(multicore, workers = detectCores() - 1)




#
# data_tbl <-  as.data.frame(ncin, xy = TRUE) %>% tibble()
#
#
# f = future ({
#   ncin <- rast(here("API", "2m_temp_2023.nc"))
#   dim(ncin)
#   ncell(ncin)
#   data_tbl <-  as.data.frame(ncin, xy = TRUE) %>% tibble()
#   data_tbl
# })
#
# value(f)
#
#
# ncin <- rast(here("API", "2m_temp_2023.nc"))
# dim(ncin)
# ncell(ncin)
#
# data_tbl <- future_lapply(ncin, function(x) as.data.fram(x, xy = TRUE))
#
#
# as.data.frame(ncin[[1:10]], xy = TRUE) %>% tibble()
#
#
#
#
# crs(ncin)
#
# ncin_lower <- aggregate(ncin[[1]], fact = 2, fun = mean)
# plot(ncin_lower)
# plot(ncin[[1]], xlim = c(-0.125, 359.875), ylim = c(-90.125, 90.125))
# ncin_mean <- mean(ncin[[1]])
# ncell(ncin[[1]])
# ncell(ncin_lower)
#
#
# head(data_tbl)
#
#
# data_tbl %>%
#   pivot_longer(
#     cols = -c(x, y),
#     names_to = "time",
#     values_to = "temp"
#   ) %>%
#   ggplot() +
#   geom_raster(aes(x = x, y = y, fill = temp)) +
#   facet_wrap(~time) +
#   coord_equal() +
#   scale_fill_viridis_c() +
#   theme_void() +
#   theme(
#     legend.position = "bottom"
#   )






