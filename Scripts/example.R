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


tic()
object <- terra::rast(here::here("API", "2m_temp_2023.nc"))
object_tbl <- terra::as.data.frame(object, xy = TRUE) %>% as_tibble()
toc()
object_tbl %>% write_csv(here::here("Data", "2m_temp_2023.csv"))
object_tbl %>% write_rds(here::here("Data", "2m_temp_2023.rds"))

terra::describe(here::here("API", "2m_temp_2023.nc"))
data_tbl <- terra::as.data.frame(data, xy = TRUE)
data_tbl %>% as_tibble()

glimpse(data_tbl)

terra::summary()
  dplyr::select(t2m)
  slice(index = 1, along = "valid_time") %>%
  plot()

library(sf)
library(stars)
library(tidyverse)
library(raster)
library(terra)

object <-   stars::read_stars(
  here::here("Data", "landscan-global-2022-assets", "landscan-global-2022-colorized.tif"), proxy = T)
object <- setNames(object, c("population"))
str(stars::st_dimensions(object))
stars::st_get_dimension_values(object, "x") %>%  length()
stars::st_get_dimension_values(object, "y") %>%  length()
plot(object, axes = TRUE)

describe(here::here("Data", "landscan-global-2022-assets", "landscan-global-2022-colorized.tif"))
data <- terra::rast(here::here("Data", "landscan-global-2022-assets", "landscan-global-2022.tif"))


plot(data)
terra::summary(data)
terra::summary(values(data))
data_tbl <- terra::as.data.frame(data, xy = TRUE)


terra::describe(terra::rast(here::here("Data", "spei01.nc")))
data <- terra::rast(here::here("Data", "spei48.nc"))
terra::summary(data)
terra::plot(data)


data %>% terra::plot()
data_tbl <- terra::as.data.frame(data, xy = TRUE) %>% as_tibble()
str(data_tbl)
data_tbl %>% head()
data_tbl %>% write_csv(here::here("Data", "spei48.csv"))
data_tbl %>% write_rds(here::here("Data", "spei48.rds"))



st_apply(object[,,,1], c("x", "y"), mean)

to_data_frame <- function(data, i){
  temp <- as.data.frame(data[,,,i], xy = TRUE) %>% as_tibble()

  return(temp)
}


registerDoFuture()

n_cores <- parallel::detectCores()
plan(
  strategy = cluster,
  workers = parallel::makeCluster(n_cores)
)


tic()
  object_tbl <- as.data.frame(object[,,,1], xy = TRUE) %>% as_tibble()
  object_2_tbl <- as.data.frame(object[,,,2], xy = TRUE) %>% as_tibble()
data <-   object_tbl %>%
    left_join(object_2_tbl, by = c("x", "y"))
toc()


plan(sequential)
tic()
f <- future({
  object_tbl <- as.data.frame(object[,,,1], xy = TRUE) %>% as_tibble()
  object_2_tbl <- as.data.frame(object[,,,2], xy = TRUE) %>% as_tibble()
  object_tbl %>%
    left_join(object_2_tbl, by = c("x", "y"))
}, lazy = TRUE)

y <- value(f)
toc()




object_tbl %>% as_tibble()



object.size(object)



set.seed(115517)
pts <- st_bbox(object) %>%  st_as_sfc()
st_sample(pts, size = 100000, force = TRUE)
pts <- st_sample(pts, size = 1, force = TRUE)

st_extract(object, pts)
str(pts)
pts %>% plot()

x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,10),c(0,90),c(0,0))))) # NOT long/lat:
plot(x)
p_exact = st_sample(x, 10, exact = TRUE)
p_exact %>% plot()
aperm(object, c(2, 1))



pts <- sf::st_bbox(object) %>% st_as_sfc() %>% st_sample(2, force = TRUE)



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






