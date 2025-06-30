# Description
# Calculating network shocks - Xolani Sibande May 2025
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(dtplyr)
library(arrow)
library(data.table)
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

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

options(scipen = 999)
mem.maxVSize(100000)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
sec_anomaly <- function(data, anomaly, source_sector) {
  tbl <-
    data |>
    filter(row_industry == source_sector) |>
    left_join(climate_shocks |>
                dplyr::select(date, country, {{anomaly}}),
              by = c("row_country" = "country", "date")) |>
    mutate(
      across(contains("."), ~ .x * {{anomaly}})) |>
    dplyr::select(- {{anomaly}}) |>
    tidytable::pivot_longer(
      cols = -c(date, row_country, row_industry),
      names_to = "country.industry",
      values_to = "value"
    ) |>
    tidytable::separate(col = "country.industry",
                        into = c("col_country", "col_industry"),
                        sep = ".") |>
    relocate(c("col_country", "col_industry"), .before = "value") |>
    mutate(domestic = ifelse(row_country == col_country, 1, 0)) |>
    mutate(foreign= ifelse(row_country != col_country, 1, 0)) |>
    mutate(value = as.numeric(value)) |>
    dplyr::select(-row_country, -row_industry) |>
    as_tibble()
  gc()
  return(tbl)
}
multi_sec_anomaly <- function(args, list_names) {
  args |>
    set_names(list_names) |>
    map(
      ~ sec_anomaly(
        input_output_tbl,
        !!sym(.x[1]),
        .x[2]
      )
    )
}

# Import -------------------------------------------------------------
input_output_tbl <- read_rds(here("Outputs",
                                  "artifacts_monthly_production_network.rds")) |>
  pluck(1)

climate_shocks <- read_rds(here("Outputs", "artifacts_climate_shocks.rds")) |>
  pluck(1)

# Calculating network shocks ---------------------------------------------------
## Shocks ---------------------------------------------------
list_names <- c("agric_temp_tbl",
                "non_agric_temp_tbl",
                "agric_precip_tbl",
                "non_agric_precip_tbl")


args_list <- list(
  c("land_weighted_temp_anomaly", "Agrifood"),
  c("population_weighted_temp_anomaly", "Downstream"),
  c("land_weighted_precip_anomaly", "Agrifood"),
  c("population_weighted_precip_anomaly", "Downstream")
)

tic()
shocks_list <- multi_sec_anomaly(args_list, list_names)
toc()

# Visualization example ---------------------------------------------------
climate_shocks |>
  filter(country == "USA") |>
  ggplot(aes(x = date, y = land_weighted_temp_anomaly)) +
  geom_line() +
  theme_minimal()

shocks_list |>
  pluck("non_agric_temp_tbl") |>
  filter(col_country == "USA" & domestic == 1) |>
  drop_na() |>
  arrange(date) |>
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(col = col_industry)) +
  facet_wrap(~col_industry, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 8)
  ) +
  scale_color_manual(
    values = pnw_palette("Sunset2", 9)
  )

# Export ---------------------------------------------------------------
artifacts_network_shocks <- list (
  shocks_list = shocks_list
)

write_rds(artifacts_network_shocks,
          file = here("Outputs", "artifacts_network_shocks.rds"))

