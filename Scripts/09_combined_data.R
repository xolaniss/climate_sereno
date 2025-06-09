# Description
# combining climate and sectoral data - Xolani Sibande April 2025
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

# Parallel processing
library(furrr)
library(parallel)
library(tictoc)

mem.maxVSize(100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import and clean -------------------------------------------------------------
## Network shocks data ---------------------------------------------------------------
network_shocks <-
  read_rds(here("Outputs", "artifacts_network_shocks.rds"))

agric_temp_shocks_tbl <- network_shocks |> pluck(1,1)
non_agric_temp_shocks_tbl <- network_shocks |> pluck(1,2)

agric_precip_shocks_tbl <- network_shocks |> pluck(1,3)
non_agric_precip_shocks_tbl <- network_shocks |> pluck(1,4)

## Inflation data ---------------------------------------------------------------
inflation_rate_tbl <- read_rds(here("Outputs", "artifacts_inflation_rate.rds")) |>
  pluck(1) |>
  mutate(
    date = as.Date(date)
  ) |>
  rename(
    industry = category
  ) |>
  mutate(industry = str_to_title(industry)) |>
  filter(date >= "2000-01-01" & industry != "Headline")

inflation_rate_tbl |>
  filter(country == "ZAF" & industry == "Transport" & date >= "2016-01-01")

# Combining data ---------------------------------------------------------------
combined_temp_data_tbl <-
  agric_temp_shocks_tbl |>
  bind_rows(non_agric_temp_shocks_tbl, .id = "shock_type") |>
  mutate(
    shock_type = case_when(
      shock_type == 1 ~ "Agricultural Temperature Shock",
      shock_type == 2 ~ "Non-Agricultural Temperature Shock"
    )
  ) |>
  relocate(
    date,
    .before = shock_type
  ) |>
  rename(country = dest_country,
         industry = dest_industry) |>
  left_join(inflation_rate_tbl,
            by = join_by(date,
                        country,
                        industry),
            relationship = "many-to-many")

combined_precip_data_tbl <-
  agric_precip_shocks_tbl |>
  bind_rows(non_agric_precip_shocks_tbl, .id = "shock_type") |>
  mutate(
    shock_type = case_when(
      shock_type == 1 ~ "Agricultural Precipitation Shock",
      shock_type == 2 ~ "Non-Agricultural Precipitation Shock"
    )
  ) |>
  relocate(
    date,
    .before = shock_type
  ) |>
  rename(country = dest_country,
         industry = dest_industry) |>
  left_join(inflation_rate_tbl,
            by = join_by(date,
                        country,
                        industry),
            relationship = "many-to-many")




combined_temp_data_tbl |> filter(
  country == "ZAF" &
    foreign == 1 &
    date >= "2016-01-01" &
    shock_type == "Non-Agricultural Temperature Shock"
)

combined_precip_data_tbl |> filter(
  country == "ZAF" &
    domestic == 1 &
    date >= "2016-01-01" &
    shock_type == "Agricultural Precipitation Shock"
)


# Export ---------------------------------------------------------------
artifacts_combined_data <- list (
  combined_temp_data_tbl = combined_temp_data_tbl,
  combined_precip_data_tbl = combined_precip_data_tbl
)

write_rds(artifacts_combined_data, file = here("Outputs",
                                               "artifacts_combined_data.rds"))
