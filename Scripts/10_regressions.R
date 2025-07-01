# Description
# Regressions - Xolani Sibande (June 2025)
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

options(scien = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_combined_data.rds"))

combined_temp_tbl <- combined_data |>
  pluck(1) |>
  mutate(year = as.character(year(date)))
combined_precip_tbl <- combined_data |>
  pluck(2) |>
  mutate(year = as.character(year(date)))

# first  regressions -----------------------------------------------------------
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       value:domestic +
                       value:foreign +
                       industry +
                        year")
ols <- function(formula, data, selected_country = NULL, selected_shock_type) {
  lm(formula,
     data |>
       filter(country == selected_country) |>
       filter(shock_type == selected_shock_type)) |>
  tidy() |>
    print(n = 100)
}

## Temp ---

ols(formula = formula,
    selected_country = "ZAF",
    selected_shock_type = "Agricultural Temperature Shock",
    data = combined_temp_tbl)
ols(formula = formula,
    selected_country = "ZAF",
    selected_shock_type = "Non-Agricultural Temperature Shock",
    data = combined_temp_tbl)

## Precip ---
ols(formula = formula,
    selected_country = "ZAF",
    selected_shock_type = "Agricultural Precipitation Shock",
    data = combined_precip_tbl)
ols(formula = formula,
    selected_country = "ZAF",
    selected_shock_type = "Non-Agricultural Precipitation Shock",
    data = combined_precip_tbl)

# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
