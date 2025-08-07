# Description
# standardised shocks - Xolani Sibande August 2025
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "reg.R"))
shock_standardise <- function(list, type) {
  list |>
    map(~ {
      .x |>
        group_by(col_industry) |>
        mutate(across(
          .cols = contains(type),
          .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
        ))
    })
}

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_baseline_reg_data.rds"))
temp_list <- combined_data |> pluck(1)
precip_list <- combined_data |> pluck(2)
industry_names <- combined_data |> pluck(3)


# Shock standardisation -----------------------------------------------------------------
standardised_temp_list <-
  temp_list |>
  shock_standardise(type = "temperature")

standardised_precip_list <-
  precip_list |>
  shock_standardise(type = "precipitation")

# standardised shock regressions ------------------------------------------------
## Temp formula -----
formula <- as.formula(
  "inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_temperature_shock +
                       foreign_agricultural_temperature_shock +
                       domestic_non_agricultural_temperature_shock +
                       foreign_non_agricultural_temperature_shock +
                      col_country +
                      year"
)

## Run standardised temp shocks regression ----
tic("standardised temp regressions")
temp_reg_list <- temp_list |> map( ~ reg(.x))
toc()

## Precip formula -----
formula<- as.formula(
  "inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_precipitation_shock +
                       foreign_agricultural_precipitation_shock +
                       domestic_non_agricultural_precipitation_shock +
                       foreign_non_agricultural_precipitation_shock +
                      col_country +
                      year"
)

## Run standardised precip shocks regression ----
tic("standardised precip regressions")
precip_reg_list <- precip_list |> map( ~ reg(.x))
toc()


# Export ---------------------------------------------------------------
artifacts_standardised_shocks <- list (
  standardised_temp_list = standardised_temp_list,
  standardised_precip_list = standardised_precip_list,
  temp_reg_list = temp_reg_list,
  precip_reg_list = precip_reg_list
)

write_rds(artifacts_standardised_shocks, file =
            here("Outputs", "artifacts_standardised_shocks.rds"))


