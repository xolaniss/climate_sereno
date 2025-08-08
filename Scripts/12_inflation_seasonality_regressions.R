# Description
# inflation seasonality regressions - Xolani Sibande August 2025

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

# loading data
library(qs2)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "reg.R"))

# Import -------------------------------------------------------------
combined_data <- qd_read(here("Outputs", "artifacts_baseline_reg_data.qs2"))
temp_list <- combined_data |> pluck(1)
precip_list <- combined_data |> pluck(2)
industry_names <- combined_data |> pluck(3)

# Inflation seasonality (3 lags) regressions ------
## Temp inflation seasonality formula -----
formula <- as.formula(
  "inflation_rate ~
                       lag(inflation_rate) +
                       lag(inflation_rate, 2) +
                       lag(inflation_rate, 3) +
                       domestic_agricultural_temperature_shock +
                       foreign_agricultural_temperature_shock +
                       domestic_non_agricultural_temperature_shock +
                       foreign_non_agricultural_temperature_shock +
                      col_country +
                      year"
) # degrees of freedom problems after 3 lags

## Run inflation seasonality and temp shock regression ----

tic("Temp regressions at different inflation seasonality lags")
temp_reg_list <- temp_list |> map( ~ reg(.x))
toc()


## precip inflation seasonality formula -----
formula <- as.formula(
  "inflation_rate ~
                       lag(inflation_rate) +
                       lag(inflation_rate, 2) +
                       lag(inflation_rate, 3) +
                       domestic_agricultural_precipitation_shock +
                       foreign_agricultural_precipitation_shock +
                       domestic_non_agricultural_precipitation_shock +
                       foreign_non_agricultural_precipitation_shock +
                      col_country +
                      year"
)

## Run inflation seasonality and precip shock regression ----
tic("Precip shock regressions at different inflation seasonality lags")
precip_reg_list <- precip_list |> map( ~ reg(.x))
toc()


# Export ---------------------------------------------------------------
artifacts_inflation_seasonality_regs <- list (
  temp_reg_list = temp_reg_list,
  precip_reg_list = precip_reg_list
)

write_rds(artifacts_inflation_seasonality_regs, file = here("Outputs", "artifacts_inflation_seasonality_regs.rds"))


