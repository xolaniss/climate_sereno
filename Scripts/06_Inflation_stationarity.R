# Description
# Inflation stationarity - Xolani Sibande July 2025
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
# Load the inflation rate data
inflation_rate_tbl <- read_rds(here("Outputs",
                                    "artifacts_inflation_rate.rds")) |>
  pluck(1)


# Stationary tests ---------------------------------------------------
stationarity_list <- plm::purtest(inflation_rate ~ 1, data = inflation_rate_tbl,
                                  index = c("country", "date"),
                                  test = "ips",
                                  pmax = 0,
                                  exo = "trend")


# Export ---------------------------------------------------------------
artifacts_inflation_stationarity <- list (
  stationarity_list = stationarity_list
)

write_rds(artifacts_inflation_stationarity,
          file = here("Outputs", "artifacts_inflation_stationarity.rds"))
