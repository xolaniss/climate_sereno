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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_combined_data.rds"))

combined_temp_tbl <- combined_data |> pluck(1)
combined_precip_tbl <- combined_data |> pluck(2)

# Cleaning -----------------------------------------------------------------
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       value:domestic +
                       value:foreign")

lm(formula, data = combined_temp_tbl |>
     filter(shock_type == "Agricultural Temperature Shock")) |>
  tidy()

# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
