# Description
# Production network - Xolani Sibande April 2025

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
eora_ma_tbl <- read_rds(here("Outputs", "artifacts_combined_eoro26.rds")) |>
  pluck(1)

# Cleaning -----------------------------------------------------------------


# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
