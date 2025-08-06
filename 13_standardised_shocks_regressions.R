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

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_baseline_reg_data.rds"))
temp_list <- combined_data |> pluck(1)
precip_list <- combined_data |> pluck(2)
industry_names <- combined_data |> pluck(3)



# Cleaning -----------------------------------------------------------------


# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


