# Description
# Controls preparation - Xolani Sibande August 2025
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
controls_tbl <- read_excel(here("Data", "Controls.xlsx")) |>
  rename(country = Country_code)

# Hemisphere ----- Source ?
hemisphere_tbl <-
  controls_tbl |>
  dplyr::select(country, hemisphere) |>
  mutate(hemisphere_classification = if_else(hemisphere == 1, "northern", "southern"))

# Income classification ----- # Source?
income_classification_tbl <-
  controls_tbl |>
  dplyr::select(country, classification) |>
  mutate(classification = as.numeric(classification)) |>
  drop_na() |>  # dropped two non-numeric values
  mutate(income_classification = if_else(
    classification == 1,
    "Developing",
    if_else(
      classification == 2,
      "Emerging",
      if_else(classification == 3, "Advanced", NA)
    )
  ))

# Inflation targeting classification ----- # Source?
inflation_targeting_tbl <-
  controls_tbl |>
  dplyr::select(country, target_start) |>
  mutate(inflation_targeting = if_else(is.na(target_start), "No", "Yes")) # includes exchange rate pegs


# Export ---------------------------------------------------------------
artifacts_controls_prep <- list (
  hemisphere_tbl = hemisphere_tbl,
  income_classification_tbl = income_classification_tbl,
  inflation_targeting_tbl = inflation_targeting_tbl
)

write_rds(artifacts_controls_prep,
          file = here("Outputs", "artifacts_controls_prep.rds"))
