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
mem.maxVSize(v = 100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_combined_data.rds"))

combined_temp_tbl <- combined_data |> pluck(1) |>
  mutate(year= as.character(year(date))) |>
  janitor::clean_names()
combined_precip_tbl <- combined_data |> pluck(2) |>
  mutate(year= as.character(year(date))) |>
  janitor::clean_names()

# Temp  regressions -----------------------------------------------------------
## Temp ---
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_temperature_shock +
                       foreign_agricultural_temperature_shock +
                       domestic_non_agricultural_temperature_shock +
                       foreign_non_agricultural_temperature_shock +
                      col_country +
                      year")

tic()
temp_reg_list <-
  combined_temp_tbl |>
  filter(!col_industry == "Communication") |>
  group_by(col_industry) |>
  group_map(safely( ~ {
    lm(formula = formula, data = .x) |>
      coeftest(vcov = vcovHC, type = "HC1") |>
      tidy() |>
      mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
        p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
      )))
  }))
toc()

## Precip ---
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_precipitation_shock +
                       foreign_agricultural_precipitation_shock +
                       domestic_non_agricultural_precipitation_shock +
                       foreign_non_agricultural_precipitation_shock +
                      col_country +
                      year")

tic()
precip_reg_list <-
  combined_precip_tbl |>
  filter(!col_industry == "Communication") |>
  group_by(col_industry) |>
  group_map(~ {
    lm(formula = formula, data = .x) |>
      coeftest(vcov = vcovHC, type = "HC1") |>
      tidy() |>
      mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
        p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
      )))
  })
toc()


# Export ---------------------------------------------------------------
artifacts_regressions <- list (
  temp_regressions = temp_reg_list,
  precip_regressions = precip_reg_list
)

write_rds(artifacts_regressions, file = here("Outputs", "artifacts_regressions.rds"))
