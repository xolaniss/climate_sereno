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
#Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

reg <- function(data){
data |>
  filter(!col_industry == "Communication") |>
  group_by(col_industry) |>
  group_map(~ {
    lm(formula = formula, data = .x) |>
      coeftest(vcov = vcovHC, type = "HC1") |>
      tidy() |>
      mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
        p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
      )))
      # set_names(industry_names)
  })
}

extract_level_data <- function(data, list_element){
  data |>
  pluck(list_element) |>
  mutate(year= as.character(year(date))) |>
  janitor::clean_names()
}

# Import -------------------------------------------------------------
combined_data <- read_rds(here("Outputs", "artifacts_combined_baseline_data.rds"))

# Extracting data at different levels ---------------------------------------
## Temp ----
temp_baseline_5_tbl <-
  combined_data |>
  extract_level_data(1)

temp_baseline_10_tbl <-
  combined_data |>
  extract_level_data(2)

temp_baseline_90_tbl <-
  combined_data |>
  extract_level_data(3)

temp_baseline_95_tbl <-
  combined_data |>
  extract_level_data(4)

## Precip ----
precip_baseline_5_tbl <-
  combined_data |>
  extract_level_data(5)

precip_baseline_10_tbl <-
  combined_data |>
  extract_level_data(6)

precip_baseline_90_tbl <-
  combined_data |>
  extract_level_data(7)

precip_baseline_95_tbl <-
  combined_data |>
  extract_level_data(8)


# Temp  regressions -----------------------------------------------------------
industry_names <-
  c(
  "Agrifood",
  "Clothing",
  "Education",
  "Energy",
  "Health",
  "Hotels",
  "Household_goods",
  "Housing",
  "Transport"
)

## Temp formula
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_temperature_shock +
                       foreign_agricultural_temperature_shock +
                       domestic_non_agricultural_temperature_shock +
                       foreign_non_agricultural_temperature_shock +
                      col_country +
                      year")

## Temp 5% level ---
tic("Regressions at 5% level")
temp_baseline_5_reg_list <-
  temp_baseline_5_tbl |>
  reg()
toc()

## Temp 10% level ---
tic("Regressions at 10% level")
temp_baseline_10_reg_list <-
  temp_baseline_5_tbl |>
  reg()
toc()

## Temp 90% level ---
tic("Regressions at 90% level")
temp_baseline_90_reg_list <-
  temp_baseline_5_tbl |>
  reg()
toc()

## Temp 95% level ---
tic("Regressions at 95% level")
temp_baseline_95_reg_list <-
  temp_baseline_95_tbl |>
  reg()
toc()

## Precip formula ---
formula <- as.formula("inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_precipitation_shock +
                       foreign_agricultural_precipitation_shock +
                       domestic_non_agricultural_precipitation_shock +
                       foreign_non_agricultural_precipitation_shock +
                      col_country +
                      year")

## 5% level precip ----
tic("Regressions at 5% level precip")
precip_baseline_5_reg_list <-
  precip_baseline_5_tbl |>
  reg()
toc()

## 10% level precip ----
tic("Regressions at 10% level precip")
precip_baseline_10_reg_list <-
  precip_baseline_5_tbl |>
  reg()
toc()

## 90% level precip ----
tic("Regressions at 90% level precip")
precip_baseline_90_reg_list <-
  precip_baseline_5_tbl |>
  reg()
toc()

## 95% level precip ----
tic("Regressions at 95% level precip")
precip_baseline_95_reg_list <-
  precip_baseline_95_tbl |>
  reg()
toc()


# Export -------------------------------------------------------------
artifacts_regressions <- list(
  temp_baseline_5_reg_list = temp_baseline_5_reg_list,
  temp_baseline_10_reg_list = temp_baseline_10_reg_list,
  temp_baseline_90_reg_list = temp_baseline_90_reg_list,
  temp_baseline_95_reg_list = temp_baseline_95_reg_list,
  precip_baseline_5_reg_list = precip_baseline_5_reg_list,
  precip_baseline_10_reg_list = precip_baseline_10_reg_list,
  precip_baseline_90_reg_list = precip_baseline_90_reg_list,
  precip_baseline_95_reg_list = precip_baseline_95_reg_list
)

write_rds(artifacts_regressions, here("Outputs", "artifacts_baseline_regressions.rds"))


