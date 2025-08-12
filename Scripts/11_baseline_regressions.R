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

# loading
library(qs2)

options(scien = 999)
mem.maxVSize(v = 100000)

#Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
reg <- function(data) {
  data |>
    filter(!col_industry %in% c("Communication", "Education")) |>
    group_by(col_industry) |>
    group_map( ~ {
      lm(formula = formula, data = .x) |>
        coeftest(vcov = vcovHC, type = "HC1") |>
        tidy() |>
        mutate(stars = ifelse(p.value < 0.01, "***", ifelse(
          p.value < 0.05, "**", ifelse(p.value < 0.1, "*", "")
        )))
    }) |> set_names(industry_names)
}
extract_level_data <- function(data, list_element) {
  data |>
    pluck(list_element) |>
    mutate(year = as.character(year(date))) |>
    janitor::clean_names()
}

# Import -------------------------------------------------------------
combined_data <- qd_read(here("Outputs", "artifacts_combined_baseline_data.qs2"))

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
    # "Education", # same as health
    "Energy",
    "Health",
    "Hotels",
    "Household Goods",
    "Housing",
    "Transport"
  )

## Temp formula ----
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

## Data list for mapping ----
temp_list <- list(
  temp_baseline_5_tbl = temp_baseline_5_tbl,
  temp_baseline_10_tbl = temp_baseline_10_tbl,
  temp_baseline_90_tbl = temp_baseline_90_tbl,
  temp_baseline_95_tbl = temp_baseline_95_tbl
)


## Temp regression ----
tic("Regressions at different temperature thresholds")
temp_reg_list <- temp_list |> map( ~ reg(.x))
toc()

# Precip regressions -----------------------------------------------------------

## Precip formula ---
formula <- as.formula(
  "inflation_rate ~
                       lag(inflation_rate) +
                       domestic_agricultural_precipitation_shock +
                       foreign_agricultural_precipitation_shock +
                       domestic_non_agricultural_precipitation_shock +
                       foreign_non_agricultural_precipitation_shock +
                      col_country +
                      year"
)

## Data list for mapping ----
precip_list <- list(
  precip_baseline_5_tbl = precip_baseline_5_tbl,
  precip_baseline_10_tbl = precip_baseline_10_tbl,
  precip_baseline_90_tbl = precip_baseline_90_tbl,
  precip_baseline_95_tbl = precip_baseline_95_tbl
)

## Precip regressions ----
tic("Regressions at different precipitation thresholds")
precip_reg_list <- precip_list |> map( ~ reg(.x))
toc()


# Export -------------------------------------------------------------
artifacts_regressions <- list(
  temp_regressions = temp_reg_list,
  precip_regressions = precip_reg_list
  )

write_rds(artifacts_regressions,
          here("Outputs", "artifacts_baseline_regressions.rds"))

artifacts_reg_data <- list(
  temp_list = temp_list,
  precip_list = precip_list,
  industry_names = industry_names
  )

qd_save(artifacts_reg_data, "Outputs/artifacts_baseline_reg_data.qs2")
