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

# Import -------------------------------------------------------------
eora_ma_tbl <- read_rds(here("Outputs", "artifacts_combined_eoro26.rds")) |>
  pluck(1) |>
  relocate(column_country,.after = row_country) |>
  relocate(column_industry, .after = row_industry)


# EDA ---------------------------------------------------------------
eora_ma_tbl |> group_by(row_country) |> skim()

eora_ma_shares_tbl <- eora_ma_tbl |>
  group_by(date, row_country) |>
  mutate(
    row_sales = sum(ma)
  ) |>
  ungroup() |>
  group_by(date, column_industry) |>
  mutate(
    shares = ma / row_sales
  ) |>
  ungroup()

eora_ma_shares_tbl |>
  filter(column_country == "CHN" &
           column_industry == "health" &
           row_industry == "downstream") |>
  ggplot(aes(x = date, y = shares, col = row_country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Agrifood share of total sales in South Africa",
    x = "Date",
    y = "Share"
  ) +
  facet_wrap(~ row_country) +
  theme_minimal() +
  theme(legend.position = "none")
# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
