# Description
# Calculating inflation rates by country by sector - Xolani Sibande April 2025

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
cpi_tbl <- read_rds(here("Outputs", "artifacts_cpi.rds")) |>
  pluck(1)

# EDA ---------------------------------------------------------------
cpi_tbl |> skim()

# Calculating inflation ---------------------------------------------------
inflation_rate_tbl <-
  cpi_tbl |>
  group_by(country, category) |>
  mutate(
    inflation_rate = 400*(log(cpi+ lag(cpi, 1) + lag(cpi, 2)) -
                          log(lag(cpi, 3) + lag(cpi, 4) + lag(cpi, 5))
                          ) # annualized and seasonal adjusted
  ) |>
  ungroup() |>
  dplyr::select(-cpi)

inflation_ratecpi_tbl |>
  skim()


# Visualisation ----------------------------------------------------------
inflation_rate_tbl|>
  filter(country == "GBR") |>
  ggplot(aes(x = date, y = inflation_rate, col = category)) +
  geom_line() +
  labs(
    title = "Inflation rate",
    subtitle = "Annualised inflation rate",
    x = "Date",
    y = "Inflation rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~category, ncol = 3, scales = "free_y")

# Export ---------------------------------------------------------------
artifacts_inflation_rate <- list (
  inflation_rate_tbl = inflation_rate_tbl
)

write_rds(artifacts_inflation_rate, file = here("Outputs",
                                                "artifacts_inflation_rate.rds"))
