# Description
# CPI data - Xolani Sibande March 2025

# TO DO -------------------------------------------------------------------
## calculate seasonal adjusted inflation rate
## fix 136 issue

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
library(countrycode)

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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
clean_cpi_data <- function(tbl, column = "x1") {
  tbl |>
    janitor::clean_names(replace = replace_vector) |>
    rename(date = {{ column }}) |>
    mutate(date = parse_date_time(date, orders = "ym"))
}

# Import -------------------------------------------------------------
replace_vector <- c("US")
names(replace_vector) <- c("U.S.")


sheet_list <- excel_sheets(here("Data", "CPI_NSA.xlsx"))[1:13]

clean_sheet_list <- c(
  "Headline (NSA)",
  "Food & Bev (NSA)",
  "AlcBev (NSA)",
  "Clothing (NSA)",
  "Housing (NSA)",
  "HH Goods (NSA)",
  "Health (NSA)",
  "Transport (NSA)",
  "Communications (NSA)",
  "Recreation (NSA)",
  "Education (NSA)",
  "Hotels (NSA)",
  "Other (NSA)"
)

cpi_tbl <-
  set_names(sheet_list) |>
  map(~ read_xlsx(here("Data", "CPI_NSA.xlsx"), sheet = .x, na = "NA")) |>
  set_names(clean_sheet_list) |>
  map(~rename(.x,  "...1" = 1)) |>
  map(~ clean_cpi_data(.x)) |>
  bind_rows(.id = "category") |>
  dplyr::select(-x136) |>  # check Serina to fix this.
  pivot_longer(cols = -c(date, category), names_to = "country", values_to = "cpi") |>
  mutate(country = str_replace(country, "south_korea", "korea"),
         country = str_replace(country, "lao_pdr", "laos")) |>
  mutate(country_abs =
           countrycode::countrycode(country,
                                    origin = "country.name",
                                    destination = "iso3c",
                                    custom_match = c("kosovo" = "XKX"))) |>
  relocate(date, .before = category) |>
  relocate(country_abs, .before = country) |>
  filter(date >= "1980-01-01")

cpi_tbl |> glimpse()

# EDA ---------------------------------------------------------------
cpi_tbl |> group_by(category) |> skim()

# Export ---------------------------------------------------------------
artifacts_cpi <- list (
  cpi_tbl = cpi_tbl
)

write_rds(artifacts_cpi, file = here("Outputs", "artifacts_cpi.rds"))
