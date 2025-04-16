# Description
# CPI data - Xolani Sibande March 2025

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
    mutate(date = parse_date_time(date, orders = c("ym", "ymd")))
}

# Import -------------------------------------------------------------
replace_vector <- c("US")
names(replace_vector) <- c("U.S.")


sheet_list <- excel_sheets(here("Data", "CPI_NSA_2.xlsx"))

clean_sheet_list <- c(
  "Headline",
  "Food & Bev",
  "AlcBev",
  "Clothing",
  "Housing",
  "HH Goods",
  "Health",
  "Transport",
  "Communications",
  "Recreation",
  "Education",
  "Hotels",
  "Other",
  "Energy"
)

cpi_tbl <-
  set_names(sheet_list) |>
  map(~ read_xlsx(here("Data", "CPI_NSA_2.xlsx"), sheet = .x, na = "NA")) |>
  set_names(clean_sheet_list) |>
  map(~rename(.x,  "...1" = 1)) |>
  map(~ clean_cpi_data(.x)) |>
  bind_rows(.id = "category") |>
  dplyr::select(-x53, -eu_27) |>  # check Serina to fix this.
  pivot_longer(cols = -c(date, category), names_to = "country", values_to = "cpi") |>
  mutate(country = str_replace(country, "south_korea", "korea"),
         country = str_replace(country, "lao_pdr", "laos")) |>
  mutate(country_abs =
           countrycode::countrycode(country,
                                    origin = "country.name",
                                    destination = "iso3c",
                                    custom_match = c("kosovo" = "XKX" ))) |>
  relocate(date, .before = category) |>
  relocate(country_abs, .before = country) |>
    filter(date >= "1990-01-01" & date <= "2017-12-31") |>
  dplyr::select(-country) |>
  rename(country = country_abs) |>
  mutate(category = str_to_lower(category)) |>
  mutate(category = str_replace_all(category, "hh goods", "household_goods")) |>
  mutate(category = str_replace_all(category, "food & bev", "agrifood")) |>
  filter(category != "other") |>
  filter(category != "alcbev") |>
  filter(category != "recreation")

cpi_tbl |> glimpse()

# EDA ---------------------------------------------------------------
cpi_tbl |> group_by(country) |> skim()

# Plot ---------------------------------------------------------------
cpi_tbl |>
  filter(country %in% c("USA", "ZAF", "BRA", "CHN", "IND", "RUS")) |>
  ggplot(aes(x = date, y = cpi, color = category)) +
  geom_line() +
  labs(title = "CPI categories by country",
       x = "Date",
       y = "CPI") +
  facet_wrap(~country, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = pnw_palette("Sunset2", 14))


# Export ---------------------------------------------------------------
artifacts_cpi <- list (
  cpi_tbl = cpi_tbl
)

write_rds(artifacts_cpi, file = here("Outputs", "artifacts_cpi.rds"))
