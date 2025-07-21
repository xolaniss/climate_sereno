# Description
# Calculating shares over time - Xolani Sibande May 2025
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

options(scipn = 999)
mem.maxVSize(100000)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
## Transaction data ------------------------------------
years <- c(1990:2017)
path_list_transaction <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/Eora_transaction/Eora26_{.}_bp_T.txt"
    )
  )

n_cores <- detectCores() - 1 # leave 1 core for OS
plan(multisession, workers = n_cores) # setting multisession plan

tic()
eora_transaction_list <-
  path_list_transaction |>
  future_map(~ read_delim(.x, col_names = FALSE, show_col_types = FALSE)) |>
  set_names(years)
toc()
plan(sequential)

## Final demand data ----
path_list_final_demand <-
  years |>
  map(
    ~ glue(
      "~/Papers/climate_sereno/Data/Eora26/Eora_final_demand/Eora26_{.}_bp_FD.txt"
    )
  )

plan(multisession, workers = n_cores)
tic()
eora_final_demand_list <-
  path_list_final_demand |>
  future_map(~ read_delim(.x, col_names = FALSE, show_col_types = FALSE)) |>
  set_names(years)

toc()
plan(sequential)

## Transaction names ---------------------------------
eora_transaction_names_tbl <-
  read_delim(here("Data", "Eora26", "indices", "labels_T.txt"),
             col_names = FALSE, show_col_types = FALSE) |>
  dplyr::select(-last_col()) |>
  slice(-4915) |>
  mutate(names = paste(X1, X4, sep = ".")) |>
  mutate(names = str_replace_all(names, " ", "_")) |>
  dplyr::select(X1, X4, names)

## final demand names ---------------------------------------
eora_final_demand_names_tbl <-
  read_delim(here("Data", "Eora26", "indices", "labels_FD.txt"),
             col_names = FALSE, show_col_types = FALSE) |>
  dplyr::select(-last_col()) |>
  # remove last 6 rows
  slice(-c(1135:1140)) |>
  mutate(names = paste(X2, X3, sep = ".")) |>
  mutate(names = paste(names, X4, sep = "_")) |>
  # remove spaces in names col
  mutate(names = str_replace_all(names, " ", "_")) |>
  dplyr::select(names)

# Clean final demand -----------------------------------------
eora_final_demand_tbl <-
  eora_final_demand_list |>
  map(~ .x |> dplyr::slice(-4915)) |>
  map(~ .x |> dplyr::select(-c(1135:1140))) |>
  map(~ .x |> dplyr::rename_with(~ eora_final_demand_names_tbl$names)) |>
  map(~ .x |> dplyr::mutate(industry = eora_transaction_names_tbl$X4)) |>
  map(~.x |> relocate(industry, .before = 1)) |>
  map(~ .x |> pivot_longer(
    cols = -industry,
    names_to = "col_name",
    values_to = "final_demand")) |>
  map(~ .x |>
        mutate(country = str_sub(col_name, 1, 3)) |>
        dplyr::select(-col_name)) |>
  map(~ .x |> group_by(country, industry) |>
        summarise(final_demand = sum(final_demand, na.rm = TRUE),
                  .groups = "drop")) |>
  bind_rows(.id = "year")

# Clean transactions  -------------------------------------------------------
eora_transaction_clean_list <-
  eora_transaction_list |>
  map(~ .x |> dplyr::select(-last_col())) |>
  map(~ .x |> dplyr::slice(-4915)) |>
  map(~ .x |> dplyr::rename_with(~ eora_transaction_names_tbl$names)) |>
  map(~ .x |> dplyr::mutate(row_industry = eora_transaction_names_tbl$X4)) |>
  map(~ .x |> dplyr::mutate(row_country = eora_transaction_names_tbl$X1)) |>
  map(~ .x |> dplyr::relocate(row_industry, .before = 1)) |>
  map(~ .x |> dplyr::relocate(row_country, .before = 1))

#  Long version of trans data and export ---------------------------------------
tic()
eora_transaction_clean_long_list <-
  eora_transaction_clean_list |>
  map(~ .x |> tidytable::pivot_longer(
    cols = -c(row_country, row_industry),
    names_to = "col_name",
    values_to = "values"
  ))
toc()

tic()
eora_transaction_clean_long_sep_list <-
  eora_transaction_clean_long_list |>
  map(~ .x |> tidytable::separate(
                 col = "col_name",
                 into = c("col_country", "col_industry"),
                 sep = "."
               ))
toc()

# Column industry aggregation -------------------------
tic()
eora_transaction_categories_list <-
  eora_transaction_clean_long_sep_list |>
  map(~ .x |> mutate(
    aggregated_sectors = tidytable::case_when(
      col_industry == "Agriculture" ~ "Agrifood",
      col_industry == "Food & Beverages" ~ "Agrifood",
      col_industry == "Fishing" ~ "Agrifood",
      col_industry == "Petroleum,_Chemical_and_Non-Metallic_Mineral_Products" ~ "Energy",
      col_industry == "Electricity,_Gas_and_Water" ~ "Energy",
      col_industry == "Textiles_and_Wearing_Apparel" ~ "Clothing",
      col_industry == "Construction" ~ "Housing",
      col_industry == "Maintanance_and_Repair" ~ "Housing",
      col_industry == "Wood_and_Paper" ~ "Household_goods",
      col_industry == "Electrical_and_Machinery" ~ "Household_goods",
      col_industry == "Other_Manufacturing" ~ "Household_goods",
      col_industry == "Transport" ~ "Transport",
      col_industry == "Transport_Equipment" ~ "Transport",
      col_industry == "Education,_Health_and_Other_Services" ~ "Education",
      # col_industry == "Education,_Health_and_Other_Services" ~ "Health",
      col_industry == "Hotels_and_Restraurants" ~ "Hotels",
      col_industry == "Post_and_Telecommunications" ~ "Communications",
      .default  =  "Drop"
    )
  )
)
toc()


tic()
eora_transactions_aggregated_tbl <-
  eora_transaction_categories_list |>
  map(
    ~ .x |>
      tidytable::filter(aggregated_sectors != "Drop") |>
      tidytable::select(-col_industry) |>
      tidytable::unite(
        "country_industry",
        c("col_country", "aggregated_sectors"),
        sep = "."
      ) |>
      tidytable::pivot_wider(
        id_cols = c("row_country", "row_industry"),
        values_fn = mean,
        names_from = country_industry,
        values_from = values)
  ) |>
  bind_rows(.id = "year")

toc()

# Combine transactions and final demand ---------------------------------------
combined_tbl <-
  eora_transactions_aggregated_tbl |>
  as_tibble() |>
  dplyr::left_join(
    eora_final_demand_tbl,
    by = c("year", "row_country" = "country", "row_industry" = "industry"))

# Calculate shares -------------------------------------------------------
combined_shares_tbl <-
  combined_tbl |>
  mutate(across(-c(year, row_country, row_industry), ~ .x / final_demand)) |>
  mutate(across(-c(year, row_country, row_industry), ~ round(.x, 3))) |>
  dplyr::select(-final_demand)

# Aggregating rows -----------------------------
tic()
combined_aggregated_shares_tbl <-
  combined_shares_tbl |>
  mutate(category =
           case_when(
             row_industry == "Agriculture" ~ "Agrifood",
             row_industry == "Food & Beverages" ~ "Agrifood",
             row_industry == "Fishing" ~ "Agrifood",
             .default  =  "Downstream"
           )
         ) |>
  relocate(category, .after = row_industry) |>
  dplyr::select(-row_industry) |>
  rename(row_industry = category) |>
  group_by(year, row_country, row_industry) |>
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(-c(year, row_country, row_industry), ~ round(.x, 3))) |>
  mutate(year = parse_date(year, format = "%Y"))
toc()

# Export ---------------------------------------------------------------
artifacts_eoro26 <- list (
  combined_aggregated_shares_tbl = combined_aggregated_shares_tbl
)
write_rds(artifacts_eoro26,
          file = here("Outputs", "artifacts_eoro26.rds"))
