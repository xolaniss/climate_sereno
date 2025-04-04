# Description
# cleaning Eora26 data - Xolani Sibande April 2025
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

# parallel processing
library(furrr)
library(parallel)
library(tictoc)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
years <- c(1990:2017)
path_list <-
  years |>
  map(
    ~ glue(
      "/Users/xolanisibande-dev/Papers/climate_sereno/Data/Eora26/Eora26_{.}_bp_T.txt"
    )
  )

n_cores <- detectCores() - 1
plan(multisession, workers = n_cores) # setting multisession plan

tic()
eora_data <-
  path_list |>
  future_map(
    ~ read_delim(
      .x,
      col_names = FALSE,
      show_col_types = FALSE
    )
  ) |>
  set_names(years)
toc()


# Cleaning ----------------------------------------------------------------
industry_names_vec <- c("Agriculture", "Fishing", "Mining and Quarrying",
               "Food & Beverages", "Textiles and Wearing Apparel",
               "Wood and Paper", "Petroleum, Chemical and Non-Metallic Mineral Products",
               "Metal Products", "Electrical and Machinery", "Transport Equipment",
               "Other Manufacturing", "Recycling", "Electricity, Gas and Water",
               "Construction", "Maintenance and Repair", "Wholesale Trade",
               "Retail Trade", "Hotels and Restraurants", "Transport",
               "Post and Telecommunications",
               "Financial Intermediation and Business Activities",
               "Public Administration", "Education, Health and Other Services",
               "Private Households", "Others", "Re-export & Re-import")

country_names_vec <- c("AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "ABW", "AUS",
              "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN",
              "BMU", "BTN", "BOL", "BIH", "BWA", "BRA", "VGB", "BRN", "BGR", "BFA",
              "BDI", "KHM", "CMR", "CAN", "CPV", "CYM", "CAF", "TCD", "CHL", "CHN",
              "COL", "COG", "CRI", "HRV", "CUB", "CYP", "CZE", "CIV", "PRK", "COD",
              "DNK", "DJI", "DOM", "ECU", "EGY", "SLV", "ERI", "EST", "ETH", "FJI",
              "FIN", "FRA", "PYF", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRL",
              "GTM", "GIN", "GUY", "HTI", "HND", "HKG", "HUN", "ISL", "IND", "IDN",
              "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN",
              "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU",
              "LUX", "MAC", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MRT", "MUS",
              "MEX", "MCO", "MNG", "MNE", "MAR", "MOZ", "MMR", "NAM", "NPL", "NLD",
              "ANT", "NCL", "NZL", "NIC", "NER", "NGA", "NOR", "PSE", "OMN", "PAK",
              "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "KOR", "MDA",
              "ROU", "RUS", "RWA", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC",
              "SLE", "SGP", "SVK", "SVN", "SOM", "ZAF", "SDS", "ESP", "LKA", "SUD",
              "SUR", "SWZ", "SWE", "CHE", "SYR", "TWN", "TJK", "THA", "MKD", "TGO",
              "TTO", "TUN", "TUR", "TKM", "USR", "UGA", "UKR", "ARE", "GBR", "TZA",
              "USA", "URY", "UZB", "VUT", "VEN", "VNM", "YEM", "ZMB", "ZWE")

industry_vec <- rep(industry_names_vec, 189)
country_vec <- rep(country_names_vec, each = 26)

country_industry_names_vec <- paste(country_vec, industry_vec, sep = ",")
length(country_industry_names_vec)

tic()
eora_data_cleaned_tbl <-
  eora_data |>
  future_map(~ {
    .x <- .x[-4915, -4915]
    names(.x) <- country_industry_names_vec
    return(.x)
  }) |>
  bind_rows(.id = "year")
toc()
plan(sequential)
# Export ---------------------------------------------------------------
artifacts_combined_eoro26 <- list (
  eora_data_cleaned_tbl = eora_data_cleaned_tbl,
  country_names_vec = country_names_vec,
  industry_names_vec = industry_names_vec,
  country_industry_names_vec = country_industry_names_vec
)

write_rds(artifacts_combined_eoro26,
          file = here("Outputs", "artifacts_combined_eoro26.rds"))




