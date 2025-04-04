# Description

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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------


#########SECTORAL AGGREGATION OF ROWS/SUPPLIERS
# Define industries to aggregate
agrifood_industries <- c("Agriculture", "fishing", "Food & Beverages")
downstream_industries <- c(
  "Petroleum, Chemical, and Non-Metallic Mineral Products",
  "Electricity, Gas and Water",
  "Textiles and Wearing Apparel",
  "Construction",
  "Maintanance and Repair",
  "Wood and Paper",
  "Electrical and Machinery",
  "Other Manufacturing",
  "Transport Equipment",
  "Transport",
  "Education, Health and Other Services",
  "Post and Telecommunications",
  "Hotels and Restraurants"
)


# Function to process a single year
process_year <- function(year) {
  # Get the data object name
  data_name <- paste0("T", year)

  # Check if the data exists in the environment
  if(!exists(data_name)) {
    warning(paste("Data for year", year, "not found. Skipping..."))
    return(NULL)
  }

  # Get the data
  data <- get(data_name)
  print(paste("\n=== Processing year:", year, "==="))
  print("Original dimensions:")
  print(dim(data))

  # Store column names and classes to preserve format
  col_names <- names(data)
  col_classes <- sapply(data, class)

  # Create a new dataframe for the aggregated results
  aggregated_data <- data

  # Replace industries with their new categories
  aggregated_data$industry[aggregated_data$industry %in% agrifood_industries] <- "agrifood"
  aggregated_data$industry[aggregated_data$industry %in% downstream_industries] <- "downstream"

  # Remove rows that aren't in either category
  aggregated_data <- aggregated_data[aggregated_data$industry %in% c("agrifood", "downstream"), ]

  # For each country and new industry category, aggregate all numeric columns
  result <- aggregate(aggregated_data[, 3:ncol(aggregated_data)],
                      by = list(country = aggregated_data$country,
                                industry = aggregated_data$industry),
                      FUN = sum)

  # Sort results by country
  result <- result[order(result$country), ]

  # Ensure column names match original format
  names(result)[1:2] <- c("country", "industry")

  # Print final dimensions
  print("Final dimensions:")
  print(dim(result))
  print("First few rows:")
  print(head(result[, 1:min(5, ncol(result))]))  # Show first 5 columns or less

  # Write the results back to the original T-file format
  output_file <- paste0("T", year, ".txt")
  write.table(result,
              file = output_file,
              sep = "\t",
              row.names = FALSE,
              quote = FALSE,  # Don't quote strings
              na = "",       # Use empty string for NA values
              col.names = TRUE)
  print(paste("Finished writing results to", output_file))

  # Also assign the result back to the original T object in the environment
  assign(data_name, result, envir = .GlobalEnv)

  return(result)
}

# Process all years from 1990 to 2017
print("Starting to process all years from 1990 to 2017...")
results <- list()
for(year in 1990:2017) {
  results[[as.character(year)]] <- process_year(year)
}


###########
# Function to create aggregated columns for a country
create_aggregated_cols <- function(country, data) {
  # Create the new aggregated columns
  new_cols <- list()

  # 1. Agrifood (Agriculture + Fishing + Food & Beverages)
  agrifood_cols <- grep(paste0("^", country, "\\.(Agriculture|Fishing|Food...Beverages)$"),
                        names(data), value = TRUE)
  new_cols[[paste0(country, ".Agrifood")]] <- rowSums(data[, agrifood_cols, drop = FALSE])

  # 2. Energy (Petroleum... + Electricity...)
  energy_cols <- grep(paste0("^", country, "\\.(Petroleum..Chemical.and.Non.Metallic.Mineral.Products|Electricity..Gas.and.Water)$"),
                      names(data), value = TRUE)
  new_cols[[paste0(country, ".Energy")]] <- rowSums(data[, energy_cols, drop = FALSE])

  # 3. Housing (Construction + Maintenance)
  housing_cols <- grep(paste0("^", country, "\\.(Construction|Maintenance.and.Repair)$"),
                       names(data), value = TRUE)
  new_cols[[paste0(country, ".Housing")]] <- rowSums(data[, housing_cols, drop = FALSE])

  # 4. Household_goods (Wood + Electrical + Other Manufacturing)
  household_cols <- grep(paste0("^", country, "\\.(Wood.and.Paper|Electrical.and.Machinery|Other.Manufacturing)$"),
                         names(data), value = TRUE)
  new_cols[[paste0(country, ".Household_goods")]] <- rowSums(data[, household_cols, drop = FALSE])

  # 5. Transport (Transport Equipment + Transport)
  transport_cols <- grep(paste0("^", country, "\\.(Transport.Equipment|Transport)$"),
                         names(data), value = TRUE)
  new_cols[[paste0(country, ".Transport")]] <- rowSums(data[, transport_cols, drop = FALSE])

  # 6 & 7. Education and Health (duplicate Education, Health and Other Services)
  education_col <- grep(paste0("^", country, "\\.Education..Health.and.Other.Services$"),
                        names(data), value = TRUE)
  if(length(education_col) > 0) {
    new_cols[[paste0(country, ".Education")]] <- data[, education_col]
    new_cols[[paste0(country, ".Health")]] <- data[, education_col]
  }

  # 8. Communication (rename Post and Telecommunications)
  comm_col <- grep(paste0("^", country, "\\.Post.and.Telecommunications$"),
                   names(data), value = TRUE)
  if(length(comm_col) > 0) {
    new_cols[[paste0(country, ".Communication")]] <- data[, comm_col]
  }

  # 9. Clothing (rename Textiles and Wearing Apparel)
  clothing_col <- grep(paste0("^", country, "\\.Textiles.and.Wearing.Apparel$"),
                       names(data), value = TRUE)
  if(length(clothing_col) > 0) {
    new_cols[[paste0(country, ".Clothing")]] <- data[, clothing_col]
  }

  # 10. Hotels (rename Hotel and Restaurant)
  hotels_col <- grep(paste0("^", country, "\\.Hotels.and.Restraurants$"),
                     names(data), value = TRUE)
  if(length(hotels_col) > 0) {
    new_cols[[paste0(country, ".Hotels")]] <- data[, hotels_col]
  }

  return(as.data.frame(new_cols))
}

# Function to process a single year
process_year <- function(year) {
  # Get the data object name
  data_name <- paste0("T", year)

  # Check if the data exists in the environment
  if(!exists(data_name)) {
    warning(paste("Data for year", year, "not found. Skipping..."))
    return(NULL)
  }

  # Get the data
  data <- get(data_name)

  print(paste("\n=== Processing year:", year, "==="))
  print("Original dimensions:")
  print(dim(data))

  # Get unique countries
  countries <- unique(gsub("\\.(.*)", "", names(data)[-c(1,2)]))  # Exclude first two columns
  countries <- countries[countries != ""]

  # Start with the first two columns of original data
  result <- data[, 1:2]

  # Process each country
  for(country in countries) {
    new_cols <- create_aggregated_cols(country, data)
    result <- cbind(result, new_cols)
  }

  # Write the results back to the original T-file format
  output_file <- paste0("T", year, ".txt")
  write.table(result,
              file = output_file,
              sep = "\t",
              row.names = FALSE,
              quote = FALSE,  # Don't quote strings
              na = "",       # Use empty string for NA values
              col.names = TRUE)
  print(paste("Finished writing results to", output_file))

  # Also assign the result back to the original T object in the environment
  assign(data_name, result, envir = .GlobalEnv)

  return(result)
}

# Process all years from 1990 to 2017
print("Starting to process all years from 1990 to 2017...")
results <- list()
for(year in 1990:2017) {
  results[[as.character(year)]] <- process_year(year)
}



# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))
