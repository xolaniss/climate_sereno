# Function to calculate moving average for a specific year
calculate_ma_for_year <- function(target_year) {
  # Get list of years to include in moving average (2 years before and 2 years after)
  years <- (target_year-2):(target_year+2)
  years <- years[years >= 1990 & years <= 2017]  # Ensure we stay within bounds
  
  print(paste("\n=== Processing moving average for year:", target_year, "==="))
  print(paste("Including years:", paste(years, collapse=", ")))
  
  # Get the target year data
  target_data <- get(paste0("T", target_year))
  total_cells <- (ncol(target_data) - 2) * nrow(target_data)  # Exclude first 2 columns
  print(paste("Total numeric cells to process:", total_cells))
  
  # Create empty result data frame with same structure as target data
  result <- target_data
  result[,3:ncol(result)] <- NA  # Clear numeric values but keep structure
  
  # For each row and each numeric column, calculate moving average
  cells_processed <- 0
  start_time <- Sys.time()
  
  for(row in 1:nrow(target_data)) {
    if(row %% 10 == 0) {
      elapsed <- difftime(Sys.time(), start_time, units="secs")
      pct_complete <- cells_processed / total_cells * 100
      print(sprintf("Row %d/%d (%.1f%% complete)", 
                    row, nrow(target_data), pct_complete))
    }
    
    for(col in 3:ncol(target_data)) {
      values <- numeric()
      for(year in years) {
        # Get data for this year
        year_data <- get(paste0("T", year))
        
        # Only include value if row and column exist and match current position
        if(row <= nrow(year_data) && 
           col <= ncol(year_data) && 
           all(year_data[row, 1:2] == target_data[row, 1:2])) {
          values <- c(values, year_data[row, col])
        }
      }
      
      # Calculate moving average if we have any values
      if(length(values) > 0) {
        result[row, col] <- mean(values, na.rm = TRUE)
      }
      
      cells_processed <- cells_processed + 1
    }
  }
  
  # Create output directory if it doesn't exist
  dir.create("moving_averages", showWarnings = FALSE)
  
  # Write result to new file
  output_file <- file.path("moving_averages", paste0("MA_", target_year, ".txt"))
  write.table(result, 
              file = output_file, 
              sep = "\t",
              row.names = FALSE,
              quote = FALSE,
              na = "",
              col.names = TRUE)
  
  print(paste("Wrote moving averages to", output_file))
  
  # Also store in memory with new name
  assign(paste0("MA_", target_year), result, envir = .GlobalEnv)
  
  return(result)
}

# Process all years from 1990 to 2017
print("Starting to calculate moving averages for all years...")
print("Note: This will take several minutes to complete.")
print("Progress updates will be shown every 10 rows")
ma_results <- list()

for(year in 1990:2017) {
  ma_results[[as.character(year)]] <- calculate_ma_for_year(year)
}

print("\nAll moving averages calculated successfully!")
print("Results are saved in the 'moving_averages' directory with prefix 'MA_'")

#########Filter <USD1m

# Process each year from 1990 to 2017
years <- 1990:2017

for (year in years) {
  print(paste("\nProcessing year:", year))
  
  # Get the MA data for this year
  ma_data <- get(paste0("MA_", year))
  
  # Replace values < 1 with 0 in all numeric columns
  # Skip first two columns if they are country and industry
  for (col in names(ma_data)) {
    ma_data[[col]][ma_data[[col]] < 1] <- 0
  }
  
  # Save back to the original variable
  assign(paste0("MA_", year), ma_data)
  
  # Save to file (overwrite existing MA file)
  write.csv(ma_data, 
            file = paste0("moving_averages/MA_", year, ".txt"),
            row.names = FALSE,
            quote = FALSE)
  
  print(paste("Updated MA_", year, "with small values replaced"))
}

print("\nCompleted! All MA files have been updated:")
print("1. Variables named 'MA_YEAR' in memory")
print("2. Files named 'MA_YEAR.txt' in the moving_averages directory")

