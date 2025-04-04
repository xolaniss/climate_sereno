# Process each year from 1990 to 2017
years <- 1990:2017

for (year in years) {
  # Get the T matrix for this year
  data <- get(paste0("T", year))
  
  print(paste("Processing year:", year))
  
  # Calculate column sums for all columns except country and industry (columns 1 and 2)
  total_input <- colSums(data[, 3:ncol(data)])
  
  # Convert to data frame and transpose to make it a row
  total_input_df <- as.data.frame(t(total_input))
  
  # Save as a new variable with the name totalinput_year
  assign(paste0("totalinput_", year), total_input_df)
  
  # Create directory if it doesn't exist
  dir.create("total_inputs", showWarnings = FALSE)
  
  # Save to CSV file
  write.csv(total_input_df, 
            file = paste0("total_inputs/totalinput_", year, ".csv"),
            row.names = FALSE)
  
  print(paste("Created totalinput_", year, "and saved to CSV"))
}

print("\nCompleted! Total inputs for each year are saved in:")
print("1. Variables named 'totalinput_YEAR'")
print("2. CSV files named 'totalinput_YEAR.csv' in the total_inputs directory")

####### MA_shares
# Process each year from 1990 to 2017
years <- 1990:2017

for (year in years) {
  print(paste("\nProcessing year:", year))
  
  # Get the MA data and total input data for this year
  ma_data <- get(paste0("MA_", year))
  total_input <- get(paste0("totalinput_", year))
  
  # Create a new dataframe for the shares with same structure as MA data
  shares <- ma_data
  
  # For each column, divide MA values by corresponding total input
  for (col in names(ma_data)) {
    if (col %in% names(total_input)) {
      shares[[col]] <- ma_data[[col]] / total_input[[col]]
    } else {
      warning(paste("Column", col, "not found in total inputs for year", year))
    }
  }
  
  # Save as new variable
  assign(paste0("MAshares_", year), shares)
  
  # Create directory if it doesn't exist
  dir.create("ma_shares", showWarnings = FALSE)
  
  # Save to file
  write.csv(shares, 
            file = paste0("ma_shares/MAshares_", year, ".csv"),
            row.names = FALSE)
  
  print(paste("Created MAshares_", year, "and saved to CSV"))
}

print("\nCompleted! MA shares for each year are saved in:")
print("1. Variables named 'MAshares_YEAR'")
print("2. CSV files named 'MAshares_YEAR.csv' in the ma_shares directory")
