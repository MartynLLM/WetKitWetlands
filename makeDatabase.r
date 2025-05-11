# Load required packages
library(RSQLite)
library(readr)
library(dplyr)

create_water_database <- function(csv_path = "ForClaude.csv", db_path = "water_samples.db") {
  # Read the CSV file with error handling
  tryCatch({
    cat("Reading CSV file:", csv_path, "\n")
    water_data <- read_csv(csv_path, show_col_types = FALSE)
    
    # Print column names to verify
    cat("Detected column names:\n")
    print(names(water_data))
    
    # Clean column names if needed (replace spaces, special characters)
    names(water_data) <- gsub(" ", "_", names(water_data))
    names(water_data) <- gsub("[^A-Za-z0-9_]", "", names(water_data))
    
    cat("CSV loaded successfully with", nrow(water_data), "rows and", ncol(water_data), "columns\n")
    
    # Create a SQLite database connection
    cat("Creating SQLite database:", db_path, "\n")
    con <- dbConnect(SQLite(), db_path)
    
    # Create table with dynamically generated schema based on actual columns
    cat("Creating water_samples table...\n")
    
    # Write table with overwrite option to handle column discrepancies
    dbWriteTable(con, "water_samples", water_data, overwrite = TRUE)
    
    # Create an index on SampleID for faster querying
    cat("Creating index on SampleID...\n")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sample_id ON water_samples(SampleID)")
    
    # Verify the data was written correctly
    count <- dbGetQuery(con, "SELECT COUNT(*) FROM water_samples")
    cat("Number of rows in database:", count[[1]], "\n")
    
    # Get a sample of data to verify
    sample_data <- dbGetQuery(con, "SELECT * FROM water_samples LIMIT 5")
    cat("Sample of data in database:\n")
    print(sample_data)
    
    # Disconnect from the database
    dbDisconnect(con)
    cat("Database creation complete!\n")
    
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
    
    # Try an alternative approach if the first one fails
    cat("Attempting alternative approach with read.csv...\n")
    
    water_data <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = TRUE)
    cat("CSV loaded with alternative method.\n")
    
    # Connect to the database
    con <- dbConnect(SQLite(), db_path)
    
    # Write table with overwrite option
    dbWriteTable(con, "water_samples", water_data, overwrite = TRUE)
    
    # Create index
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_sample_id ON water_samples(SampleID)")
    
    # Disconnect
    dbDisconnect(con)
    cat("Database creation complete with alternative method!\n")
  })
}

# Call the function to create the database
create_water_database()
