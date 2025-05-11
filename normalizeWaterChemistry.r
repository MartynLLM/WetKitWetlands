normalize_water_chemistry_data <- function(db_path = "water_samples_auto.db") {
  # Load required packages
  library(RSQLite)
  
  # Connect to the database
  cat("Connecting to database:", db_path, "\n")
  con <- dbConnect(SQLite(), db_path)
  
  # Read the SQL script from a file or use the code above as a string
  sql_script <- "-- Insert the SQL code from above here"
  
  # Execute the SQL script
  cat("Normalizing chemistry data...\n")
  dbExecute(con, sql_script)
  
  # Verify the data was normalized correctly
  count <- dbGetQuery(con, "SELECT COUNT(*) FROM normalized_chemistry")
  cat("Number of rows in normalized table:", count[[1]], "\n")
  
  # Example query to verify data
  sample_data <- dbGetQuery(con, "
    SELECT * FROM normalized_chemistry 
    LIMIT 10
  ")
  cat("Sample of normalized data:\n")
  print(sample_data)
  
  # Disconnect from the database
  dbDisconnect(con)
  cat("Data normalization complete!\n")
}