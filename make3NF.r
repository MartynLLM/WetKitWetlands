# Function to create 3NF transformation SQL and execute it
create_3nf_transformation <- function(db_path = "water_samples.db", output_sql_path = "3nf.sql") {
  library(RSQLite)
  library(DBI)
  
  # Connect to the database
  cat("Connecting to database:", db_path, "\n")
  con <- dbConnect(SQLite(), db_path)
  
  # Get the column names from the existing table
  columns <- dbListFields(con, "water_samples")
  cat("Found", length(columns), "columns in the water_samples table\n")
  
  # Identify metadata columns vs parameter columns
  # Assuming the first few columns are metadata and the rest are parameters
  metadata_columns <- c("SampleID", "WaterbodyID", "InletOrOutlet")
  parameter_columns <- setdiff(columns, c(metadata_columns, "Notes_and_comments_by_samplers_and_laboratory_staff", 
                                         "Time", "SamplingDate", "PO4_P_QC", "UorV"))
  
  cat("Identified", length(parameter_columns), "parameter columns to normalize\n")
  
  # Start building the SQL script
  sql_script <- c(
    "-- Create a new normalized table for the chemistry data",
    "CREATE TABLE IF NOT EXISTS normalized_chemistry (",
    "    waterbodyID INTEGER,",
    "    sampleID REAL,",
    "    inlet_outlet TEXT,",
    "    parameter TEXT,",
    "    value REAL",
    ");",
    "",
    "-- Delete any existing data (if table already exists)",
    "DELETE FROM normalized_chemistry;",
    ""
  )
  
  # Create INSERT statements for each parameter
  for (param in parameter_columns) {
    # Use column name directly without quotes
    insert_stmt <- paste0(
      "-- Insert ", param, " values\n",
      "INSERT INTO normalized_chemistry (waterbodyID, sampleID, inlet_outlet, parameter, value)\n",
      "SELECT WaterbodyID, SampleID, InletOrOutlet, '", param, "', ", param, "\n",
      "FROM water_samples\n",
      "WHERE ", param, " IS NOT NULL;"
    )
    
    sql_script <- c(sql_script, insert_stmt, "")
  }
  
  # Add indexes for better performance
  index_stmts <- c(
    "-- Create indexes for better performance",
    "CREATE INDEX IF NOT EXISTS idx_norm_waterbody ON normalized_chemistry(waterbodyID);",
    "CREATE INDEX IF NOT EXISTS idx_norm_sample ON normalized_chemistry(sampleID);", 
    "CREATE INDEX IF NOT EXISTS idx_norm_parameter ON normalized_chemistry(parameter);"
  )
  
  sql_script <- c(sql_script, index_stmts)
  
  # Write SQL to file
  writeLines(sql_script, output_sql_path)
  cat("SQL script written to", output_sql_path, "\n")
  
  # Execute the SQL script
  sql_text <- paste(sql_script, collapse = "\n")
  
  # Execute the SQL script with error handling
  tryCatch({
    cat("Executing SQL to transform data to 3NF...\n")
    dbExecute(con, sql_text)
    
    # Verify the transformation
    count <- dbGetQuery(con, "SELECT COUNT(*) FROM normalized_chemistry")
    cat("Number of rows in normalized table:", count[[1]], "\n")
    
    # Sample of normalized data
    sample <- dbGetQuery(con, "SELECT * FROM normalized_chemistry LIMIT 10")
    cat("Sample of normalized data:\n")
    print(sample)
    
    # Count by parameter
    param_counts <- dbGetQuery(con, "SELECT parameter, COUNT(*) as count FROM normalized_chemistry GROUP BY parameter ORDER BY count DESC")
    cat("Data points per parameter:\n")
    print(param_counts)
  }, error = function(e) {
    cat("Error during SQL execution:", conditionMessage(e), "\n")
    
    # Output the problematic columns for debugging
    cat("Columns that might have caused issues:\n")
    print(columns)
    
    # Try to execute each statement individually to identify problematic ones
    for(stmt in sql_script) {
      if(grepl("^INSERT", stmt)) {
        cat("Testing statement:", substr(stmt, 1, 80), "...\n")
        tryCatch({
          dbExecute(con, stmt)
        }, error = function(e) {
          cat("Error in statement:", conditionMessage(e), "\n")
        })
      }
    }
  })
  
  # Disconnect from the database
  dbDisconnect(con)
  cat("3NF transformation process completed!\n")
  
  return(invisible(NULL))
}

# Function to execute a SQL script from file
execute_sql_from_file <- function(db_path = "water_samples.db", sql_path = "3nf.sql") {
  library(RSQLite)
  
  # Connect to the database
  cat("Connecting to database:", db_path, "\n")
  con <- dbConnect(SQLite(), db_path)
  
  # Read the SQL script from the file
  sql_script <- readChar(sql_path, file.info(sql_path)$size)
  
  # Execute the SQL script line by line
  cat("Executing SQL script from", sql_path, "...\n")
  
  # Split the script by semicolons to get individual statements
  statements <- strsplit(sql_script, ";")[[1]]
  
  # Execute each statement with error handling
  for(stmt in statements) {
    stmt <- trimws(stmt)
    if(nchar(stmt) > 0) {
      tryCatch({
        dbExecute(con, paste0(stmt, ";"))
      }, error = function(e) {
        cat("Error executing statement:", substr(stmt, 1, 100), "...\n")
        cat("Error message:", conditionMessage(e), "\n")
      })
    }
  }
  
  # Verify the transformation
  count <- dbGetQuery(con, "SELECT COUNT(*) FROM normalized_chemistry")
  cat("Number of rows in normalized table after execution:", count[[1]], "\n")
  
  # Disconnect from the database
  dbDisconnect(con)
  cat("SQL execution complete!\n")
  
  return(invisible(NULL))
}

# Generate the 3NF SQL file
create_3nf_transformation()

# Alternatively, just execute an existing SQL file
# execute_sql_from_file()