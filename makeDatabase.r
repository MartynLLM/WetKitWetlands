# Load required packages
library(RSQLite)
library(readr)

# Create a function to handle the database creation process
create_water_database <- function(csv_path = "ForClaude.csv", db_path = "water_samples.db") {
  # Read the CSV file
  cat("Reading CSV file:", csv_path, "\n")
  water_data <- read_csv(csv_path)
  cat("CSV loaded successfully with", nrow(water_data), "rows and", ncol(water_data), "columns\n")
  
  # Create a SQLite database connection
  cat("Creating SQLite database:", db_path, "\n")
  con <- dbConnect(SQLite(), db_path)
  
  # Create table with appropriate schema
  cat("Creating water_samples table...\n")
  dbExecute(con, "
  CREATE TABLE water_samples (
    SampleID REAL,
    WaterbodyID INTEGER,
    InletOrOutlet TEXT,
    SamplingDate TEXT,
    Time TEXT,
    WaterTemperature_C REAL,
    Air_Temperature_C REAL,
    Pressure_Bar REAL,
    pH REAL,
    O2_pct REAL,
    O2_mg_l REAL,
    EC_us_cm REAL,
    Chla REAL,
    Alkalinitet_mekv_l REAL,
    Kond_ms_m_25_C REAL,
    \"PO4_P filtr_mg_l\" REAL,
    TotP_mg_l REAL,
    Tot_P_filtr_mg_l REAL,
    NH4_N_mg_l REAL,
    NO2_NO3_N_mg_l REAL,
    NO2_NO3_QC TEXT,
    TOC_mg_l REAL,
    Tot_N_mg_l REAL,
    Susp_mg_l REAL,
    Dissolved_CH4_mg_L REAL,
    Dissolved_N2O_ug_L REAL,
    UorV TEXT,
    Methanotrophs_water_sample REAL,
    Methanotrophs_sediment_sample REAL,
    Methanogens_water_sample REAL,
    \"Methanogens_sediment _sample\" REAL
  )")
  
  # Insert data into the SQLite table
  cat("Writing data to database...\n")
  dbWriteTable(con, "water_samples", water_data, append = TRUE, row.names = FALSE)
  
  # Create an index on SampleID for faster querying
  cat("Creating index on SampleID...\n")
  dbExecute(con, "CREATE INDEX idx_sample_id ON water_samples(SampleID)")
  
  # Verify the data was written correctly
  count <- dbGetQuery(con, "SELECT COUNT(*) FROM water_samples")
  cat("Number of rows in database:", count[[1]], "\n")
  
  # Disconnect from the database
  dbDisconnect(con)
  cat("Database creation complete!\n")
}

# Alternative approach using automatic table creation
create_water_database_auto <- function(csv_path = "ForClaude.csv", db_path = "water_samples_auto.db") {
  # Read the CSV file
  water_data <- read_csv(csv_path)
  
  # Connect to SQLite database
  con <- dbConnect(SQLite(), db_path)
  
  # Let RSQLite automatically create the table (handles column names with spaces)
  dbWriteTable(con, "water_samples", water_data, overwrite = TRUE)
  
  # Create an index on SampleID
  dbExecute(con, "CREATE INDEX idx_sample_id ON water_samples(SampleID)")
  
  # Disconnect from the database
  dbDisconnect(con)
  cat("Database created successfully using automatic table creation!\n")
}

# Example of how to query the database
query_example <- function(db_path = "water_samples.db") {
  # Connect to the database
  con <- dbConnect(SQLite(), db_path)
  
  # Example query: Get average pH and O2 levels by WaterbodyID
  result <- dbGetQuery(con, "
    SELECT 
      WaterbodyID, 
      AVG(pH) as Avg_pH, 
      AVG(O2_mg_l) as Avg_O2_mg_l,
      COUNT(*) as Sample_Count
    FROM water_samples
    GROUP BY WaterbodyID
    ORDER BY Avg_pH DESC
  ")
  
  # Print the result
  print(result)
  
  # Disconnect from the database
  dbDisconnect(con)
}

# Run the database creation function
create_water_database()

# Uncomment to use the automatic table creation approach instead
# create_water_database_auto()

# Uncomment to run a sample query
# query_example()