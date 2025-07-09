# Water Chemistry Analysis Repository

## Overview

This repository contains a comprehensive R-based system for analyzing water chemistry data, with a focus on comparing inlet and outlet measurements across different water bodies and regions. The project includes data processing scripts, database management, and interactive Shiny applications for visualization and analysis.

## Repository Structure

### Core Files

- **`project.Rproj`** - RStudio project configuration file
- **`.gitignore`** - Git ignore file for R projects (excludes workspace files, history, etc.)

### Data Processing & Database

- **`makeDatabase.r`** - Database creation and CSV import script
- **`3nf.sql`** - Database normalization script (Third Normal Form)

### Shiny Applications

- **`waterChemistryShinyDemo.r`** - Comprehensive multi-module Shiny application
- **`inletOutlet.r`** - Simplified inlet vs outlet comparison app

## Detailed File Documentation

### makeDatabase.r

**Purpose**: Creates a SQLite database from CSV water chemistry data

**Key Features**:
- Reads CSV file (`ForClaude.csv` by default) with error handling
- Automatically cleans column names (removes spaces and special characters)
- Creates SQLite database (`water_samples.db`)
- Implements fallback CSV reading method if primary method fails
- Creates performance indexes on SampleID
- Provides detailed logging throughout the process

**Dependencies**: `RSQLite`, `readr`, `dplyr`

**Usage**:
```r
# Default usage (uses ForClaude.csv -> water_samples.db)
create_water_database()

# Custom file paths
create_water_database("my_data.csv", "my_database.db")
```

### 3nf.sql

**Purpose**: Normalizes water chemistry data into Third Normal Form (3NF)

**Key Features**:
- Creates `normalized_chemistry` table with structure:
  - `waterbodyID` (INTEGER)
  - `sampleID` (REAL) 
  - `inlet_outlet` (TEXT)
  - `parameter` (TEXT)
  - `value` (REAL)
- Transforms wide-format data into long-format for 24 different parameters
- Handles NULL value filtering
- Creates performance indexes on key columns

**Parameters Normalized**:
- Physical: Water Temperature, Air Temperature, Pressure, pH
- Chemical: Oxygen (% and mg/l), Electrical Conductivity, Chlorophyll-a
- Nutrients: Alkalinity, Phosphates (various forms), Nitrogen compounds
- Carbon: Total Organic Carbon, Suspended solids
- Greenhouse Gases: Dissolved CH4, N2O
- Microbiology: Methanotrophs and Methanogens (water and sediment samples)

### waterChemistryShinyDemo.r

**Purpose**: Comprehensive Shiny application with two analysis modules

**Architecture**: 
- **Main Selection Screen**: Choose between analysis types
- **Parameter Comparison Module**: Analyze relationships between any two parameters
- **Inlet-Outlet Comparison Module**: Compare inlet vs outlet for same parameter

**Key Features**:

#### Parameter Comparison Module
- X/Y axis parameter selection from all available parameters
- Geographic filtering by region and water body
- Statistical analysis including correlation coefficients and p-values
- Visualization options: regression lines, log scaling
- Interactive data tables with sorting and filtering
- Data export functionality

#### Inlet-Outlet Comparison Module  
- Parameter selection (limited to those with both inlet and outlet data)
- Paired sample analysis by water body and date
- Statistical summaries including paired t-tests
- Visualization options: 1:1 reference lines, regression lines, date labels
- Percent change calculations and regional summaries

**Dependencies**: `shiny`, `RSQLite`, `DBI`, `ggplot2`, `dplyr`, `DT`, `shinydashboard`, `shinyjs`

**Database Requirements**: 
- Expects `chem_to_display` view in the database
- Handles both scenarios: with and without `inlet_outlet` column in view
- Falls back to joining with `normalized_chemistry` table when needed

### inletOutlet.r

**Purpose**: Simplified Shiny application focused specifically on inlet vs outlet comparison

**Key Features**:
- Single-purpose interface for inlet/outlet analysis
- Parameter and region filtering
- Scatter plot with 1:1 reference line
- Summary statistics table by water body
- Calculates average changes and percent changes

**Dependencies**: `shiny`, `RSQLite`, `DBI`, `ggplot2`, `dplyr`

**Database Requirements**: 
- Expects `inlet_outlet` view/table in database

## Database Schema

The system expects/creates the following database structure:

### Primary Table: `water_samples`
- Contains raw water chemistry data in wide format
- Includes metadata: SampleID, WaterbodyID, Region, SamplingDate, InletOrOutlet
- Multiple parameter columns (24+ chemistry parameters)

### Normalized Table: `normalized_chemistry`
- Long-format normalized data (3NF)
- Structure: waterbodyID, sampleID, inlet_outlet, parameter, value
- Created by `3nf.sql` script

### Expected Views:
- **`chem_to_display`**: Main view for Shiny applications
- **`inlet_outlet`**: Specialized view for inlet/outlet comparisons

## Setup Instructions

1. **Data Preparation**: 
   - Ensure your CSV file matches expected format (columns for all 24 parameters)
   - Default filename should be `ForClaude.csv` or specify custom path

2. **Database Creation**:
   ```r
   source("makeDatabase.r")
   # Creates water_samples.db
   ```

3. **Database Normalization**:
   ```sql
   -- Run 3nf.sql against your database
   .read 3nf.sql
   ```

4. **Create Required Views**: 
   - Create `chem_to_display` view (joins normalized_chemistry with metadata)
   - Create `inlet_outlet` view for simplified app

5. **Run Applications**:
   ```r
   # Comprehensive app
   source("waterChemistryShinyDemo.r")
   
   # Or simplified app
   source("inletOutlet.r")
   ```

## Expected Data Format

The CSV input should contain columns for:
- **Metadata**: SampleID, WaterbodyID, Region, Waterbody, SamplingDate, InletOrOutlet
- **Parameters**: WaterTemperature_C, Air_Temperature_C, Pressure_Bar, pH, O2_pct, O2_mg_l, EC_us_cm, Chla, Alkalinitet_mekv_l, Kond_ms_m_25_C, PO4_P_filtr_mg_l, TotP_mg_l, Tot_P_filtr_mg_l, NH4_N_mg_l, NO2_NO3_N_mg_l, TOC_mg_l, Tot_N_mg_l, Susp_mg_l, Dissolved_CH4_mg_L, Dissolved_N2O_ug_L, Methanotrophs_water_sample, Methanotrophs_sediment_sample, Methanogens_water_sample, Methanogens_sediment_sample

## Use Cases

1. **Environmental Monitoring**: Track water quality changes across different water bodies
2. **Treatment Efficacy**: Compare inlet vs outlet to assess treatment performance
3. **Parameter Relationships**: Identify correlations between different chemistry parameters
4. **Regional Analysis**: Compare patterns across different geographic regions
5. **Temporal Analysis**: Track changes over time periods

## Technical Notes

- **Error Handling**: Robust error handling in database operations and CSV reading
- **Performance**: Indexed database operations for efficient querying
- **Flexibility**: Handles missing data and various CSV formats
- **Scalability**: Designed to handle large datasets with efficient SQL queries
- **User Experience**: Intuitive interface with clear navigation and data export options

## Dependencies Summary

**R Packages Required**:
- `shiny` - Web application framework
- `RSQLite` - SQLite database interface
- `DBI` - Database interface
- `ggplot2` - Data visualization
- `dplyr` - Data manipulation
- `readr` - CSV reading (makeDatabase.r)
- `DT` - Interactive tables (comprehensive app)
- `shinydashboard` - Dashboard components (comprehensive app)
- `shinyjs` - JavaScript integration (comprehensive app)

**External Dependencies**:
- SQLite database system
- R (version compatible with above packages)
