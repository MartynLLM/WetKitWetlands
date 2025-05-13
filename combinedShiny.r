# Load required packages
library(shiny)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyjs)

# Define the combined Shiny app
water_chemistry_combined_app <- function(db_path = "water_samples.db") {
  
  # UI definition
  ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .analysis-button {
          height: 120px;
          font-size: 16px;
          margin: 10px;
          border-radius: 10px;
        }
        .title-box {
          background-color: #f8f9fa;
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
          border-left: 4px solid #007bff;
        }
        .region-filter {
          background-color: #f8f9fa;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 15px;
        }
      "))
    ),
    
    # Title Panel
    div(class = "title-box",
        h1("Water Chemistry Analysis Tool", align = "center"),
        h4("Select an analysis type to begin", align = "center")
    ),
    
    # Initial selection screen
    div(id = "selection-screen",
        fluidRow(
          column(width = 6, offset = 3,
                 wellPanel(
                   h3("Choose Analysis Type", align = "center"),
                   br(),
                   fluidRow(
                     column(width = 6,
                            actionButton("btn_parameter_comparison", 
                                         "Compare Different Parameters",
                                         class = "btn-primary analysis-button btn-block")