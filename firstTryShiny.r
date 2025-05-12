# Load required packages
library(shiny)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)

# Define the Shiny app
water_chemistry_app <- function(db_path = "water_samples.db") {
  
  # UI definition
  ui <- fluidPage(
    titlePanel("Water Chemistry Data Explorer"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("xParam", "X-axis Parameter:", choices = NULL),
        selectInput("yParam", "Y-axis Parameter:", choices = NULL),
        selectInput("waterbodyID", "Filter by Waterbody:", choices = NULL, multiple = TRUE),
        checkboxInput("showRegression", "Show regression line", value = TRUE),
        downloadButton("downloadData", "Download Plot Data")
      ),
      
      mainPanel(
        plotOutput("scatterPlot"),
        br(),
        tableOutput("correlationTable")
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    
    # Get list of available parameters
    parameters <- dbGetQuery(con, "SELECT DISTINCT parameter FROM normalized_chemistry ORDER BY parameter")$parameter
    
    # Get list of waterbody IDs
    waterbodies <- dbGetQuery(con, "SELECT DISTINCT waterbodyID FROM normalized_chemistry ORDER BY waterbodyID")$waterbodyID
    
    # Update UI input choices
    updateSelectInput(session, "xParam", choices = parameters)
    updateSelectInput(session, "yParam", choices = parameters, selected = parameters[2])
    updateSelectInput(session, "waterbodyID", choices = c("All", waterbodies), selected = "All")
    
    # Function to get data for selected parameters
    get_parameter_data <- reactive({
      # Validate inputs
      req(input$xParam)
      req(input$yParam)
      
      # Build waterbody filter
      waterbody_filter <- ""
      if (!identical(input$waterbodyID, "All") && length(input$waterbodyID) > 0 && !("All" %in% input$waterbodyID)) {
        waterbody_ids <- paste(input$waterbodyID, collapse = ",")
        waterbody_filter <- paste0(" AND x_data.waterbodyID IN (", waterbody_ids, ")")
      }
      
      # SQL query to join the data for both parameters
      query <- paste0("
        SELECT 
          x_data.sampleID,
          x_data.waterbodyID,
          x_data.inlet_outlet,
          x_data.value as x_value,
          y_data.value as y_value
        FROM 
          normalized_chemistry x_data
        JOIN 
          normalized_chemistry y_data 
        ON 
          x_data.sampleID = y_data.sampleID
        WHERE 
          x_data.parameter = '", input$xParam, "'
          AND y_data.parameter = '", input$yParam, "'", 
          waterbody_filter, "
        ORDER BY
          x_data.sampleID
      ")
      
      # Execute query
      data <- dbGetQuery(con, query)
      
      # Return the data
      return(data)
    })
    
    # Render the scatter plot
    output$scatterPlot <- renderPlot({
      data <- get_parameter_data()
      
      # Check if we have data
      if (nrow(data) == 0) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected combination") + 
               theme_minimal())
      }
      
      # Create the scatter plot
      p <- ggplot(data, aes(x = x_value, y = y_value, color = factor(waterbodyID))) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Relationship between", input$xParam, "and", input$yParam),
          x = input$xParam,
          y = input$yParam,
          color = "Waterbody ID"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12)
        )
      
      # Add regression line if requested
      if (input$showRegression) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")
      }
      
      return(p)
    })
    
    # Generate correlation table
    output$correlationTable <- renderTable({
      data <- get_parameter_data()
      
      if (nrow(data) < 3) {
        return(data.frame(
          Statistic = c("Number of Samples", "Correlation not available"),
          Value = c(nrow(data), "Insufficient data")
        ))
      }
      
      # Calculate correlation
      correlation <- cor.test(data$x_value, data$y_value)
      
      # Create the table
      data.frame(
        Statistic = c("Number of Samples", "Pearson Correlation", "p-value"),
        Value = c(
          nrow(data),
          round(correlation$estimate, 4),
          format.pval(correlation$p.value, digits = 3)
        )
      )
    })
    
    # Download handler for the data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("water_chemistry_", input$xParam, "_vs_", input$yParam, ".csv")
      },
      content = function(file) {
        write.csv(get_parameter_data(), file, row.names = FALSE)
      }
    )
    
    # Close connection when session ends
    onSessionEnded(function() {
      dbDisconnect(con)
    })
  }
  
  # Return the Shiny app
  shinyApp(ui, server)
}

# Run the app
water_chemistry_app()