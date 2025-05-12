# Load required packages
library(shiny)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(DT)

# Define the Shiny app for inlet-outlet comparison
inlet_outlet_app <- function(db_path = "water_samples.db") {
  
  # UI definition
  ui <- fluidPage(
    titlePanel("Inlet vs. Outlet Water Chemistry Comparison"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("parameter", "Parameter:", choices = NULL),
        selectInput("waterbodyID", "Select Waterbody:", choices = NULL, multiple = TRUE),
        checkboxInput("showLine", "Show 1:1 reference line", value = TRUE),
        checkboxInput("showRegression", "Show regression line", value = TRUE),
        hr(),
        checkboxInput("logScale", "Use log scale (for both axes)", value = FALSE),
        checkboxInput("showLabels", "Show sample dates as labels", value = FALSE),
        hr(),
        downloadButton("downloadData", "Download Paired Data")
      ),
      
      mainPanel(
        plotOutput("inletOutletPlot", height = "400px"),
        hr(),
        h4("Data Summary"),
        verbatimTextOutput("summaryStats"),
        hr(),
        h4("Paired Data"),
        DT::dataTableOutput("pairedDataTable")
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    
    # Get list of available parameters that have both inlet and outlet values
    parameters_query <- "
      SELECT DISTINCT parameter 
      FROM normalized_chemistry
      WHERE parameter IN (
        SELECT parameter
        FROM normalized_chemistry
        WHERE inlet_outlet = 'Inlet'
        INTERSECT
        SELECT parameter
        FROM normalized_chemistry
        WHERE inlet_outlet = 'Outlet'
      )
      ORDER BY parameter
    "
    parameters <- dbGetQuery(con, parameters_query)$parameter
    
    # Get list of waterbody IDs that have both inlet and outlet data
    waterbodies_query <- "
      SELECT DISTINCT waterbodyID 
      FROM normalized_chemistry
      WHERE waterbodyID IN (
        SELECT waterbodyID
        FROM normalized_chemistry
        WHERE inlet_outlet = 'Inlet'
        INTERSECT
        SELECT waterbodyID
        FROM normalized_chemistry
        WHERE inlet_outlet = 'Outlet'
      )
      ORDER BY waterbodyID
    "
    waterbodies <- dbGetQuery(con, waterbodies_query)$waterbodyID
    
    # Update UI input choices
    updateSelectInput(session, "parameter", choices = parameters)
    updateSelectInput(session, "waterbodyID", choices = c("All", waterbodies), selected = "All")
    
    # Function to get paired inlet-outlet data by waterbodyID and date
    get_paired_data <- reactive({
      # Validate inputs
      req(input$parameter)
      
      # Build waterbody filter
      waterbody_filter <- ""
      if (length(input$waterbodyID) > 0 && !("All" %in% input$waterbodyID)) {
        waterbody_ids <- paste(input$waterbodyID, collapse = ",")
        waterbody_filter <- paste0(" AND ws_inlet.WaterbodyID IN (", waterbody_ids, ")")
      }
      
      # SQL query to get paired inlet-outlet data by waterbodyID and date
      query <- paste0("
        SELECT 
          ws_inlet.WaterbodyID,
          ws_inlet.SamplingDate,
          inlet.sampleID as inlet_sampleID,
          outlet.sampleID as outlet_sampleID,
          inlet.value as inlet_value,
          outlet.value as outlet_value
        FROM 
          normalized_chemistry inlet
        JOIN
          water_samples ws_inlet ON inlet.sampleID = ws_inlet.SampleID
        JOIN 
          normalized_chemistry outlet
        JOIN
          water_samples ws_outlet ON outlet.sampleID = ws_outlet.SampleID
        WHERE 
          inlet.parameter = '", input$parameter, "' AND
          outlet.parameter = '", input$parameter, "' AND
          inlet.inlet_outlet = 'Inlet' AND
          outlet.inlet_outlet = 'Outlet' AND
          ws_inlet.WaterbodyID = ws_outlet.WaterbodyID AND
          ws_inlet.SamplingDate = ws_outlet.SamplingDate", 
          waterbody_filter, "
        ORDER BY
          ws_inlet.WaterbodyID, ws_inlet.SamplingDate
      ")
      
      # Execute query
      data <- dbGetQuery(con, query)
      
      # Add difference and percent change columns
      if (nrow(data) > 0) {
        data$difference <- data$outlet_value - data$inlet_value
        data$percent_change <- ifelse(
          data$inlet_value != 0,
          (data$outlet_value - data$inlet_value) / data$inlet_value * 100,
          NA
        )
        
        # Format the sampling date for display (assuming it's stored as YYYY-MM-DD or similar)
        if (is.character(data$SamplingDate)) {
          # Try to convert to Date if it's a character
          data$SamplingDate <- as.Date(data$SamplingDate)
        }
      }
      
      return(data)
    })
    
    # Render the inlet-outlet plot
    output$inletOutletPlot <- renderPlot({
      data <- get_paired_data()
      
      # Check if we have data
      if (nrow(data) == 0) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No paired inlet-outlet data available for the selected parameter/waterbody") + 
               theme_minimal() +
               theme(panel.background = element_rect(fill = "white")))
      }
      
      # Create the base plot
      p <- ggplot(data, aes(x = inlet_value, y = outlet_value, color = factor(WaterbodyID))) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Inlet vs. Outlet:", input$parameter),
          subtitle = paste("Matched by Waterbody ID and Sampling Date"),
          x = paste("Inlet", input$parameter),
          y = paste("Outlet", input$parameter),
          color = "Waterbody ID"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12)
        )
      
      # Apply log scale if requested
      if (input$logScale && all(data$inlet_value > 0) && all(data$outlet_value > 0)) {
        p <- p + scale_x_log10() + scale_y_log10()
      }
      
      # Add 1:1 reference line if requested
      if (input$showLine) {
        p <- p + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50")
      }
      
      # Add regression line if requested
      if (input$showRegression && nrow(data) >= 3) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid")
      }
      
      # Add date labels if requested
      if (input$showLabels) {
        # Format the date for display
        data$label <- format(data$SamplingDate, "%Y-%m-%d")
        
        p <- p + 
          geom_text(data = data, aes(label = label), 
                    hjust = -0.2, vjust = -0.5, size = 3, check_overlap = TRUE)
      }
      
      return(p)
    })
    
    # Generate summary statistics
    output$summaryStats <- renderPrint({
      data <- get_paired_data()
      
      if (nrow(data) == 0) {
        return("No data available for the selected criteria.")
      }
      
      # Calculate summary statistics
      cat("Number of paired samples:", nrow(data), "\n")
      cat("Number of waterbodies:", length(unique(data$WaterbodyID)), "\n")
      cat("Date range:", format(min(data$SamplingDate), "%Y-%m-%d"), "to", 
          format(max(data$SamplingDate), "%Y-%m-%d"), "\n\n")
      
      # Basic statistics on inlet values
      cat("Inlet values summary:\n")
      print(summary(data$inlet_value))
      cat("\n")
      
      # Basic statistics on outlet values
      cat("Outlet values summary:\n")
      print(summary(data$outlet_value))
      cat("\n")
      
      # Difference statistics
      cat("Change (Outlet - Inlet) summary:\n")
      print(summary(data$difference))
      cat("\n")
      
      # Mean and median percent change
      cat("Mean percent change:", round(mean(data$percent_change, na.rm = TRUE), 2), "%\n")
      cat("Median percent change:", round(median(data$percent_change, na.rm = TRUE), 2), "%\n\n")
      
      # Paired t-test between inlet and outlet values
      if (nrow(data) >= 3) {
        cat("Paired t-test (Inlet vs. Outlet):\n")
        t_test <- t.test(data$inlet_value, data$outlet_value, paired = TRUE)
        cat("t-value:", round(t_test$statistic, 4), "\n")
        cat("p-value:", format.pval(t_test$p.value, digits = 4), "\n")
        
        if (t_test$p.value < 0.05) {
          if (mean(data$difference) > 0) {
            cat("Interpretation: Values significantly INCREASE from inlet to outlet (p < 0.05)\n")
          } else {
            cat("Interpretation: Values significantly DECREASE from inlet to outlet (p < 0.05)\n")
          }
        } else {
          cat("Interpretation: No significant difference between inlet and outlet values (p ≥ 0.05)\n")
        }
      }
    })
    
    # Display paired data table
    output$pairedDataTable <- DT::renderDataTable({
      data <- get_paired_data()
      
      if (nrow(data) == 0) {
        return(data.frame(Message = "No data available for the selected criteria."))
      }
      
      # Format the data for display
      display_data <- data %>%
        mutate(SamplingDate = format(SamplingDate, "%Y-%m-%d")) %>%
        select(
          WaterbodyID,
          SamplingDate,
          `Inlet SampleID` = inlet_sampleID,
          `Outlet SampleID` = outlet_sampleID,
          Inlet = inlet_value,
          Outlet = outlet_value,
          Difference = difference,
          `% Change` = percent_change
        )
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 20, 50, 100),
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      DT::formatRound(columns = c("Inlet", "Outlet", "Difference"), digits = 4) %>%
      DT::formatRound(columns = c("% Change"), digits = 2)
    })
    
    # Download handler for the paired data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("inlet_outlet_", input$parameter, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Format date properly for the download
        data <- get_paired_data()
        data$SamplingDate <- format(data$SamplingDate, "%Y-%m-%d")
        write.csv(data, file, row.names = FALSE)
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
inlet_outlet_app()