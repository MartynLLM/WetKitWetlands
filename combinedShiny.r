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
                     ),
                     column(width = 6,
                            actionButton("btn_inlet_outlet", 
                                         "Compare Inlet vs. Outlet",
                                         class = "btn-success analysis-button btn-block")
                     )
                   ),
                   br(),
                   hr(),
                   p("• Parameter Comparison: Analyze the relationship between any two water chemistry parameters", 
                     style = "font-size: 14px;"),
                   p("• Inlet vs. Outlet: Compare the same parameter between inlet and outlet measurements", 
                     style = "font-size: 14px;")
                 )
          )
        )
    ),
    
    # Parameter comparison interface (initially hidden)
    div(id = "parameter-comparison-ui", style = "display: none;",
        fluidRow(
          column(width = 12,
                 h2("Parameter Comparison Analysis", 
                    span(actionButton("back_to_main_from_param", "← Back", 
                                     class = "btn-sm btn-default"), 
                         style = "float: right; margin-right: 20px;"))
          )
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput("xParam", "X-axis Parameter:", choices = NULL),
            selectInput("yParam", "Y-axis Parameter:", choices = NULL),
            hr(),
            div(class = "region-filter",
                h4("Geographic Filters"),
                selectInput("region_param", "Region:", choices = NULL, multiple = TRUE),
                selectInput("waterbody_param", "Water Body:", choices = NULL, multiple = TRUE)
            ),
            hr(),
            checkboxInput("showRegression_param", "Show regression line", value = TRUE),
            checkboxInput("logScale_param", "Use log scale (for both axes)", value = FALSE),
            hr(),
            downloadButton("downloadData_param", "Download Plot Data")
          ),
          
          mainPanel(
            plotOutput("scatterPlot_param", height = "400px"),
            br(),
            tableOutput("correlationTable_param"),
            br(),
            h4("Data Points"),
            DT::dataTableOutput("dataTable_param")
          )
        )
    ),
    
    # Inlet-Outlet comparison interface (initially hidden)
    div(id = "inlet-outlet-ui", style = "display: none;",
        fluidRow(
          column(width = 12,
                 h2("Inlet vs. Outlet Comparison", 
                    span(actionButton("back_to_main_from_io", "← Back", 
                                     class = "btn-sm btn-default"),
                         style = "float: right; margin-right: 20px;"))
          )
        ),
        sidebarLayout(
          sidebarPanel(
            selectInput("parameter_io", "Parameter:", choices = NULL),
            hr(),
            div(class = "region-filter",
                h4("Geographic Filters"),
                selectInput("region_io", "Region:", choices = NULL, multiple = TRUE),
                selectInput("waterbody_io", "Water Body:", choices = NULL, multiple = TRUE)
            ),
            hr(),
            checkboxInput("showLine_io", "Show 1:1 reference line", value = TRUE),
            checkboxInput("showRegression_io", "Show regression line", value = TRUE),
            checkboxInput("logScale_io", "Use log scale (for both axes)", value = FALSE),
            checkboxInput("showLabels_io", "Show sample dates as labels", value = FALSE),
            hr(),
            downloadButton("downloadData_io", "Download Paired Data")
          ),
          
          mainPanel(
            plotOutput("inletOutletPlot", height = "400px"),
            hr(),
            h4("Data Summary"),
            verbatimTextOutput("summaryStats_io"),
            hr(),
            h4("Paired Data"),
            DT::dataTableOutput("pairedDataTable_io")
          )
        )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    # Connect to database
    con <- dbConnect(SQLite(), db_path)
    
    # Check if Waterbody table exists
    tables <- dbListTables(con)
    if (!("Waterbody" %in% tables)) {
      stop("Waterbody table not found in the database. Please check your database structure.")
    }
    
    # Button actions to switch between interfaces
    observeEvent(input$btn_parameter_comparison, {
      hide("selection-screen")
      show("parameter-comparison-ui")
    })
    
    observeEvent(input$btn_inlet_outlet, {
      hide("selection-screen")
      show("inlet-outlet-ui")
    })
    
    observeEvent(input$back_to_main_from_param, {
      hide("parameter-comparison-ui")
      show("selection-screen")
    })
    
    observeEvent(input$back_to_main_from_io, {
      hide("inlet-outlet-ui")
      show("selection-screen")
    })
    
    # --- COMMON DATA OPERATIONS ---
    
    # Get list of all parameters
    all_parameters <- reactive({
      dbGetQuery(con, "SELECT DISTINCT parameter FROM normalized_chemistry ORDER BY parameter")$parameter
    })
    
    # Get list of all regions
    all_regions <- reactive({
      dbGetQuery(con, "SELECT DISTINCT region FROM Waterbody ORDER BY region")$region
    })
    
    # Get list of all waterbodies
    all_waterbodies <- reactive({
      dbGetQuery(con, "SELECT waterbodyID, Waterbody, region FROM Waterbody ORDER BY region, Waterbody")
    })
    
    # Filtered waterbodies based on selected regions (for parameter comparison)
    filtered_waterbodies_param <- reactive({
      req(all_waterbodies())
      wb_data <- all_waterbodies()
      
      # If regions are selected, filter waterbodies
      if (length(input$region_param) > 0) {
        wb_data <- wb_data %>% filter(region %in% input$region_param)
      }
      
      wb_data
    })
    
    # Filtered waterbodies based on selected regions (for inlet-outlet)
    filtered_waterbodies_io <- reactive({
      req(all_waterbodies())
      wb_data <- all_waterbodies()
      
      # If regions are selected, filter waterbodies
      if (length(input$region_io) > 0) {
        wb_data <- wb_data %>% filter(region %in% input$region_io)
      }
      
      wb_data
    })
    
    # --- PARAMETER COMPARISON MODULE ---
    
    # Initialize parameter comparison inputs
    observe({
      # Only run this when the parameter comparison UI is visible
      req(input$btn_parameter_comparison)
      
      params <- all_parameters()
      regions <- all_regions()
      
      # Update inputs
      updateSelectInput(session, "xParam", choices = params)
      updateSelectInput(session, "yParam", choices = params, selected = if(length(params) > 1) params[2] else params[1])
      updateSelectInput(session, "region_param", choices = c("All", regions), selected = "All")
    })
    
    # Update waterbody select based on region selection
    observe({
      waterbodies <- filtered_waterbodies_param()
      
      waterbody_choices <- setNames(
        waterbodies$waterbodyID,
        paste0(waterbodies$Waterbody, " (", waterbodies$region, ")")
      )
      
      updateSelectInput(session, "waterbody_param", 
                        choices = c("All", waterbody_choices), 
                        selected = "All")
    })
    
    # Function to get data for selected parameters
    get_parameter_data <- reactive({
      # Validate inputs
      req(input$xParam)
      req(input$yParam)
      
      # Build region filter
      region_filter <- ""
      if (length(input$region_param) > 0 && !("All" %in% input$region_param)) {
        regions <- paste0("'", paste(input$region_param, collapse = "','"), "'")
        region_filter <- paste0(" AND wb.region IN (", regions, ")")
      }
      
      # Build waterbody filter
      waterbody_filter <- ""
      if (length(input$waterbody_param) > 0 && !("All" %in% input$waterbody_param)) {
        waterbody_ids <- paste(input$waterbody_param, collapse = ",")
        waterbody_filter <- paste0(" AND wb.waterbodyID IN (", waterbody_ids, ")")
      }
      
      # SQL query to join the data for both parameters with Waterbody table
      query <- paste0("
        SELECT 
          x_data.sampleID,
          wb.waterbodyID,
          wb.Waterbody,
          wb.region,
          x_data.inlet_outlet,
          x_data.value as x_value,
          y_data.value as y_value
        FROM 
          normalized_chemistry x_data
        JOIN 
          normalized_chemistry y_data 
        ON 
          x_data.sampleID = y_data.sampleID
        JOIN
          Waterbody wb
        ON
          x_data.waterbodyID = wb.waterbodyID
        WHERE 
          x_data.parameter = '", input$xParam, "'
          AND y_data.parameter = '", input$yParam, "'", 
          region_filter,
          waterbody_filter, "
        ORDER BY
          wb.region, wb.Waterbody, x_data.sampleID
      ")
      
      # Execute query
      data <- dbGetQuery(con, query)
      
      # Return the data
      return(data)
    })
    
    # Render the parameter comparison scatter plot
    output$scatterPlot_param <- renderPlot({
      data <- get_parameter_data()
      
      # Check if we have data
      if (nrow(data) == 0) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected combination") + 
               theme_minimal() +
               theme(panel.background = element_rect(fill = "white")))
      }
      
      # Create the scatter plot
      p <- ggplot(data, aes(x = x_value, y = y_value, color = Waterbody)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Relationship between", input$xParam, "and", input$yParam),
          x = input$xParam,
          y = input$yParam,
          color = "Water Body"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.box = "horizontal"
        )
      
      # Apply log scale if requested
      if (input$logScale_param && all(data$x_value > 0) && all(data$y_value > 0)) {
        p <- p + scale_x_log10() + scale_y_log10()
      }
      
      # Add regression line if requested
      if (input$showRegression_param && nrow(data) >= 3) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed")
      }
      
      return(p)
    })
    
    # Generate correlation table for parameter comparison
    output$correlationTable_param <- renderTable({
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
        Statistic = c(
          "Number of Samples", 
          "Number of Water Bodies",
          "Number of Regions",
          "Pearson Correlation", 
          "p-value"
        ),
        Value = c(
          nrow(data),
          length(unique(data$Waterbody)),
          length(unique(data$region)),
          round(correlation$estimate, 4),
          format.pval(correlation$p.value, digits = 3)
        )
      )
    })
    
    # Display data table for parameter comparison
    output$dataTable_param <- DT::renderDataTable({
      data <- get_parameter_data()
      
      if (nrow(data) == 0) {
        return(data.frame(Message = "No data available for the selected criteria."))
      }
      
      # Format the data for display
      display_data <- data %>%
        select(
          SampleID = sampleID,
          Region = region,
          `Water Body` = Waterbody,
          `Inlet/Outlet` = inlet_outlet,
          !!sym(input$xParam) := x_value,
          !!sym(input$yParam) := y_value
        )
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 20, 50),
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      DT::formatRound(columns = c(input$xParam, input$yParam), digits = 4)
    })
    
    # Download handler for parameter comparison data
    output$downloadData_param <- downloadHandler(
      filename = function() {
        paste0(input$xParam, "_vs_", input$yParam, ".csv")
      },
      content = function(file) {
        write.csv(get_parameter_data(), file, row.names = FALSE)
      }
    )
    
    # --- INLET-OUTLET COMPARISON MODULE ---
    
    # Initialize inlet-outlet comparison inputs
    observe({
      # Only run this when the inlet-outlet UI is visible
      req(input$btn_inlet_outlet)
      
      # Get list of parameters that have both inlet and outlet values
      inlet_outlet_parameters_query <- "
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
      inlet_outlet_parameters <- dbGetQuery(con, inlet_outlet_parameters_query)$parameter
      
      regions <- all_regions()
      
      # Update inputs
      updateSelectInput(session, "parameter_io", choices = inlet_outlet_parameters)
      updateSelectInput(session, "region_io", choices = c("All", regions), selected = "All")
    })
    
    # Update waterbody select based on region selection (for inlet-outlet)
    observe({
      waterbodies <- filtered_waterbodies_io()
      
      waterbody_choices <- setNames(
        waterbodies$waterbodyID,
        paste0(waterbodies$Waterbody, " (", waterbodies$region, ")")
      )
      
      updateSelectInput(session, "waterbody_io", 
                        choices = c("All", waterbody_choices), 
                        selected = "All")
    })
    
    # Function to get paired inlet-outlet data by waterbodyID and date
    get_paired_data <- reactive({
      # Validate inputs
      req(input$parameter_io)
      
      # Build region filter
      region_filter <- ""
      if (length(input$region_io) > 0 && !("All" %in% input$region_io)) {
        regions <- paste0("'", paste(input$region_io, collapse = "','"), "'")
        region_filter <- paste0(" AND wb.region IN (", regions, ")")
      }
      
      # Build waterbody filter
      waterbody_filter <- ""
      if (length(input$waterbody_io) > 0 && !("All" %in% input$waterbody_io)) {
        waterbody_ids <- paste(input$waterbody_io, collapse = ",")
        waterbody_filter <- paste0(" AND wb.waterbodyID IN (", waterbody_ids, ")")
      }
      
      # SQL query to get paired inlet-outlet data by waterbodyID and date
      query <- paste0("
        SELECT 
          wb.waterbodyID,
          wb.Waterbody,
          wb.region,
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
        JOIN
          Waterbody wb ON ws_inlet.WaterbodyID = wb.waterbodyID
        WHERE 
          inlet.parameter = '", input$parameter_io, "' AND
          outlet.parameter = '", input$parameter_io, "' AND
          inlet.inlet_outlet = 'Inlet' AND
          outlet.inlet_outlet = 'Outlet' AND
          ws_inlet.WaterbodyID = ws_outlet.WaterbodyID AND
          ws_outlet.WaterbodyID = wb.waterbodyID AND
          ws_inlet.SamplingDate = ws_outlet.SamplingDate", 
          region_filter,
          waterbody_filter, "
        ORDER BY
          wb.region, wb.Waterbody, ws_inlet.SamplingDate
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
        
        # Format the sampling date for display
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
      p <- ggplot(data, aes(x = inlet_value, y = outlet_value, color = Waterbody)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Inlet vs. Outlet:", input$parameter_io),
          subtitle = paste("Matched by Water Body and Sampling Date"),
          x = paste("Inlet", input$parameter_io),
          y = paste("Outlet", input$parameter_io),
          color = "Water Body"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 12),
          legend.position = "bottom",
          legend.box = "horizontal"
        )
      
      # Apply log scale if requested
      if (input$logScale_io && all(data$inlet_value > 0) && all(data$outlet_value > 0)) {
        p <- p + scale_x_log10() + scale_y_log10()
      }
      
      # Add 1:1 reference line if requested
      if (input$showLine_io) {
        p <- p + geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50")
      }
      
      # Add regression line if requested
      if (input$showRegression_io && nrow(data) >= 3) {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid")
      }
      
      # Add date labels if requested
      if (input$showLabels_io) {
        # Format the date for display
        data$label <- format(data$SamplingDate, "%Y-%m-%d")
        
        p <- p + 
          geom_text(data = data, aes(label = label), 
                    hjust = -0.2, vjust = -0.5, size = 3, check_overlap = TRUE)
      }
      
      return(p)
    })
    
    # Generate summary statistics for inlet-outlet comparison
    output$summaryStats_io <- renderPrint({
      data <- get_paired_data()
      
      if (nrow(data) == 0) {
        return("No data available for the selected criteria.")
      }
      
      # Calculate summary statistics
      cat("Number of paired samples:", nrow(data), "\n")
      cat("Number of water bodies:", length(unique(data$Waterbody)), "\n")
      cat("Number of regions:", length(unique(data$region)), "\n")
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
      
      # Show summary by region if multiple regions are present
      if (length(unique(data$region)) > 1) {
        cat("\nSummary by Region:\n")
        region_summary <- data %>%
          group_by(region) %>%
          summarize(
            count = n(),
            mean_diff = mean(difference, na.rm = TRUE),
            median_diff = median(difference, na.rm = TRUE),
            mean_pct_change = mean(percent_change, na.rm = TRUE)
          )
        
        print(as.data.frame(region_summary))
      }
    })
    
    # Display paired data table for inlet-outlet comparison
    output$pairedDataTable_io <- DT::renderDataTable({
      data <- get_paired_data()
      
      if (nrow(data) == 0) {
        return(data.frame(Message = "No data available for the selected criteria."))
      }
      
      # Format the data for display
      display_data <- data %>%
        mutate(SamplingDate = format(SamplingDate, "%Y-%m-%d")) %>%
        select(
          Region = region,
          `Water Body` = Waterbody,
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
    
    # Download handler for inlet-outlet data
    output$downloadData_io <- downloadHandler(
      filename = function() {
        paste0("inlet_outlet_", input$parameter_io, "_", Sys.Date(), ".csv")
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
water_chemistry_combined_app()