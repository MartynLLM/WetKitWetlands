# Water Chemistry Analysis: Inlet vs Outlet Shiny App
library(shiny)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)

# UI Definition
ui <- fluidPage(
  titlePanel("Water Chemistry Analysis: Inlet vs Outlet"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter", "Select Parameter:", choices = NULL),
      selectInput("region", "Select Region:", choices = NULL, multiple = TRUE),
      hr(),
      helpText("This app displays inlet vs outlet water chemistry values. Points above the 1:1 line indicate higher concentrations at the outlet than at the inlet.")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      hr(),
      tableOutput("summaryTable")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Connect to SQLite database
  db_conn <- reactive({
    dbConnect(RSQLite::SQLite(), "water_samples.db")
  })
  
  # Fetch available parameters and regions for dropdowns
  observe({
    conn <- db_conn()
    
    # Get unique parameters
    parameters <- dbGetQuery(conn, "SELECT DISTINCT PARAMETER FROM inlet_outlet ORDER BY PARAMETER")
    updateSelectInput(session, "parameter", choices = parameters$PARAMETER)
    
    # Get unique regions
    regions <- dbGetQuery(conn, "SELECT DISTINCT REGION FROM inlet_outlet ORDER BY REGION")
    updateSelectInput(session, "region", choices = regions$REGION, selected = regions$REGION[1])
    
    dbDisconnect(conn)
  })
  
  # Fetch filtered data based on user selections
  getData <- reactive({
    req(input$parameter)
    
    conn <- db_conn()
    
    # Build query with filters
    query <- "SELECT * FROM inlet_outlet WHERE PARAMETER = ?"
    params <- list(input$parameter)
    
    # Add region filter if specified
    if (!is.null(input$region) && length(input$region) > 0) {
      placeholders <- paste(rep("?", length(input$region)), collapse = ", ")
      query <- paste0(query, " AND REGION IN (", placeholders, ")")
      params <- c(params, input$region)
    }
    
    # Execute query
    data <- dbGetQuery(conn, query, params)
    
    dbDisconnect(conn)
    
    # Convert values to numeric
    data$INLET_VALUE <- as.numeric(data$INLET_VALUE)
    data$OUTLET_VALUE <- as.numeric(data$OUTLET_VALUE)
    
    data
  })
  
  # Generate scatter plot
  output$scatterPlot <- renderPlot({
    data <- getData()
    
    # Skip if no data
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Find the max value for setting plot limits
    max_value <- max(c(data$INLET_VALUE, data$OUTLET_VALUE), na.rm = TRUE)
    
    # Create plot with ggplot2
    ggplot(data, aes(x = INLET_VALUE, y = OUTLET_VALUE, color = WATERBODY)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      xlim(0, max_value * 1.1) +
      ylim(0, max_value * 1.1) +
      labs(
        title = paste("Inlet vs Outlet Concentration for", input$parameter),
        subtitle = paste("Region(s):", paste(input$region, collapse = ", ")),
        x = "Inlet Concentration",
        y = "Outlet Concentration",
        color = "Water Body"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")
  })
  
  # Generate summary table
  output$summaryTable <- renderTable({
    data <- getData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Calculate summary statistics by waterbody
    data %>%
      group_by(WATERBODY) %>%
      summarize(
        `Sample Count` = n(),
        `Avg Inlet` = round(mean(INLET_VALUE, na.rm = TRUE), 2),
        `Avg Outlet` = round(mean(OUTLET_VALUE, na.rm = TRUE), 2),
        `Avg Change` = round(mean(OUTLET_VALUE - INLET_VALUE, na.rm = TRUE), 2),
        `Percent Change` = round(mean((OUTLET_VALUE - INLET_VALUE) / INLET_VALUE * 100, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(`Avg Change`))
  })
  
  # Disconnect from database when the session ends
  onSessionEnded(function() {
    if (dbIsValid(db_conn())) {
      dbDisconnect(db_conn())
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)