#install.packages(c("shiny", "sf", "ggplot2", "viridis", "dplyr", "readr", "ggspatial", "countrycode", "bslib","leaflet","RColorBrewer","shinythemes","mapview", "duckdb"))

# Load libraries
library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggspatial)
library(countrycode)
library(bslib)
library(tidyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(mapview)
library(duckdb)

#------------------------------------------------------------------------------#
# DuckDB Database Setup
#------------------------------------------------------------------------------#
# Path to DuckDB file
db_path <- "malaria_data.duckdb"

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), db_path)

#------------------------------------------------------------------------------#
# Data Loading and Preparation (Run Once)
#------------------------------------------------------------------------------#
# Load CSV data into DuckDB (run only if you haven't done this before)
# Note: Adjust the file paths to your CSV files
national_file_path <- "data/National_Unit-data_World.csv"
subnational_file_path <- "data/Subnational_Unit-data.csv"

# Uncomment the following lines if you need to load data into DuckDB initially
# national_data <- read_csv(national_file_path, show_col_types = FALSE)
# subnational_data <- read_csv(subnational_file_path, show_col_types = FALSE)

# dbWriteTable(con, "national_data", national_data, overwrite = TRUE)
# dbWriteTable(con, "subnational_data", subnational_data, overwrite = TRUE)

#------------------------------------------------------------------------------#
# UI Definition
#------------------------------------------------------------------------------#
ui <- navbarPage(
  title = "Malaria and Related Data Visualization in Africa (2010–2023)",
  theme = shinytheme("cosmo"),
  tabPanel("Map",
           sidebarLayout(
             sidebarPanel(
               selectInput("region_level", "Region Level:", choices = c("National", "Subnational")),
               selectInput("country", "Select Country:", choices = NULL, multiple = TRUE),
               selectInput("metric", "Select Metric:", choices = NULL),
               sliderInput("year_range", "Select Year Range:", min = 2010, max = 2023, value = c(2010, 2023), sep = ""),
               selectInput("palette", "Select Color Palette:", choices = rownames(brewer.pal.info)),
               actionButton("update", "Update Map"),
               downloadButton("downloadData", "Download Data")
             ),
             mainPanel(
               leafletOutput("dataMap", height = "700px")
             )
           )
  ),
  tabPanel("About",
           fluidRow(
             column(12,
                    h3("About this App"),
                    p("This application allows users to visualize malaria and related data in Africa from 2010 to 2023."),
                    p("Select countries, metrics, and customize the visualization to explore the data."),
                    p("Developed by Bachir SABO")
             )
           )
  )
)

#------------------------------------------------------------------------------#
# Server Logic
#------------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  # Populate country choices dynamically from the database
  observe({
    countries <- dbGetQuery(con, "SELECT DISTINCT name FROM national_data")
    updateSelectInput(session, "country", choices = countries$name)
  })
  
  # Reactive to gather and combine metrics across selected countries
  available_metrics <- reactive({
    req(input$country)
    region <- if (input$region_level == "National") "national_data" else "subnational_data"
    
    query <- sprintf(
      "SELECT DISTINCT metric FROM %s WHERE name IN (%s)",
      region, paste(shQuote(input$country), collapse = ",")
    )
    metrics <- dbGetQuery(con, query)
    
    return(metrics$metric)
  })
  
  # Update metric input choices based on available metrics
  observe({
    updateSelectInput(session, "metric", choices = available_metrics())
  })
  
  # Reactive expression to load and filter data
  filtered_data <- reactive({
    req(input$country, input$metric)
    region <- if (input$region_level == "National") "national_data" else "subnational_data"
    
    query <- sprintf(
      "SELECT * FROM %s WHERE name IN (%s) AND metric = %s AND year BETWEEN %d AND %d",
      region, paste(shQuote(input$country), collapse = ","), 
      shQuote(input$metric), input$year_range[1], input$year_range[2]
    )
    data <- dbGetQuery(con, query)
    
    return(data)
  })
  
  # Map rendering
  output$dataMap <- renderLeaflet({
    input$update  # Depend on the update button
    req(filtered_data())
    spatial_data <- load_spatial_data(input$country, input$region_level)
    if (is.null(spatial_data) || nrow(spatial_data) == 0) {
      showNotification("No spatial data available for the selected country.", type = "error")
      return(NULL)
    }
    data <- filtered_data()
    
    # Determine the merging columns
    merged_data <- merge(spatial_data, data, by.x = "shapeName", by.y = "name", all.x = TRUE)
    
    if (nrow(merged_data) == 0) {
      showNotification("No data available for the selected options.", type = "warning")
      return(NULL)
    }
    
    units <- unique(data$units)
    if (length(units) == 0 || is.null(units)) units <- ""
    
    # Create a color palette
    palette_func <- colorNumeric(palette = input$palette, domain = merged_data$value)
    
    # Create labels for tooltips
    labels <- paste0(
      "<strong>", merged_data$shapeName, "</strong><br/>",
      input$metric, ": ", round(merged_data$value, 2), " ", units
    ) %>% lapply(htmltools::HTML)
    
    # Generate the leaflet map
    leaflet(merged_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~palette_func(value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(
        pal = palette_func,
        values = ~value,
        title = paste(input$metric, units),
        position = "bottomright"
      )
  })
  
  # Data download
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
}

# Close DuckDB connection when the app stops
onStop(function() {
  dbDisconnect(con)
})

#------------------------------------------------------------------------------#
# Run the Shiny App
#------------------------------------------------------------------------------#
shinyApp(ui = ui, server = server)
