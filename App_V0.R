# Load libraries
library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(readr)
library(ggspatial)
library(countrycode)
library(bslib)
library(tidyr)
library(stringr)

#------------------------------------------------------------------------------#
# Load and clean malaria data
#------------------------------------------------------------------------------#
# Define the path to CSV files
national_file_path <- "data/National_Unit-data_World.csv"    # Path to national data
subnational_file_path <- "data/Subnational_Unit-data.csv"    # Path to subnational data

# Clean column names for the national data
clean_national_columns <- function(df) {
  df <- df %>%
    rename(
      iso3 = ISO3,
      name = Name,
      admin_level = `Admin Level`,
      metric = Metric,
      units = Units,
      year = Year,
      value = Value
    )
  return(df)
}

# Clean column names for the subnational data
clean_subnational_columns <- function(df) {
  df <- df %>%
    rename(
      iso3 = ISO3,
      national_unit = `National Unit`,
      name = Name,
      admin_level = `Admin Level`,
      metric = Metric,
      units = Units,
      year = Year,
      value = Value
    )
  return(df)
}

# Load and clean data
national_data <- read_csv(national_file_path, show_col_types = FALSE) %>% clean_national_columns()
subnational_data <- read_csv(subnational_file_path, show_col_types = FALSE) %>% clean_subnational_columns()

#------------------------------------------------------------------------------#
# Load and process per-country data files
#------------------------------------------------------------------------------#
# Function to load and reshape a country's data file
load_country_data <- function(country) {
  file_path <- paste0("data/", country, "/", country, ".csv")
  if (file.exists(file_path)) {
    data <- read_csv(file_path, show_col_types = FALSE)
    return(data)
  } else {
    showNotification(paste("Data file not found for", country), type = "warning")
    return(NULL)
  }
}

reshape_country_data <- function(data) {
  if (is.null(data)) return(NULL)
  # Include necessary identifier columns
  id_cols <- c('asdf_id', 'shapeName')
  
  # Filter only numeric columns for pivot_longer
  numeric_data <- data %>% select(all_of(id_cols), where(is.numeric))
  
  data_long <- numeric_data %>%
    pivot_longer(
      cols = -all_of(id_cols),  # Exclude identifier columns
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      Metric = sub("\\.\\d{4}.*", "", variable),
      Year = as.integer(str_extract(variable, "\\d{4}")),
      Statistic = sub(".*\\d{4}\\.", "", variable)
    ) %>%
    filter(!is.na(Year)) %>%
    select(-variable)
  
  return(data_long)
}

# Function to load and process data for a given country
load_and_process_country_data <- function(country) {
  data <- load_country_data(country)
  data_long <- reshape_country_data(data)
  if (!is.null(data_long)) {
    data_long <- data_long %>% mutate(country = country)
  }
  return(data_long)
}

# Define a custom theme for all maps
map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, face = "italic", hjust = 0.5),
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.spacing.y = unit(0.5, "cm"),
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.caption = element_text(size = 16, hjust = 1, margin = margin(t = 10)),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Function to read GEOJSON files for selected countries
load_spatial_data <- function(countries, region_level) {
  spatial_list <- lapply(countries, function(country) {
    country_code <- toupper(countrycode::countrycode(country, "country.name", "iso3c"))
    adm_level <- ifelse(region_level == "National", "0", "1")
    geojson_file <- paste0("data/", country, "/", country_code, "_ADM", adm_level, ".geojson")
    if (file.exists(geojson_file)) {
      st_read(geojson_file, quiet = TRUE)
    } else {
      showNotification(paste("GeoJSON file not found for", country), type = "warning")
      return(NULL)
    }
  })
  # Remove NULL elements
  spatial_list <- spatial_list[!sapply(spatial_list, is.null)]
  if (length(spatial_list) == 0) {
    return(NULL)
  }
  spatial_data <- do.call(rbind, spatial_list)
  return(spatial_data)
}

#------------------------------------------------------------------------------#
# UI Definition
#------------------------------------------------------------------------------#
ui <- fluidPage(
  titlePanel("Malaria and Related Data Visualization in Africa (2010–2023)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region_level", "Region Level:", choices = c("National", "Subnational")),
      selectInput("country", "Select Country:", choices = unique(national_data$name), multiple = TRUE),
      selectInput("metric", "Select Metric:", choices = NULL),
      sliderInput("year_range", "Select Year Range:", min = 2010, max = 2023, value = c(2010, 2023), sep = ""),
      actionButton("update", "Update Map"),
      downloadButton("downloadData", "Download Data"),
      downloadButton("downloadPlot", "Download Map")
    ),
    mainPanel(
      plotOutput("dataMap", height = "700px")
    )
  )
)

#------------------------------------------------------------------------------#
# Server Logic
#------------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  # Reactive to gather and combine metrics across selected countries
  available_metrics <- reactive({
    req(input$country)
    
    # Compute malaria metrics available in selected countries
    malaria_metrics_list <- lapply(input$country, function(cntry) {
      if (input$region_level == "National") {
        metrics <- unique(national_data %>% filter(name == cntry) %>% pull(metric))
      } else {
        metrics <- unique(subnational_data %>% filter(national_unit == cntry) %>% pull(metric))
      }
      return(metrics)
    })
    # Remove NULL or empty elements
    malaria_metrics_list <- malaria_metrics_list[!sapply(malaria_metrics_list, function(x) length(x) == 0)]
    # If there are malaria metrics
    if (length(malaria_metrics_list) > 0) {
      malaria_metrics <- Reduce(intersect, malaria_metrics_list)
    } else {
      malaria_metrics <- character(0)
    }
    
    # Now, compute the intersection of country-specific metrics
    country_metrics_list <- lapply(input$country, function(cntry) {
      data <- load_country_data(cntry)
      if (!is.null(data)) {
        unique(reshape_country_data(data)$Metric)
      } else {
        NULL
      }
    })
    # Remove NULL elements
    country_metrics_list <- country_metrics_list[!sapply(country_metrics_list, is.null)]
    # If there are country-specific metrics
    if (length(country_metrics_list) > 0) {
      country_metrics <- Reduce(intersect, country_metrics_list)
    } else {
      country_metrics <- character(0)
    }
    
    # Combine the malaria metrics and country-specific metrics
    available_metrics <- unique(c(malaria_metrics, country_metrics))
    
    return(available_metrics)
  })
  
  # Update metric input choices based on available metrics
  observe({
    updateSelectInput(session, "metric", choices = available_metrics())
  })
  
  # Reactive expression to load malaria data if metric is from malaria data
  malaria_data <- reactive({
    req(input$country, input$metric)
    if (input$metric %in% unique(c(national_data$metric, subnational_data$metric))) {
      if (input$region_level == "National") {
        malaria_data <- national_data %>% filter(name %in% input$country, year >= input$year_range[1], year <= input$year_range[2], metric == input$metric)
      } else {
        malaria_data <- subnational_data %>% filter(national_unit %in% input$country, year >= input$year_range[1], year <= input$year_range[2], metric == input$metric)
      }
      return(malaria_data)
    } else {
      NULL
    }
  })
  
  # Reactive expression to load and reshape country data
  country_data <- reactive({
    req(input$country)
    data_list <- lapply(input$country, load_and_process_country_data)
    data_combined <- bind_rows(data_list)
    return(data_combined)
  })
  
  # Filtered data based on selected metric
  filtered_data <- reactive({
    req(input$country, input$metric)
    if (!is.null(malaria_data()) && input$metric %in% malaria_data()$metric) {
      data <- malaria_data()
    } else {
      data <- country_data() %>% filter(Metric == input$metric, Year >= input$year_range[1], Year <= input$year_range[2])
    }
    return(data)
  })
  
  # Map rendering
  output$dataMap <- renderPlot({
    input$update  # Depend on the update button
    req(filtered_data())
    spatial_data <- load_spatial_data(input$country, input$region_level)
    if (is.null(spatial_data) || nrow(spatial_data) == 0) {
      showNotification("No spatial data available for the selected country.", type = "error")
      return(NULL)
    }
    data <- filtered_data()
    
    # Determine the merging columns
    if (!is.null(malaria_data()) && input$metric %in% malaria_data()$metric) {
      if (input$region_level == "National") {
        merge_by_y <- "name"
      } else {
        merge_by_y <- "name"
      }
    } else {
      merge_by_y <- "shapeName"
    }
    merged_data <- merge(spatial_data, data, by.x = "shapeName", by.y = merge_by_y, all.x = TRUE)
    
    if (nrow(merged_data) == 0) {
      showNotification("No data available for the selected options.", type = "warning")
      return(NULL)
    }
    
    units <- unique(data$units)
    if (length(units) == 0 || is.null(units)) units <- ""
    
    ggplot(merged_data) +
      geom_sf(aes(fill = value), color = NA) +
      scale_fill_viridis_c(option = "inferno", name = paste(input$metric, units)) +
      annotation_scale(location = "bl") +
      annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering) +
      map_theme +
      labs(
        title = paste(input$metric, "in", paste(input$country, collapse = ", "), "(", paste(input$year_range, collapse = "-"), ")"),
        caption = "Source: Data | Visualization: Bachir SABO"
      )
  })
  
  # Data download
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  # Map download
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("map_", Sys.Date(), ".png", sep = "") },
    content = function(file) { ggsave(file, plot = last_plot(), width = 15, height = 10, dpi = 300) }
  )
}

#------------------------------------------------------------------------------#
# Run the Shiny App
#------------------------------------------------------------------------------#
shinyApp(ui = ui, server = server)
