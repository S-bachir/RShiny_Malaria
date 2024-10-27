# Load necessary libraries
library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(readr)
library(ggspatial)
library(countrycode)
library(tidyr)
library(stringr)

#------------------------------------------------------------------------------#
# Define a custom theme for all maps
#------------------------------------------------------------------------------#
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

#------------------------------------------------------------------------------#
# Load and clean malaria data
#------------------------------------------------------------------------------#
# Define the path to your CSV files
national_file_path <- "data/National_Unit-data_World.csv"
subnational_file_path <- "data/Subnational_Unit-data.csv"

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

# Load and clean malaria data
national_data <- read_csv(national_file_path) %>% clean_national_columns()
subnational_data <- read_csv(subnational_file_path) %>% clean_subnational_columns()

#------------------------------------------------------------------------------#
# Define functions to load and process per-country data
#------------------------------------------------------------------------------#
# Function to load a country's CSV file
load_country_data <- function(country) {
  file_path <- paste0("data/", country, "/", country, ".csv")
  if (file.exists(file_path)) {
    data <- read_csv(file_path)
    data$country <- country
    return(data)
  } else {
    return(NULL)
  }
}

# Function to reshape country data from wide to long format
reshape_country_data <- function(data) {
  data_long <- data %>%
    pivot_longer(
      cols = -c(asdf_id, country),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      Year = as.integer(str_extract(variable, "\\d{4}")),
      Metric = str_remove(variable, "\\.\\d{4}.*"),
      Statistic = str_extract(variable, "(?<=\\d{4}\\.).*")
    ) %>%
    filter(!is.na(Year)) %>%
    select(-variable)
  return(data_long)
}

# Function to load and process data for selected countries
load_and_process_country_data <- function(countries) {
  data_list <- lapply(countries, function(country) {
    data <- load_country_data(country)
    if (!is.null(data)) {
      data_long <- reshape_country_data(data)
      return(data_long)
    } else {
      return(NULL)
    }
  })
  data_combined <- bind_rows(data_list)
  return(data_combined)
}

#------------------------------------------------------------------------------#
# Shiny UI
#------------------------------------------------------------------------------#
ui <- fluidPage(
  titlePanel("Malaria and Related Data Visualization in Africa (2010–2023)"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("region_level", "Region Level:",
                   choices = c("National", "Subnational")),
      uiOutput("country_selector"),
      uiOutput("metric_selector"),
      sliderInput("year_range", "Select Year Range:",
                  min = 1900, max = 2023, value = c(2010, 2023), sep = ""),
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
# Shiny Server
#------------------------------------------------------------------------------#
server <- function(input, output, session) {

  # Reactive expression for available countries
  available_countries <- reactive({
    if (input$region_level == "National") {
      unique(national_data$name)
    } else {
      unique(subnational_data$national_unit)
    }
  })

  # Update country selector based on region level
  output$country_selector <- renderUI({
    selectInput("country", "Select Country:",
                choices = available_countries(), multiple = TRUE)
  })

  # Reactive expression for available metrics
  available_metrics <- reactive({
    req(input$country)
    # Malaria metrics
    malaria_metrics <- if (input$region_level == "National") {
      national_data %>%
        filter(name %in% input$country) %>%
        pull(metric) %>%
        unique()
    } else {
      subnational_data %>%
        filter(national_unit %in% input$country) %>%
        pull(metric) %>%
        unique()
    }
    # Per-country data metrics
    country_metrics <- load_and_process_country_data(input$country) %>%
      pull(Metric) %>%
      unique()
    # Combine and return unique metrics
    unique(c(malaria_metrics, country_metrics))
  })

  # Update metric selector
  output$metric_selector <- renderUI({
    selectInput("metric", "Select Metric:", choices = available_metrics())
  })

  # Reactive expression for filtered data
  filtered_data <- reactive({
    req(input$country, input$metric)
    # Check if metric is in malaria data
    if (input$metric %in% c(national_data$metric, subnational_data$metric)) {
      if (input$region_level == "National") {
        data <- national_data %>%
          filter(
            name %in% input$country,
            year >= input$year_range[1],
            year <= input$year_range[2],
            metric == input$metric
          )
      } else {
        data <- subnational_data %>%
          filter(
            national_unit %in% input$country,
            year >= input$year_range[1],
            year <= input$year_range[2],
            metric == input$metric
          )
      }
      data <- data %>%
        rename(
          Year = year,
          Value = value,
          Metric = metric,
          units = units
        )
    } else {
      # Load and filter per-country data
      data <- load_and_process_country_data(input$country) %>%
        filter(
          Metric == input$metric,
          Year >= input$year_range[1],
          Year <= input$year_range[2]
        ) %>%
        rename(
          Value = value
        )
    }
    return(data)
  })

  # Reactive expression for spatial data
  spatial_data <- reactive({
    req(input$country)
    if (input$region_level == "National") {
      countries <- input$country
      adm_level <- "ADM0"
    } else {
      countries <- input$country
      adm_level <- "ADM1"
    }
    spatial_list <- lapply(countries, function(country) {
      country_code <- toupper(countrycode(country, "country.name", "iso3c"))
      geojson_file <- paste0("data/", country, "/", country_code,
                             "_", adm_level, ".geojson")
      if (file.exists(geojson_file)) {
        st_read(geojson_file)
      } else {
        NULL
      }
    })
    spatial_data <- do.call(rbind, spatial_list)
    return(spatial_data)
  })

  # Render the map
  output$dataMap <- renderPlot({
    input$update  # Depend on the update button
    data <- filtered_data()
    spatial <- spatial_data()

    if (is.null(spatial) || nrow(spatial) == 0) {
      showNotification("Spatial data not available.", type = "error")
      return(NULL)
    }

    # Merge data with spatial data
    if (input$region_level == "National") {
      merged_data <- merge(spatial, data, by.x = "NAME", by.y = "name")
    } else {
      merged_data <- merge(spatial, data, by.x = "NAME", by.y = "name")
    }

    if (nrow(merged_data) == 0) {
      showNotification("No data available for the selected options.",
                       type = "warning")
      return(NULL)
    }

    # Retrieve units or statistics
    units_or_statistic <- if ("units" %in% names(merged_data)) {
      unique(merged_data$units)
    } else if ("Statistic" %in% names(merged_data)) {
      unique(merged_data$Statistic)
    } else {
      ""
    }

    # Create the map using ggplot2
    data_map <- ggplot() +
      geom_sf(data = merged_data, aes(fill = Value), color = NA) +
      scale_fill_viridis_c(
        option = "inferno",
        name = paste(input$metric, units_or_statistic),
        guide = guide_colorbar(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(20, "cm"),
          barheight = unit(0.5, "cm"),
          label.theme = element_text(size = 12, face = "bold", color = "black")
        )
      ) +
      annotation_scale(location = "bl", width_hint = 0.3,
                       height = unit(0.5, "cm"), text_cex = 1.2) +
      annotation_north_arrow(
        location = "tr", which_north = "true",
        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_fancy_orienteering
      ) +
      map_theme +
      labs(
        title = paste(input$metric, "in",
                      paste(input$country, collapse = ", "),
                      "(", paste(input$year_range, collapse = "-"), ")"),
        caption = "Source: Data | Visualization: Your Name"
      ) +
      coord_sf(datum = NA)

    print(data_map)
  })

  # Data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )

  # Map download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("map_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 15, height = 10, dpi = 300)
    }
  )
}

#------------------------------------------------------------------------------#
# Run the Shiny App
#------------------------------------------------------------------------------#
shinyApp(ui = ui, server = server)
