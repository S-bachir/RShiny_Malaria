# Load necessary libraries
library(shiny)
library(sf)
library(ggplot2)
library(viridis)
library(dplyr)
library(readr)
library(ggspatial)
library(countrycode)

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

# Clean column names for easier handling
clean_column_names <- function(df) {
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
national_data <- read_csv("path/to/National_Unit-data.csv") %>% clean_column_names()
subnational_data <- read_csv("path/to/Subnational_Unit-data.csv") %>% clean_column_names()

# Define UI
ui <- fluidPage(
  titlePanel("Malaria Data Visualization in Africa (2010–2023)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(national_data$national_unit), multiple = TRUE),
      radioButtons("region_level", "Region Level:", choices = c("National", "Subnational")),
      sliderInput("year_range", "Select Year Range:", min = 2010, max = 2023, value = c(2010, 2023), sep = ""),
      selectInput("metric", "Select Metric:", choices = unique(national_data$metric)),
      actionButton("update", "Update Map"),
      downloadButton("downloadData", "Download Data"),
      downloadButton("downloadPlot", "Download Map")
    ),
    mainPanel(
      plotOutput("malariaMap", height = "700px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive expression for filtered malaria data
  filtered_data <- reactive({
    req(input$country, input$metric)
    data <- if (input$region_level == "National") national_data else subnational_data
    data <- data %>%
      filter(
        national_unit %in% input$country,
        year >= input$year_range[1], year <= input$year_range[2],
        metric == input$metric
      )
    return(data)
  })

  # Function to read GEOJSON files for selected countries
  load_spatial_data <- function(countries) {
    spatial_list <- lapply(countries, function(country) {
      country_code <- toupper(countrycode::countrycode(country, "country.name", "iso3c"))
      geojson_file <- paste0("path/to/", country, "/", country_code, "_ADM1.geojson")
      st_read(geojson_file)
    })
    spatial_data <- do.call(rbind, spatial_list)
    return(spatial_data)
  }

  # Reactive expression for spatial data
  spatial_data <- reactive({
    req(input$country)
    load_spatial_data(input$country)
  })

  # Render the malaria incidence map using ggplot2
  output$malariaMap <- renderPlot({
    input$update  # Depend on the update button
    data <- filtered_data()
    spatial <- spatial_data()

    # Merge spatial and malaria data
    merged_data <- merge(spatial, data, by.x = "NAME", by.y = "name")

    if (nrow(merged_data) == 0) {
      showNotification("No data available for the selected options.", type = "warning")
      return(NULL)
    }

    # Retrieve the units for the selected metric
    units <- unique(merged_data$units)

    # Create the map using ggplot2
    incidence_map <- ggplot() +
      geom_sf(data = merged_data, aes(fill = value), color = NA) +
      scale_fill_viridis_c(
        option = "inferno",
        name = paste(input$metric, "(", units, ")"),
        guide = guide_colorbar(
          title.position = "top",
          title.hjust = 0.5,
          barwidth = unit(20, "cm"),
          barheight = unit(0.5, "cm"),
          label.theme = element_text(size = 12, face = "bold", color = "black")
        )
      ) +
      annotation_scale(location = "bl", width_hint = 0.3, height = unit(0.5, "cm"), text_cex = 1.2) +
      annotation_north_arrow(
        location = "tr", which_north = "true",
        pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_fancy_orienteering
      ) +
      map_theme +
      labs(
        title = paste(input$metric, "in", paste(input$country, collapse = ", "), "(", paste(input$year_range, collapse = "-"), ")"),
        caption = "Source: Malaria Data | Visualization: Your Name"
      ) +
      coord_sf(datum = NA)

    print(incidence_map)
  })

  # Data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("malaria_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )

  # Map download handler
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("malaria_map_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save the last rendered plot
      ggsave(file, plot = last_plot(), width = 15, height = 10, dpi = 300)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
