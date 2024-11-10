# Malaria and Related Data Visualization in Africa (2010–2023)

[![R 4.0+](https://img.shields.io/badge/R-4.0%2B-blue)](https://cran.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-%234CAF50)](https://shiny.rstudio.com/)
[![Leaflet](https://img.shields.io/badge/Mapping-Leaflet-%2300BFFF)](https://rstudio.github.io/leaflet/)
[![GeoJSON](https://img.shields.io/badge/GeoData-GeoJSON-%23444444)](https://geojson.org/)
[![Data Transformation](https://img.shields.io/badge/Data--Transformation-dplyr-%23FF9A00)](https://dplyr.tidyverse.org/)



This simple (local) Shiny app provides interactive visualizations of malaria and related data across various African countries from 2010 to 2023. Users can explore national and subnational data, select various metrics, and customize visualizations for deeper insights into the malaria situation in Africa. Developed by Bachir SABO, this app uses `R`, `shiny`, `ggplot2`, `leaflet`, and several other R packages for data manipulation and visualization.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Data Sources](#data-sources)
- [Customization](#customization)
- [Sample Visuals](#sample-visuals)
- [Contributing](#contributing)
- [License](#license)

## Features

- **Map-Based Visualization**: Display malaria data on interactive maps at both national and subnational levels.
- **Customizable Parameters**: Select countries, metrics, color palettes, and year ranges for tailored analysis.
- **Downloadable Data**: Easily download filtered datasets for offline use and further analysis.

## Installation

1. **Clone the repository**:
    ```bash
    git clone https://github.com/S-bachir/RShiny_Malaria.git
    cd RShiny_Malaria
    ```

2. **Install the required R packages**:
    In R, run:
    ```R
    install.packages(c("shiny", "sf", "ggplot2", "viridis", "dplyr", "readr", 
                       "ggspatial", "countrycode", "bslib", "leaflet", 
                       "RColorBrewer", "shinythemes", "mapview"))
    ```

3. **Run the app**:
    - For the version with downloadable maps, run:
      ```R
      shiny::runApp('App_V0.R')
      ```
    - For the interactive version with customizable color palettes, run:
      ```R
      shiny::runApp('App_V0.1.R')
      ```

## Usage

1. **Launch the App**: 
   - Run `shiny::runApp('App_V0.R')` to start the version that allows downloading data analysis maps.
   - Run `shiny::runApp('App_V0.1.R')` for a more interactive experience with customizable color palettes, though maps are not downloadable in this version.
2. **Select Region Level**: Choose between `National` and `Subnational` data views.
3. **Select Country**: Use the dropdown to select one or multiple countries.
4. **Set Parameters**: Adjust the year range, metric, and color palette to refine the visualization.
5. **Update Map**: Click `Update Map` to refresh the map with selected parameters.
6. **Download Data**: Download the filtered dataset for offline analysis (available in `App_V0.R`).

## Data Sources

- **National Data**: Found in `data/National_Unit-data_World.csv`
- **Subnational Data**: Found in `data/Subnational_Unit-data.csv`
- **Per-Country Data Files**: Located in `data/{Country}/{Country}.csv`
- **GeoJSON Spatial Files**: Available for each country in `data/{Country}/{ISO3}_ADM{level}.geojson`

## Customization

### Adding New Data

To include data for additional metrics or years, ensure you have a `data` directory in your project. If it doesn't exist, create it first. Then, add your data as follows:
- **National Data**: Add data to `data/National_Unit-data_World.csv`
- **Subnational Data**: Add data to `data/Subnational_Unit-data.csv`
- **Country-Specific Data**: Add a new `{Country}.csv` in the `data/{Country}` directory.

### Changing Color Palettes

To modify color palettes, select from the available RColorBrewer palettes in the sidebar dropdown.

## Sample Visuals

Below are examples of the app's capabilities, showcasing both static and interactive map features.

<div align="center">
  <figure>
    <img src="plots/GIF/Rshinymalariademo_staticmap.gif" alt="App Demo Static" width="800"/>
    <figcaption>This RShiny minimal webapp displays ggplot type maps that can be downloaded. Run <code>App_V0.R</code> to use this version.</figcaption>
  </figure>
  <br>
  <img src="plots/example_staticmap_plot.png" alt="Example Static Map" width="600"/>
  <br><br>
  <figure>
    <img src="plots/GIF/Rshinymalariademo_interractivemap.gif" alt="App Demo Interactive" width="800"/>
    <figcaption>A more interactive map using the mapview package with customizable color palettes. Run <code>App_V0.1.R</code> to use this version.</figcaption>
  </figure>
</div>


## Contributing

Contributions are welcome! Please fork the repository and make a pull request, or submit an issue for feature requests or bug reports.
