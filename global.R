# Load packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(data.table)
library(DT)
library(rgdal)
library(leaflet)
library(sp)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(reshape2)

# Load dataset
conflicts_wide <- fread(file = "./data/gcri.csv", drop = 1)
conflicts_long <- fread(file = "./data/conflicts_by_region.csv", stringsAsFactors = TRUE)

# Custom Function ####################################
# Adapted from Kyle Walker's WDI_leaflet function
# Link: https://github.com/walkerke/teaching-with-datavis/blob/master/wdi-leaflet/wdi_leaflet.R
gcri_leaflet <- function(gc.indicator, gc.year, hue = "YlOrRd") {
  
  # Read shape file
  countries <- readOGR(dsn = "./data",
                       layer = "TM_WORLD_BORDERS_SIMPL-0.3",
                       verbose = FALSE)
  
  # Adapt dataframe for shapefile
  ## Load data frame
  conflicts_long <- fread(file = "./data/conflicts_by_region.csv")
  gcri <- conflicts_long %>% filter(indicator == gc.indicator, year == gc.year)
  
  countries2 <- countries
  countries_n <- sp::merge(x = countries2, y = gcri, by.x = "ISO3", by.y = "country.iso")
  
  # Create a color palette for map
  qpal <- colorNumeric(palette = hue, domain = countries_n[["composite.index"]])
  
  # Define popup functionality
  country_popup <- paste0("<strong>Country: </strong>", 
                          countries_n$country, 
                          "<br><strong>", 
                          gc.indicator, 
                          ", ", 
                          as.character(gc.year), 
                          ": </strong>", 
                          countries_n[["composite.index"]])
  
  # Create choropleth map
  leaflet(data = countries_n) %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    setView(0, 0, zoom = 2) %>% 
    addPolygons(fillColor = ~qpal(countries_n[["composite.index"]]), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = country_popup) %>% 
    addLegend(pal = qpal,
              values = ~countries_n[["composite.index"]],
              opacity = 0.7,
              title = gc.indicator,
              position = "bottomright")
}
