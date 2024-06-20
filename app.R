library(shiny)
library(shinyWidgets)
library(sf)
library(tidygeocoder)
library(dplyr)
library(leaflet)  # Add this line to load the leaflet package

# Load shapefiles
township_shapes <- st_read("./data/Long_Island_Cities_Towns.shp")
county_shapes <- st_read("./data/Counties.shp")

# Ensure the CRS of the shapefiles is the same as the point CRS (WGS 84)
township_shapes <- st_transform(township_shapes, crs = 4326)
county_shapes <- st_transform(county_shapes, crs = 4326)

ui <- fluidPage(
  titlePanel("Township Finder"),
  sidebarLayout(
    sidebarPanel(
      textInput("street", "Street Address (ex. 123 Kings Park Blvd)"),
      textInput("city", "City (in New York only)"),
      actionButton("submit", "Geocode")
    ),
    mainPanel(
      textOutput("matched_address"),
      textOutput("township"),
      textOutput("county"),
      leafletOutput("map")  # Add a leaflet output
    )
  ),
  HTML("<p>This app helps you find the township/city and county of a given address on Long Island. There are a few zip codes around here that span multiple towns. For more information on administrative divisions in New York, visit <a href='https://en.wikipedia.org/wiki/Administrative_divisions_of_New_York_(state)#Town' target='_blank'>this Wikipedia page</a>.</p>")
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(input$street, input$city)
    
    # Construct the full address
    full_address <- paste(input$street, input$city, "NY")
    
    # Geocode the address using tidygeocoder with the Census method
    geo_result <- geo(full_address, method = "census", full_results = TRUE)
    
    if (nrow(geo_result) == 0) {
      output$full_address <- renderText("Address not found.")
      return(NULL)
    }
    
    # Convert the geocode result to a data frame
    geo_result <- as.data.frame(geo_result)
    
    lat <- geo_result$lat
    lon <- geo_result$long
    matched_address <- geo_result$matchedAddress
    # Create an sf point object
    point <- st_sf(geometry = st_sfc(st_point(c(lon, lat)), crs = 4326))
    
    # Determine township and county
    township <- st_join(point, township_shapes, join = st_within)$NAME
    county <- st_join(point, county_shapes, join = st_within)$NAME
    
    # Render outputs
    output$matched_address <- renderText({
      paste("Full Address: ", matched_address, sep = "")
    })
    output$township <- renderText({
      paste("Township/City: ", ifelse(length(township) > 0, township, "Address is outside of Long Island or not found"), sep = "")
    })
    output$county <- renderText({
      paste("County: ", ifelse(length(county) > 0, county, "Address is outside of New York or not found"), sep = "")
    })
    
    # Render map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = lon, lat = lat, popup = matched_address)
    })
  })
}

shinyApp(ui = ui, server = server)