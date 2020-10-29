library(tidyverse)
library(leaflet)
library(shinydashboard)
library(shiny)

# -- load data
sensors <- read_rds("data/sensors.rds")
sensor_locations <- read_csv("data/locations.csv")

ui <- dashboardPage(
  dashboardHeader(
    title = "Melbourne Weather Sensor", # add title
    titleWidth = 300), # show entire width


  dashboardSidebar(disable = TRUE), # disable sidebar


  dashboardBody(
    # insert leaflet map
    box(leafletOutput("map"),
        title = "Sensor locations" ,
        width = 12,
        collapsible = TRUE),
    valueBoxOutput("temperature"),
    valueBoxOutput("humidity"),
    valueBoxOutput("pollution")
  )
)

server <- function(input, output) {

  recent_measurements <- reactive({
    # ----- no default location
    # ------ prevent error; click marker to run associated code
    # req(input$map_marker_click)

    # ----- OR if set arc 1046 as default
    site_id <- if(is.null(input$map_marker_click)){
      "arc1046"
    } else {
      input$map_marker_click$id
    }

    # ----- then
    sensors %>%
      filter(site_id == {{site_id}}) %>% # or !!
      group_by(type) %>%
      filter(local_time == max(local_time)) %>%
      ungroup()
  })

  output$map <- renderLeaflet({
    leaflet(data = sensor_locations) %>%
      addTiles() %>% # or addprovidertiles
      addMarkers(lng = ~ longitude, # access longitude
                 lat = ~ latitude,  # access latitude
                 popup = ~ description,
                 layerId = ~ site_id)
  })

  output$temperature <- renderValueBox({
    recent_measurements() %>%
      filter(type == "TPH.TEMP") %>%
      pull(value) %>%
    valueBox(value = .,
             subtitle = "Temperature")
  })

  output$humidity <- renderValueBox({
    recent_measurements() %>%
      filter(type == "TPH.RH") %>%
      pull(value) %>%
      valueBox(value = .,
               subtitle = "Relative Humidity")
  })

  output$pollution <- renderValueBox({
    recent_measurements() %>%
      filter(type == "PM2.5") %>%
      with(valueBox(value = paste(value, units),
               subtitle = type,
               color = case_when(
                 value > 300 ~ "maroon",
                 value > 200 ~ "purple",
                 value > 150 ~ "red",
                 value > 100 ~ "orange",
                 value > 50 ~ "yellow",
                 TRUE ~ "green")
               ))
  })

}


shinyApp(ui, server)
