library(shiny)
library(shinyDarkmode)
library(leaflet)

ui <- fluidPage(
  use_darkmode(),
  selectInput("polygon_choice", "Choose Polygon:",
              choices = c("Polygon 1", "Polygon 2", "Polygon 3")),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  darkmode()
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(lng = c(-4.2576, -4.2676, -4.2576),
                  lat = c(55.8651, 55.8651, 55.8751),
                  popup = "Hello, Glasgow!")
  })
  
  observe({
    polygon_data <- switch(input$polygon_choice,
                           "Polygon 1" = list(lng = c(-4.2576, -4.2676, -4.2576),
                                              lat = c(55.8651, 55.8651, 55.8751)),
                           "Polygon 2" = list(lng = c(-4.28, -4.29, -4.28),
                                              lat = c(55.84, 55.84, 55.85)),
                           "Polygon 3" = list(lng = c(-4.2, -4.21, -4.2),
                                              lat = c(55.87, 55.87, 55.88)))
    
    leafletProxy("mymap", session) %>%
      clearShapes() %>%
      addPolygons(lng = polygon_data$lng,
                  lat = polygon_data$lat,
                  popup = paste("New Polygon for", input$polygon_choice))
  })
}

shinyApp(ui, server)
