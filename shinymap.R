library(shiny)
library(leaflet)
library(sf)
library(glue)
library(data.table)
library(dplyr)
library(plotly)
library(readxl)
library(janitor)
library(DT)

file_location <- "https://martinjc.github.io/UK-GeoJSON/json/sco/topo_lad.json"
gedf <- st_read(file_location) |> 
  mutate()

dbase <- "./2022.xlsx" |> 
  read_excel(sheet = "Table 2", skip = 3) |>
  clean_names() |> 
  as.data.table() |> 
  filter(area_name != "Scotland") |> 
  mutate(area_name = ifelse(area_name=="Na h-Eileanan Siar", "Eilean Siar", area_name)) |> 
  dplyr::select(-area_type)

mergescot <- gedf |> inner_join(dbase, by=c("LAD13NM" = "area_name")) |> 
  dplyr::select(-area_code, -id, -LAD13CD, -LAD13CDO, -LAD13NMW)

# join data sets
options <- setdiff(colnames(mergescot), c("LAD13NM", "area_name", "geometry"))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("ddAge", "Stratification:", 
                  choices = options, selectize = FALSE),
      numericInput("num", "Number of rows to show data for",
                   3, 1, 10),
    ),
    mainPanel(
      leafletOutput("mymap"),
      DT::dataTableOutput("pts_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$selected_items <- renderUI({
    if (is.null(input$board_specialty)) return(NULL)
    tags$ul(
      lapply(input$board_specialty, function(board) {
        tags$div(HTML(paste("<label style='display:none;'>", board, "</label>")))
      })
    )
  })
  
  filtered_data <- reactive({
    tdf <- mergescot |>  dplyr::select(LAD13NM, population = input$ddAge, geometry) |> 
      arrange(desc(population))
    return(tdf)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data = filtered_data()) |>
      addProviderTiles(provider = providers$CartoDB.DarkMatterNoLabels) |> 
      
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", population)(population),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "yellow",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~glue("{LAD13NM} {population}"),
        labelOptions = labelOptions(
          # style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) |> 
      addLegend(
        pal = colorQuantile("YlOrRd", NULL),
        values = ~population,
        opacity = 0.7,
        title = "Population",
        position = "bottomright"
      )
  })
  
  # observe({
  #   tdf <- mergescot |>  dplyr::select(LAD13NM, population = input$ddAge, geometry) |> 
  #     arrange(desc(population))
  #   
  #   leafletProxy("mymap", session) %>%
  #     clearShapes() |> 
  #     addPolygons(
  #       data = tdf,
  #       fillColor = ~colorQuantile("YlOrRd", population)(population),
  #       weight = 2,
  #       opacity = 1,
  #       color = "white",
  #       dashArray = "3",
  #       fillOpacity = 0.7,
  #       highlightOptions = highlightOptions(
  #         weight = 5,
  #         color = "yellow",
  #         dashArray = "",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE
  #       ),
  #       label = ~glue("{LAD13NM} {population}"),
  #       labelOptions = labelOptions(
  #         # style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px",
  #         direction = "auto"
  #       )
  #     )
  # })
  # 
  output$pts_table <- DT::renderDataTable({
    filtered_data()
  }, options = list(pageLength = input$num))  # Set number of rows per page
  
}

# Run the application
shinyApp(ui = ui, server = server)