library(shiny)
library(shinyDarkmode)
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
gedf <- st_read(file_location)
dbase <- "./2022.xlsx" |> 
  read_excel(sheet = "Table 2", skip = 3) |>
  clean_names() |> 
  as.data.table() |> 
  filter(area_name != "Scotland") |> 
  dplyr::select(-area_type)

# join data sets
mergescot <- inner_join(gedf, dbase, by=c("id"="area_code")) |> 
  dplyr::select(-id, -LAD13CD, -LAD13CDO, -LAD13NMW, -area_name)

options <- setdiff(colnames(mergescot), c("LAD13NM", "area_name", "geometry"))

ui <- fluidPage(
  use_darkmode(),
  sidebarLayout(
    sidebarPanel(
      selectInput("ddAge", "Stratification:", 
                  choices = options),
      numericInput("num", "Number of rows to show data for",
                   3, 1, 10)
    ),
    mainPanel(
      leafletOutput("mymap"),
      DT::dataTableOutput("pts_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  darkmode()
  filtered_data <- reactive({
    if(input$ddAge == "all_persons"){
      tdf <- mergescot |>  dplyr::select(population = all_persons, LAD13NM, geometry) |> 
        arrange(desc(population))
    }else{
      tdf <- mergescot |>  dplyr::select(population = input$ddAge, LAD13NM, geometry) |> 
        arrange(desc(population))
    }
    
    return(tdf)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data = filtered_data()) |> 
      # providers$CartoDB.DarkMatter
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
  
  output$pts_table <- DT::renderDataTable({
    filtered_data()
  }, options = list(pageLength = input$num))  # Set number of rows per page
  
}

# Run the application
shinyApp(ui = ui, server = server)