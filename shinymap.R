library(shiny)
library(shinyDarkmode)
library(leaflet)
library(sf)
library(glue)
library(data.table)
library(dplyr)
library(plotly)
library(janitor)
library(DT)

file_location <- "https://martinjc.github.io/UK-GeoJSON/json/sco/topo_lad.json"
gedf <- st_read(file_location)
df <- data.table("LAD13NM" = c("Glasgow City", "City of Edinburgh", "Fife", "North Lanarkshire", "South Lanarkshire", "Aberdeenshire", "Highland", "Aberdeen City", "West Lothian", "Renfrewshire", "Falkirk", "Perth and Kinross", "Dumfries and Galloway", "Dundee City", "North Ayrshire", "East Ayrshire", "Angus", "Scottish Borders", "South Ayrshire", "East Lothian", "East Dunbartonshire", "East Renfrewshire", "Moray", "Midlothian", "Stirling", "West Dunbartonshire", "Argyll and Bute", "Inverclyde", "Clackmannanshire", "Na h-Eileanan Siar", "Shetland Islands", "Orkney Islands"),
                 "population" = c(635130, 526470, 374730, 341400, 322630, 262690, 238060, 227430, 185580, 179940, 160700, 153810, 148790, 148060, 135280, 121840, 116260, 115270, 112610, 106370, 106370, 95170, 95510, 92150, 94330, 89590, 86890, 78150, 51400, 26830, 23080, 22500))

setdiff(sort(unique(df$LAD13NM)),
        sort(unique(gedf$LAD13NM)))

setdiff(sort(unique(gedf$LAD13NM)),
        sort(unique(df$LAD13NM)))

df |> filter(LAD13NM == "Na h-Eileanan Siar")

df <- df |> mutate(LAD13NM = ifelse(LAD13NM == "Na h-Eileanan Siar", 
                                    "Eilean Siar", LAD13NM))

# join data sets
mergescot <- inner_join(gedf, df, by=c("LAD13NM")) |> 
  dplyr::select(LAD13NM, population, geometry) |> 
  arrange(desc(population))

ui <- fluidPage(
  #use_darkmode(),
  sidebarLayout(
    sidebarPanel(
      selectInput("ddAge", "Stratification:", 
                  choices = c("All", "kids")),
      numericInput("num", "Number of rows to show data for",
                   5, 1, 20)
    ),
    mainPanel(
      leafletOutput("mymap"),
      DT::dataTableOutput("pts_table")
      # tableOutput("ts_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # darkmode()
  filtered_data <- reactive({
    # temp dataset
    if(input$ddAge == "All"){
      tdf <- df 
    }else{
      tdf <- df 
    }
    return(tdf)
  })
  # Create a plot of the "cars" dataset 
  output$mymap <- renderLeaflet({
    leaflet(mergescot) |> 
      # addTiles() |> 
      #addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
      #         attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors © <a href="https://carto.com/attributions">CARTO</a>') |> 
      addProviderTiles(provider = providers$CartoDB.DarkMatter) |> 
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