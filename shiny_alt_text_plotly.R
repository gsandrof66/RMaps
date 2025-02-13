library(shiny)
library(bslib)
library(data.table)
library(dplyr)
library(dtplyr)
library(glue)
library(sf)
library(plotly)

shapefile <- st_read("./SG_NHS_HealthBoards_2019.shp") |> rename(HB = HBCode) |> 
  st_transform(crs = 4326) |> 
  st_set_geometry(NULL) |> 
  dplyr::select(HB, HBName)

data <- fread("./beds_by_nhs_board_of_treatment_and_specialty.csv",
              select = c("Quarter", "HB", "Location", "Specialty", "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
  arrange(Quarter, HB, Location) |> 
  filter(!is.na(PercentageOccupancy))

my_result <- function(data, type, year){
  return(final)    
}

excluded <- c("2019Q2", "2019Q3", "2019Q4","2020Q1","2020Q2", "2020Q3", "2020Q4", "2021Q1")

datafull <- data |> filter(!Quarter %in% excluded) |> 
  mutate(isFull = PercentageOccupancy==100.0) |> 
  summarise(num = n(), .by=c("Quarter", "HB", "isFull")) |> 
  as.data.table()

dtemp <- datafull |> 
  summarise(total = sum(num), .by=c(Quarter, HB))

final <- datafull |> 
  merge(dtemp, by=c("Quarter", "HB"), all.x = T) |> 
  filter(isFull) |> 
  mutate(percFull = (num*100)/total) |> 
  mutate(across(c(percFull), round, 2))

final <- shapefile |> 
  merge(final, by = "HB", all = FALSE) |> 
  dplyr::select(Quarter, HBName, percFull) |> 
  arrange(Quarter, HBName)

bootswatch_themes <- c(
  "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", 
  "lux", "materia", "minty", "morph", "pulse", "quartz", "sandstone", "simplex", 
  "sketchy", "slate", "solar", "spacelab", "superhero", "united", "vapor", "yeti", "zephyr"
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "vapor"),
  titlePanel("Dark Mode Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Choose a Bootswatch theme:", choices = bootswatch_themes,
                  selected = "vapor"),
      selectizeInput("board_name", "Select up to 3",
                     choices = sort(unique(shapefile$HBName)),
                     multiple = TRUE,
                     selected = "Scotland",
                     options = list(maxItems = 4))
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

server <- function(input, output, session){
  observe({
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
  
  output$plot <- renderPlotly({
    if(length(input$board_name)>0){
      dt_final <- final |> filter(HBName %in% input$board_name)  
    }else{
      dt_final <- final
    }
    
    p <- plot_ly(dt_final, x = ~Quarter, y = ~percFull, color = ~HBName, 
                 type = 'scatter', mode = 'lines+markers',
                 text = ~paste(Quarter, '<br>', HBName, '<br>', percFull, '%'),
                 hoverinfo = 'text') |> 
      layout(title = "Quarter Series Plot",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Percentage full beds hosp"),
             plot_bgcolor = 'black', 
             paper_bgcolor = 'black',
             font = list(color = 'white'))
    
    p <- htmlwidgets::onRender(p, "
      function(el, x) {
        el.setAttribute('aria-label', 'Time series full bed occupancy');
      }")
    p
  })
}
shinyApp(ui, server)
