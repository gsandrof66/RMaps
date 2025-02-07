library(shiny)
library(data.table)
library(dplyr)
library(dtplyr)
library(glue)
library(sf)
library(plotly)

shapefile <- st_read("./SG_NHS_HealthBoards_2019.shp") |> rename(HB = HBCode) |> 
  st_transform(crs = 4326)

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
  summarise(num = n(), .by=c("Quarter", "HB", "isFull"))

dtemp <- datafull |> 
  summarise(total = sum(num), .by=c(Quarter, HB))

final <- datafull |> 
  left_join(dtemp, by=c("Quarter", "HB")) |> 
  filter(isFull) |> 
  mutate(percFull = (num*100)/total) |> 
  mutate(across(c(percFull), round, 2))

final <- st_set_geometry(shapefile, NULL) |> 
  dplyr::select(HB, HBName) |> 
  inner_join(final, by=c("HB")) |> 
  select(Quarter, HBName, percFull) |> 
  arrange(Quarter, HBName)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
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

server <- function(input, output) {
  output$plot <- renderPlotly({
    
    p <- plot_ly(final, x = ~Quarter, y = ~percFull, color = ~HBName, 
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
