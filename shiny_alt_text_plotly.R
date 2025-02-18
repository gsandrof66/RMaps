library(shiny)
library(bslib)
library(data.table)
library(dplyr)
library(dtplyr)
library(sf)
library(plotly)

# shapefile <- st_read("./SG_NHS_HealthBoards_2019.shp") |> rename(HB = HBCode) |> 
#   st_transform(crs = 4326) |> 
#   st_set_geometry(NULL) |> 
#   dplyr::select(HB, HBName) |> 
#   arrange(HBName)

shapefile <- fread("./hb_codes.csv") |> 
  arrange(HBName)

hb_choices <- setNames(shapefile$HB, shapefile$HBName)

# data <- fread("./beds_by_nhs_board_of_treatment_and_specialty.csv",
#               select = c("Quarter", "HB", "Location", "Specialty", "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
#   arrange(Quarter, HB, Location) |> 
#   filter(!is.na(PercentageOccupancy))
# 
# my_result <- function(data, type, year){
#   return(final)    
# }
# 
# excluded <- c("2019Q2", "2019Q3", "2019Q4","2020Q1","2020Q2", "2020Q3", "2020Q4", "2021Q1")
# 
# datafull <- data |> filter(!Quarter %in% excluded) |> 
#   mutate(isFull = PercentageOccupancy==100.0) |> 
#   summarise(num = n(), .by=c("Quarter", "HB", "isFull")) |> 
#   as.data.table()
# 
# dtemp <- datafull |> 
#   summarise(total = sum(num), .by=c(Quarter, HB))
# 
# final <- datafull |> 
#   merge(dtemp, by=c("Quarter", "HB"), all.x = T) |> 
#   filter(isFull) |> 
#   mutate(percFull = (num*100)/total) |> 
#   mutate(across(c(percFull), round, 2))
# 
# final <- shapefile |> 
#   merge(final, by = "HB", all = FALSE) |> 
#   dplyr::select(Quarter, HBName, percFull) |> 
#   arrange(Quarter, HBName)

final <- fread("./total_bed.csv") |> 
  arrange(Quarter)

# bootswatch_themes <- c(
#   "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "litera", "lumen", 
#   "lux", "materia", "minty", "morph", "pulse", "quartz", "sandstone", "simplex", 
#   "sketchy", "slate", "solar", "spacelab", "superhero", "united", "vapor", "yeti", "zephyr"
# )
bootswatch_themes <- c("flatly", "vapor")

ui <- tags$html(
  lang = "en",
  fluidPage(
    theme = bs_theme(bootswatch = "vapor"),
    title = "Main Page",
    h1("Shiny website", style = "text-align: center;"),
    titlePanel("Choose your options"),
    sidebarLayout(
      sidebarPanel(
        selectInput("theme", "Choose a theme:", 
                    choices = bootswatch_themes,
                    selectize = FALSE,
                    selected = "vapor"),
        
        selectInput("board_name", "Choose a board name:",
                    choices = hb_choices,
                    multiple = TRUE),
        uiOutput("selected_items")
      ),
      mainPanel(
        plotlyOutput("plot")
      )
    )
  )  
)

server <- function(input, output, session){
  observe({
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
  
  output$selected_items <- renderUI({
    if(is.null(input$board_name)) return(NULL)
    list_names <- shapefile[HB %in% input$board_name]$HBName
    tags$ul(
      lapply(list_names, function(board) {
        tags$div(HTML(paste("<label style='display:none;'>", board, "</label>")))
      })
    )
  })
  
  output$plot <- renderPlotly({
    p <- plot_ly(if(!is.null(input$board_name)){final[HB %in% input$board_name]}else{final}, 
                 x = ~Quarter, y = ~percFull, color = ~HBName, 
                 type = 'scatter', mode = 'lines+markers',
                 text = ~paste(Quarter, '<br>', HBName, '<br>', percFull, '%'),
                 hoverinfo = 'text') |> 
      layout(title = "Quarter Series Plot",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Percentage full beds hosp"),
             plot_bgcolor = 'black', 
             paper_bgcolor = 'black',
             font = list(color = 'white'))
    
    # if(is.null(input$board_name)){
    #   hb_names_combined <- "all boards"
    # }else{
    #   # list_names <- shapefile[HB %in% input$board_name]$HBName
    #   # hb_names_combined <- paste(citlist_namesies, collapse = ", ")
    #   hb_names_combined <- "some boards"
    # }
    # 
    # alt_msg <- paste0("Time series full bed occupancy per quarter for ", hb_names_combined)
    # htnk_alt <- paste0("function(el, x) {el.setAttribute('aria-label', '", alt_msg, "');}")
    # print(htnk_alt)
    p <- htmlwidgets::onRender(p,
                     "function(el, x) {el.setAttribute('aria-label', 'This plot shows time series full bed occupancy per quarter');}")
    p
  })
  
}

shinyApp(ui, server)
