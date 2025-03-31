library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(glue)
library(data.table)
library(dplyr)
library(plotly)
library(readxl)
library(janitor)
library(DT)

shapefile <- st_read("./SG_NHS_HealthBoards_2019.shp") |> rename(HB = HBCode)
shapefile <- st_transform(shapefile, crs = 4326)

data <- fread("./beds_by_nhs_board_of_treatment_and_specialty.csv",
              select = c("Quarter", "HB", "Location", "Specialty", "SpecialtyName", "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
  arrange(Quarter) |> 
  filter(!is.na(PercentageOccupancy)) |> 
  mutate(y = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=TRUE, keep=1))),
         q = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=TRUE, keep=2))))

my_result <- function(data, type, year){
  if(type=="normal"){
    d3 <- data |> filter(y==year)
  }else if(type=="financial"){
    quarters <- glue("Q{seq(4)}")
    quarters <- c(glue("{year}{quarters[2]}"),
                  glue("{year}{quarters[3]}"),
                  glue("{year+1}{quarters[4]}"),
                  glue("{year+1}{quarters[1]}"))
    d3 <- data |> filter(Quarter %in% quarters)
  }
  
  # How many hospital were full per HB
  datafull <- d3 |> 
    mutate(
      # level = case_when(
      # PercentageOccupancy >= 0 & PercentageOccupancy < 34 ~ "low",
      # PercentageOccupancy >= 34 & PercentageOccupancy < 67 ~ "medium",
      # PercentageOccupancy >= 67 & PercentageOccupancy <= 100 ~ "high",
      # TRUE ~ NA_character_),
      isFull = PercentageOccupancy==100.0) |> 
    group_by(HB, isFull) |> 
    summarise(num = n(), .groups = "drop")
  
  dtemp <- datafull |> 
    summarise(total = sum(num), .by=c(HB))
  
  final <- datafull |> 
    left_join(dtemp, by=c("HB")) |> 
    filter(isFull) |> 
    mutate(percFull = (num*100)/total) |> 
    mutate(across(c(percFull), round, 2))
  
  final <- shapefile |> inner_join(final, by=c("HB")) |> 
    arrange(desc(percFull))
  
  # Identify the 3 highest and 3 lowest values
  top_3 <- head(final[order(-final$percFull), ], 3)
  bottom_3 <- head(final[order(final$percFull), ], 3)
  legend_values <- c(top_3$percFull, bottom_3$percFull)
  legend_labels <- c(top_3$HBName, bottom_3$HBName)
  pal <- colorNumeric("YlOrRd", domain = final$percFull)    
  
  my_list <- list(final=final, legend_values=legend_values, 
                  legend_labels=legend_labels, pal=pal)
  return(my_list)    
}

bootswatch_themes <- c("flatly", "vapor")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "vapor"),
  title = "Demo map",
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Choose a theme:", 
                  choices = bootswatch_themes,
                  selectize = FALSE,
                  selected = "vapor"),
      selectInput("ddtype", "Type of year:", 
                  choices = c("normal", "financial")),
      selectInput("ddyear", "Year", 
                  choices = seq(2019,2024),
                  selected = "2023"),
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
server <- function(input, output, session) {
  observe({
    session$setCurrentTheme(
      bs_theme(bootswatch = input$theme)
    )
  })
  
  res_final <- reactive({
    return(my_result(data, input$ddtype, as.integer(input$ddyear)))
  })
  
  output$mymap <- renderLeaflet({
    final <- res_final()
    leaflet(final[["final"]]) |> 
      addProviderTiles(provider = providers$CartoDB.DarkMatter) |> 
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", percFull)(percFull),
        weight = 1, opacity = 1, color = "white", dashArray = "3",
        fillOpacity = 0.7,
        label = ~glue("{HBName}: {percFull}"),
        labelOptions = labelOptions(textsize = "16px", direction = "auto")
      ) |> 
      addLegend("bottomright", 
                pal = final[["pal"]], 
                values = final[["legend_values"]], 
                labels = final[["legend_labels"]], 
                title = "% full hospital",
                opacity = 1)
  })
  
  output$pts_table <- DT::renderDataTable({
    final <- res_final()
    st_set_geometry(final[["final"]], NULL) |> 
      select(Health_board = HBName, percentage_full_hosps=percFull)
  }, style = 'bootstrap', rownames = F, 
  filter = "top",
  options = list(pageLength = input$num, # Set number of rows per page
                 dom = 'tip',
                 autoWidth = T
                 # columnDefs = list(list(targets = c(5), className = 'dt-right'))
                 ),
  colnames = c("Health boards", "Full capacity")
  )
}

# Run the application
shinyApp(ui = ui, server = server)