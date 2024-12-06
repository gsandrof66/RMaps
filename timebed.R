library(data.table)
library(dplyr)
library(dtplyr)
library(glue)
library(sf)
library(plotly)

shapefile <- st_read("./SG_NHS_HealthBoards_2019.shp") |> rename(HB = HBCode)
shapefile <- st_transform(shapefile, crs = 4326)

data <- fread("./beds_by_nhs_board_of_treatment_and_specialty.csv",
              select = c("Quarter", "HB", "Location", "Specialty", "SpecialtyName", "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
  arrange(Quarter) |> 
  filter(!is.na(PercentageOccupancy))

my_result <- function(data, type, year){
  return(final)    
}

hbcodes = c("S08000025", "S08000016", "S08000031")
            # , "S08000017")


# How many hospital were full per HB
datafull <- data |> filter(HB %in% hbcodes) |> 
  mutate(isFull = PercentageOccupancy==100.0) |> 
  group_by(Quarter, HB, isFull) |> 
  summarise(num = n(), .groups = "drop")

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

glimpse(final)
fig <- plot_ly(final, x = ~Quarter, y = ~percFull, color = ~HBName, 
               type = 'scatter', mode = 'lines+markers',
               text = ~paste(Quarter, '<br>', HBName, '<br>', percFull, '%'),
               hoverinfo = 'text') |> 
  layout(title = "Quarter Series Plot",
         xaxis = list(title = "Quarter"),
         yaxis = list(title = "Percentage full beds hosp"),
         plot_bgcolor = 'black', 
         paper_bgcolor = 'black',
         font = list(color = 'white'))
fig

