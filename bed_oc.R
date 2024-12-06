# https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-annual/acute-hospital-activity-and-nhs-beds-information-annual-year-ending-31-march-2024/
# https://www.opendata.nhs.scot/dataset/hospital-beds-information
library(data.table)
library(dplyr)
library(dtplyr)
library(glue)
library(sf)
library(leaflet)
library(leaflet.extras)

# Read the shapefile
# https://spatialdata.gov.scot/geonetwork/srv/api/records/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
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

final <- my_result(data, "financial", 2023)

# glimpse(final[["final"]])

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

# Golden Jubilee National Hospital and other
# "SB0801"    "S92000003" missing
# setdiff(unique(final$HB), unique(shapefile$HBCode))



