# install.packages('sf')
# install.packages('leaflet')
# install.packages("leaflet.extras")
library(leaflet)
# library(leaflet.extras)
library(sf)
library(data.table)
library(dplyr)
library(glue)

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

# US parks
# source: https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-10-08/readme.md
most_visited_nps_species_data <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv' |> 
  fread()

setdiff(unique(most_visited_nps_species_data$ParkCode),
        unique(data$UNIT_CODE))

data <- "https://gist.githubusercontent.com/erincaughey/2f221501645757e28b715c4063e87595/raw/a90be1b434b1a8cdf71c2abc3373ca63987e2d23/nps-geo-boundary.json" |> 
  st_read(as_tibble = T) |> dplyr::select(UNIT_CODE, UNIT_NAME, geometry)

us <- 'https://gist.githubusercontent.com/erincaughey/4ea1aaceeb4989bc4700bd248bb664a7/raw/8ebb3b3953c141b560b704d5054b2fa122eaadd5/us-census17-geo.json' |> 
  st_read(as_tibble = T) |> dplyr::select(NAME, geometry)

leaflet(data) |> 
  addProviderTiles(provider = providers$CartoDB.DarkMatter) |> 
  addPolygons(
    # fillColor = ~colorQuantile("YlOrRd", population)(population),
    weight = 2,
    opacity = 1,
    color = "pink",
    dashArray = "3",
    fillOpacity = 0.9,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "yellow",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~glue("{UNIT_NAME}"),
    labelOptions = labelOptions(
      # style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )
  



