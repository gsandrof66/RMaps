library(data.table)
library(dplyr)
library(plotly)

df <- fread("https://query.data.world/s/evttstxtsvrim4mmge5ycs5gpil2nb?dws=00000")
# |> clean_names()
glimpse(df)

df <- df |> filter(`Country/Region` == "United Kingdom")
print(nrow(df))
glimpse(df)

df <- df |> filter(`Province/State` == "British Virgin Islands")
nrow(df)

ndf <- df |> select(-Lat, -Long) |>
  melt(id.vars = c(1,2), variable.name = "dates", value.name = "amount", variable.factor=F) |> 
  mutate(dates = as.IDate(dates, format = "%m/%d/%y")) |> 
  clean_names() |> 
  arrange(dates)
glimpse(ndf)

ggplot(ndf, aes(x = dates, y = amount)) +
  geom_line() +                # Line plot
  geom_point() +               # Points at each observation
  labs(title = "Time Series Plot",
       x = "Date",
       y = "Value") +
  theme_dark() 

my_plot <- plot_ly(ndf, x = ~dates, y = ~amount, type = 'scatter', mode = 'lines+markers', 
                   line = list(color = 'cyan'), marker = list(color = 'yellow')) |>
  layout(title = "Time Series Plot",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Value"),
         plot_bgcolor = 'black',   # Background color
         paper_bgcolor = 'black',   # Paper color
         font = list(color = 'white'))  # Font color
my_plot

