library(tidytuesdayR)
library(lubridate)


base <- tt_load('2024-10-15')
orcas <- base$orcas
glimpse(orcas)
orcas <- orcas |> 
  mutate(pods_or_ecotype = toupper(pods_or_ecotype),
         diftime2 = as.duration(interval(begin_time, end_time)))

sort(unique(orcas$pods_or_ecotype))
