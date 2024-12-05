# https://publichealthscotland.scot/publications/acute-hospital-activity-and-nhs-beds-information-annual/acute-hospital-activity-and-nhs-beds-information-annual-year-ending-31-march-2024/
# https://www.opendata.nhs.scot/dataset/hospital-beds-information

library(data.table)
library(dplyr)
library(dtplyr)
library(glue)

data <- fread("./beds_by_nhs_board_of_treatment_and_specialty.csv",
              select = c("Quarter", "HB", "Location", "Specialty", "SpecialtyName", "TotalOccupiedBeddays", "PercentageOccupancy")) |> 
  # select(-ends_with("QF"),-AllStaffedBeddays, =AverageAvailableStaffedBeds) |> 
  arrange(Quarter) |> 
  filter(!is.na(PercentageOccupancy)) |> 
  mutate(y = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=TRUE, keep=1))),
         q = as.integer(unlist(tstrsplit(Quarter, "Q", fixed=TRUE, keep=2))))

d3 <- data |> filter(y==2023) |> 
  mutate(level = case_when(
    between(PercentageOccupancy, 0.0, 33.9999) ~ "low",
    between(PercentageOccupancy, 34.0, 66.9999) ~ "medium",
    between(PercentageOccupancy, 67.0, 100.0) ~ "high",
    TRUE ~ NA_character_),
    isFull = PercentageOccupancy==100.0)

# How many hospital were full per HB
datafull <- d3 |> 
  group_by(HB, isFull) |> 
  summarise(num = n(), .groups = "drop")

dtemp <- datafull |> 
  summarise(total = sum(num), .by=c(HB))

datafull <- datafull |> 
  left_join(dtemp, by=c("HB")) |> 
  filter(isFull) |> 
  mutate(percFull = (num*100)/total) |> 
  arrange(desc(percFull))

glimpse(datafull)


quarters <- glue("Q{seq(4)}")
years <- as.character(seq(2019, 2024))


glimpse(d3)
unique(d3$PercentageOccupancy)

