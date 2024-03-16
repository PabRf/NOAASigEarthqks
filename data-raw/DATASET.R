## Create data objects from raw data
library(readr)
earthquakes_rawData = read_csv("earthquakes_-2150to2024.csv"); range(earthquakes_rawData$Year)

## Add internal data object(s): generated data files saved in R/ subdirectory
usethis::use_data(earthquakes_rawData, internal = T, overwrite = T)
