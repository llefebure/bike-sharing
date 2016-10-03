library(httr)
library(readr)
library(dplyr)

setwd("~/Documents/Projects/BikeShare/")
source("R/config.R")
source("R/helpers.R")

paths <- getFilePaths()
stations <- rbind(read_csv(paths$station[2]),
                  read_csv(paths$station[3]))
stations <- stations %>%
  select(station_id, lat, long, landmark) %>%
  group_by(station_id, lat, long, landmark) %>%
  filter(row_number() == 1) %>%
  ungroup()

gmaps.dist.matrix.url <- paste0("https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=Washington,DC&destinations=New+York+City,NY&key=", API_KEY)
resp <- GET(url = gmaps.dist.matrix.url)
content(resp, as = "text")