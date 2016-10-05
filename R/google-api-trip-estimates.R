library(httr)
library(readr)
library(dplyr)
library(stringr)

setwd("~/Documents/Projects/BikeShare/")
source("R/config.R")
source("R/helpers.R")

# read data
paths <- getFilePaths()
stations <- rbind(read_csv(paths$station[2]),
                  read_csv(paths$station[3]))

# process
stations <- stations %>%
  select(station_id, lat, long, landmark) %>%
  group_by(station_id, lat, long, landmark) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(latlong = paste(lat, long, sep = ","))

# get pairs
station_pairs <- inner_join(stations, stations, by = "landmark") %>%
  filter(latlong.x != latlong.y) %>%
  select(station_id.x, station_id.y, latlong.x, latlong.y) %>%
  rename(origin_id = station_id.x, destination_id = station_id.y,
         origin_latlong = latlong.x, destination_latlong = latlong.y)



# make api calls
time_dist_pairs <- sapply(1:nrow(station_pairs), function(i) {
  oll <- station_pairs$origin_latlong[i]
  dll <- station_pairs$destination_latlong[i]
  url <- paste0("https://maps.googleapis.com/maps/api/directions/json?origin=", oll, 
                "&destination=", dll, "&mode=bicycling&key=", API_KEY)
  resp <- GET(url = url)
  r <- content(resp)
  print(paste(i, "of", nrow(station_pairs)))
  c(r$routes[[1]]$legs[[1]]$duration$value, r$routes[[1]]$legs[[1]]$distance$value)
})

# append to station pairs
station_pairs <- station_pairs %>% 
  mutate(duration = time_dist_pairs[1,], distance = time_dist_pairs[2,])

saveRDS(station_pairs, file = "./data/gmaps_trip_estimates.R")
