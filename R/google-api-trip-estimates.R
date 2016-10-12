# this script hits the google maps directions api to get cycling distance and duration
# estimates between stations in the BABS

library(httr)
library(readr)
library(dplyr)
library(stringr)

setwd("~/Documents/Projects/BikeShare/")
source("R/config.R")
source("R/helpers.R")

# read data
paths <- getFilePaths()
stations <- do.call("rbind", lapply(paths$station, read_csv))

# process
stations <- stations %>%
  select(station_id, lat, long, landmark, installation) %>%
  group_by(station_id, lat, long, landmark, installation) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(latlong = paste(lat, long, sep = ","))

# five stations moved but retained station_id, so I need to duplicate rows for these stations
# and include start_date and end_date columns to correctly match. These columns will be NA by
# default which means that if a date doesn't match those with non-NA start/end_date
# columns, it should match to the one with NA values.
stations <- stations %>%
  mutate(start_date = as.Date(NA), end_date = as.Date(NA)) %>%
  rbind(data.frame(station_id = 72,
                   lat = 37.780353,
                   long = -122.41226,
                   landmark = "San Francisco",
                   installation = c("8/23/2013"),
                   latlong = "37.780353,-122.41226",
                   start_date = as.Date("2015-02-13"),
                   end_date = as.Date("2015-06-03")))
changes <- list("37.488501,-122.231061" = c(as.Date("2014-09-01"), as.Date("2014-10-22")),
                "37.486725,-122.225551" = c(as.Date("2014-09-01"), as.Date("2014-10-22")),
                "37.789625,-122.390264" = c(as.Date("2014-09-01"), as.Date("2015-02-05")),
                "37.776377,-122.39607" = c(as.Date("2014-09-01"), as.Date("2015-03-11")),
                "37.776377,-122.39607" = c(as.Date("2014-09-01"), as.Date("2015-03-11")))
for (nm in names(changes)){
  stations$start_date[stations$latlong == nm] <- changes[[nm]][1]
  stations$end_date[stations$latlong == nm] <- changes[[nm]][2]
}

# get pairs
station_pairs <- inner_join(stations, stations, by = "landmark") %>%
  filter(latlong.x != latlong.y) %>%
  select(station_id.x, station_id.y, latlong.x, latlong.y, 
         start_date.x, start_date.y, end_date.x, end_date.y) %>%
  rename(origin_id = station_id.x, destination_id = station_id.y,
         origin_latlong = latlong.x, destination_latlong = latlong.y,
         start_date_origin = start_date.x, start_date_destination = start_date.y,
         end_date_origin = end_date.x, end_date_destination = end_date.y)

# make api calls
time_dist_pairs <- sapply(1:nrow(station_pairs), function(i) {
  oll <- station_pairs$origin_latlong[i]
  dll <- station_pairs$destination_latlong[i]
  url <- paste0("https://maps.googleapis.com/maps/api/directions/json?origin=", oll, 
                "&destination=", dll, "&mode=bicycling&key=", API_KEY)
  resp <- GET(url = url)
  r <- content(resp)
  cat(paste(i, "of", nrow(station_pairs)))
  c(r$routes[[1]]$legs[[1]]$duration$value, r$routes[[1]]$legs[[1]]$distance$value)
})

# append to station pairs
station_pairs <- station_pairs %>% 
  mutate(duration = time_dist_pairs[1,], distance = time_dist_pairs[2,])

saveRDS(station_pairs, file = "./data/gmaps_trip_estimates.Rds")
