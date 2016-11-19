#' Get Google Maps cycling distance and duration estimates for pairs of
#' stations in the Bay Area Bike Share system
#'
#' @description Gets all distance and duration estimates for pairs of
#' stations in the BABS that are in the same city (e.g. estimates between
#' a station in SF and one in Palo Alto are not calculated)
#' @param path, root directory of project (.R files expected to be in ./R/ dir relative to path)
#' @param fn, filename of output file relative to path
#' @return dataframe with estimates

getGoogleMapsCyclingEstimates <- function(path = "~/Documents/Projects/BikeShare/", fn = "data/gmaps_trip_estimates.Rds") {
  
  # check if estimates have already been pulled
  if (file.exists(paste0(path, fn))) {
    return(readRDS(paste0(path, fn)))
  }
  
  # hit the API if estimates haven't already been pulled
  library(httr)
  library(readr)
  library(dplyr)
  library(stringr)
  
  source(paste0(path, "R/config.R")) # config.R should define API_KEY for Google Maps Directions API
  source(paste0(path, "R/helpers.R"))
  
  # read station data
  paths <- getFilePaths()
  stations <- do.call("rbind", lapply(paths$station, read_csv))
  
  # process
  stations <- stations %>%
    select(station_id, lat, long, landmark, installation) %>%
    group_by(station_id, lat, long, landmark, installation) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(latlong = paste(lat, long, sep = ","))
  
  # Five stations temporarily moved location, so I can't just join trips with station data
  # on station_id to match trips to start and end latlong. I include start_date_origin/destination 
  # and end_date_origin/destination columns to solve this problem. These columns will be NA by
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
                  "37.780356,-122.412919" = c(as.Date("2014-09-01"), as.Date("2015-02-12")))
  for (nm in names(changes)){
    stations$start_date[stations$latlong == nm] <- changes[[nm]][1]
    stations$end_date[stations$latlong == nm] <- changes[[nm]][2]
  }
  
  # get all pairs of stations within the same landmark -- this gets all
  # the origin/destination latlong pairs needed to pass to the API
  station_pairs <- inner_join(stations, stations, by = "landmark") %>%
    filter(latlong.x != latlong.y) %>%
    select(station_id.x, station_id.y, latlong.x, latlong.y, 
           start_date.x, start_date.y, end_date.x, end_date.y, landmark) %>%
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
    cat(paste(i, "of", nrow(station_pairs), "\r"))
    c(r$routes[[1]]$legs[[1]]$duration$value, r$routes[[1]]$legs[[1]]$distance$value)
  })
  
  # append to station pairs
  station_pairs <- station_pairs %>% 
    mutate(duration = time_dist_pairs[1,], distance = time_dist_pairs[2,])
  
  saveRDS(station_pairs, file = paste0(path, fn)) 
  return(station_pairs)
  
}