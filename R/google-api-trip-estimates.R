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
  
  # config.R should define API_KEY for Google Maps Directions API
  source(paste0(path, "R/config.R")) 
  source(paste0(path, "R/helpers.R"))
  
  # read station data
  paths <- getFilePaths()
  stations <- getAllStationData()
  
  # get all pairs of stations within the same landmark as I only want
  # to analyze trips made within SF, SJ, etc. and not trips between PA->SF, etc.
  # the origin/destination latlong pairs needed to pass to the API
  station.pairs <- inner_join(stations, stations, by = "landmark") %>%
    filter(latlong.x != latlong.y) %>%
    select(station_id.x, station_id.y, latlong.x, latlong.y, 
           start_date.x, start_date.y, end_date.x, end_date.y, landmark) %>%
    rename(origin_id = station_id.x, destination_id = station_id.y,
           origin_latlong = latlong.x, destination_latlong = latlong.y,
           start_date_origin = start_date.x, start_date_destination = start_date.y,
           end_date_origin = end_date.x, end_date_destination = end_date.y)
  
  # make api calls
  time.dist.pairs <- sapply(1:nrow(station.pairs), function(i) {
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
  station.pairs <- station.pairs %>% 
    mutate(duration = time.dist.pairs[1,], distance = time.dist.pairs[2,])
  
  saveRDS(station.pairs, file = paste0(path, fn)) 
  return(station.pairs)
  
}