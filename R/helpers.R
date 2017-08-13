library(dplyr)
library(readr)
library(lubridate)

#' Retrieve paths for the relevant data files
#' 
#' @param dir, directory where the raw data files live
#' @return a list containing full file paths under category names
getFilePaths <- function(dir = "~/Documents/Projects/BikeShare/data/"){
  prefixes <- c("201402", "201408", "201508", "201608")
  list(trip = paste0(dir, prefixes, "_trip_data.csv"),
       status = paste0(dir, prefixes, "_status_data.csv"),
       station = paste0(dir, prefixes, "_station_data.csv"),
       weather = paste0(dir, prefixes, "_weather_data.csv"))
}

#' Get all trip data
#' 
#' @return dataframe with all trip data
getAllTripData <- function(file.name = "data/all_trips.Rds") {
  
  # if file already exists, load from file
  if(file.exists(file.name)){
    all.trips <- readRDS(file.name)
    return(all.trips)
  }
  
  paths = getFilePaths()
  trip_data <- do.call("rbind", lapply(paths$trip, function(fn) {
    date_fmt <- ifelse(grepl("201402", fn), "%m/%d/%y %H:%M", "%m/%d/%Y %H:%M")
    read_csv(fn) %>%
      rename(origin_id = `Start Terminal`,
             destination_id = `End Terminal`,
             origin_station = `Start Station`,
             destination_station = `End Station`,
             subscription = `Subscriber Type`,
             actual_duration = Duration,
             trip_id = `Trip ID`) %>%
      mutate(time = as.POSIXct(`Start Date`, format = date_fmt),
             year = format(time, "%Y"),
             month = format(time, "%m"),
             day = format(time, "%d"),
             hour = format(time, "%H"),
             minute = format(time, "%M"),
             weekday = ifelse(format(time, "%a") %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>%
      select(trip_id, origin_id, destination_id, origin_station, destination_station, subscription,
             actual_duration, time, year, month, day, hour, minute, weekday)
  }))
  return(trip_data)
}

#' Get Station Data
#' 
#' @return Retrieves station data
getAllStationData <- function() {
  
  paths <- getFilePaths()
  
  stations <- do.call("rbind", lapply(paths$station, read_csv)) %>%
    filter(!is.na(station_id)) %>%
    group_by(station_id, name, lat, long, dockcount, landmark, installation) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(station_id, lat, long) %>%
    mutate(start_date = NA, end_date = NA)
  
  stations <- makeStationUpdates(stations)
  
  stations <- stations %>%
    mutate(latlong = paste(lat, long, sep = ","))
  
  return(stations)
  
}

#' Make updates to stations
#'
#' @return updated stations
makeStationUpdates <- function(stations){
  # Conflict Resolution (from READMEs):
  
  # Station 23: From 9/1/14 – 10/22/14: This station was located at (37.488501, -122.231061).
  # Station 25: From 9/1/14 – 10/22/14: This station was located at (37.486725, -122.225551). It was previously named “Broadway at Main.”
  # Station 49: From 9/1/14 - 2/5/15: This station was located at (37.789625, -122.390264). 
  # Station 69: From 9/1/14 – 3/11/15: This station was located at (37.776377,-122.39607). 
  # Station 72: Moved twice. From 9/1/14 – 2/12/15, this station was located at (37.780356, -122.412919). From 2/13/15 to 6/3/15, the station was located at (37.780353, -122.41226).
  # Station 80: On 9/1/14, this station changed names from "San Jose Government Center" to "Santa Clara County Civic Center." It did not move.
  
  updates <- data.frame(matrix(c(23,37.488501,-122.231061,"9/1/14","10/22/14",
                                 25,37.486725,-122.225551,"9/1/14","10/22/14",
                                 49,37.789625,-122.390264,"9/1/14","2/5/15",
                                 69,37.776377,-122.39607,"9/1/14","3/11/15",
                                 72,37.780356,-122.412919,"9/1/14","2/12/15"),
                               nrow = 5, ncol = 5, byrow = T), stringsAsFactors = F)
  colnames(updates) <- c("station_id", "lat", "long", "start_date_update", "end_date_update")
  updates <- updates %>% mutate(station_id = as.integer(station_id),
                                lat = as.double(lat),
                                long = as.double(long),
                                start_date_update = mdy(start_date_update),
                                end_date_update = mdy(end_date_update))
  stations <- stations %>% 
    left_join(updates, by = c("station_id", "lat", "long")) %>%
    mutate(start_date = start_date_update, end_date = end_date_update) %>%
    select(station_id, name, lat, long, dockcount, landmark, installation,
           start_date, end_date)
  
  stations$start_date[stations$name == "Santa Clara County Civic Center"] <- mdy("9/1/14")
  
  s72_add <- stations %>% 
    filter(station_id == 72 & is.na(start_date)) %>%
    mutate(lat = 37.780353, long = -122.41226, 
           start_date = mdy("2/13/15"), end_date = mdy("6/3/15"))
  
  # Station 21: On 9/16/15, this station was renamed from "Franklin at Maple" to "Sequoia Hospital" and moved to (37.479303,-122.253755)
  # Station 26: On 9/16/15, this station was renamed from "Redwood City Medical Center" to "Kaiser Hospital" and moved to (37.489704,-122.224728)
  # Station 30: On 9/28/15, this station was renamed from "Evelyn Park and Ride" to "Middlefield Light Rail Station" and moved to (37.395337,-122.052476)
  # Station 33: On 9/16/15, this station was renamed from "Rengstorff Avenue / California Street" to "Charleston Park/ North Bayshore Area" and moved to (37.420909,-122.080623)
  # Station 73: Moved twice. From 3/14/16 – 5/19/16, this station was located at (37.797746, -122.407073). From 5/19/16 to 8/31/16, the station was located at (37.7979, -122.405942). The station name stayed the same for all moves. 
  # Station 83: On 9/16/15, this station was renamed from "Mezes Park" to "Mezes" and moved to (37.491405,-122.233051)
  
  s21_add <- stations %>% 
    filter(station_id == 21) %>%
    mutate(name = "Sequoia Hospital", lat = 37.479303, 
           long = -122.253755, start_date = mdy("9/16/15"))
  
  s26_add <- stations %>% 
    filter(station_id == 26) %>%
    mutate(name = "Kaiser Hospital", lat = 37.489704, 
           long = -122.224728, start_date = mdy("9/16/15"))
  
  stations$start_date[stations$name == "Middlefield Light Rail Station"] <- mdy("9/28/15")
  stations$start_date[stations$name == "Charleston Park/ North Bayshore Area"] <- mdy("9/16/15")
  stations$start_date[stations$lat == 37.7979 & stations$long == -122.405942] <- mdy("5/19/16")
  
  s73_add <- stations %>% 
    filter(station_id == 73 & is.na(start_date)) %>%
    mutate(lat = 37.797746, long = -122.407073, 
           start_date = mdy("3/14/16"), end_date = mdy("5/18/16"))
  
  s83_add <- stations %>% 
    filter(station_id == 83) %>%
    mutate(name = "Mezes", lat = 37.491405, long = -122.233051, 
           start_date = mdy("9/16/15"))
  
  stations <- rbind(stations, s72_add, s21_add, s26_add, s73_add, s83_add)
  
  return(stations)
  
}
