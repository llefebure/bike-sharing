#' Retrieve paths for the relevant data files
#' 
#' @return a list containing full file paths under category names
getFilePaths <- function(){
  dir <- "~/Documents/Projects/BikeShare/data/"
  prefixes <- c("201402", "201408", "201508")
  list(trip = paste0(dir, prefixes, "_trip_data.csv"),
       status = paste0(dir, prefixes, "_status_data.csv"),
       station = paste0(dir, prefixes, "_station_data.csv"),
       weather = paste0(dir, prefixes, "_weather_data.csv"))
}

#' Build the processed data set
#' 
#' @description Derive hourly arrivals and departures from the trip data
#' @return tbl_df with the processed data set
getProcessedTripData <- function(fn = "../data/processed_trips.Rds"){
  if (!is.null(fn)) {
    return(readRDS(fn))
  }
  
  # read in and do some preprocessing on raw data files
  file_paths <- getFilePaths()
  trip_data <- rbind(read_csv(file_paths$trip[2]),
                     read_csv(file_paths$trip[3]))
  weather_data <- rbind(read_csv(file_paths$weather[2]),
                        read_csv(file_paths$weather[3]))
  status_data <- rbind(read_csv(file_paths$status[2]),
                       read_csv(file_paths$status[3])) %>%
    filter(format(time, "%M") == "00") %>%
    mutate(year = format(time, "%Y"),
           month = format(time, "%m"),
           day = format(time, "%d"),
           hour = format(time, "%H")) %>%
    select(station_id, bikes_available, docks_available, year, month, day, hour)
  station_data <- rbind(read_csv(file_paths$station[2]),
                        read_csv(file_paths$station[3])) %>%
    select(station_id, landmark) %>%
    group_by(station_id, landmark) %>%
    filter(row_number() == 1) %>%
    ungroup()
  station_data$Zip <- sapply(station_data$landmark, function(l) {
    if (l == "San Francisco") 94107
    else if (l == "Redwood City") 94063
    else if (l == "Palo Alto") 94301
    else if (l == "Mountain View") 94041
    else if (l == "San Jose") 95113
  })
  
  # add derived date fields to the trip data
  trip_data <- trip_data %>% 
    mutate(s.time = as.POSIXct(`Start Date`, format = "%m/%d/%Y %H:%M"),
           s.year = format(s.time, "%Y"),
           s.month = format(s.time, "%m"),
           s.day = format(s.time, "%d"),
           s.dow = format(s.time, "%a"),
           s.hour = format(s.time, "%H"),
           s.weekday = ifelse(format(s.time, "%u") < 6, "Weekday", "Weekend"),
           e.time = as.POSIXct(`End Date`, format = "%m/%d/%Y %H:%M"),
           e.year = format(e.time, "%Y"),
           e.month = format(e.time, "%m"),
           e.day = format(e.time, "%d"),
           e.dow = format(e.time, "%a"),
           e.hour = format(e.time, "%H"),
           e.weekday = ifelse(format(e.time, "%u") < 6, "Weekday", "Weekend"))
  
  # pull out departures and arrivals
  departures <- trip_data %>%
    group_by(`Start Terminal`, s.year, s.month, s.day, s.dow, s.hour, s.weekday) %>%
    summarize(departures = n()) %>%
    ungroup()
  arrivals <- trip_data %>%
    group_by(`End Terminal`, e.year, e.month, e.day, e.dow, e.hour, e.weekday) %>%
    summarize(arrivals = n()) %>%
    ungroup()
  
  # rename columns to match for joining
  colnames(departures) <- c("station_id", "year", "month", "day", 
                            "dow", "hour", "weekday", "departures")
  colnames(arrivals) <- c("station_id", "year", "month", "day", 
                          "dow", "hour", "weekday", "arrivals")
  
  # join to combine arrivals and departures columns into one df
  processed <- full_join(arrivals, departures)
  
  # need to pad with rows for which there were zero arrivals and departures, so
  # we first need to get a grid with all combinations of station_id, year, month, day, etc.
  # I use the join_key = "" to simulate Cartesian product
  date_range <- tbl_df(data.frame(date = seq.Date(from = as.Date("2014-03-01"), 
                                                  to = as.Date("2015-08-31"), 
                                                  by = 1))) %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           day = format(date, "%d"),
           dow = format(date, "%a"),
           weekday = ifelse(format(date, "%u") < 6, "Weekday", "Weekend"),
           join_key = "") %>%
    full_join(data.frame(station_id = unique(processed$station_id), join_key = "")) %>%
    full_join(data.frame(hour = str_pad(as.character(0:23), width = 2, side = "left", pad = "0"), 
                         join_key = "")) %>%
    select(station_id, year, month, day, dow, hour, weekday)
  
  # join date grid with processed df
  processed <- left_join(date_range, processed)
  
  # outer joins fill with NA's, so we need to convert these to 0's
  processed[is.na(processed)] = 0
  
  # add column for net change
  processed <- mutate(processed, net = arrivals - departures)
  
  # add zips to join with weather data
  processed <- inner_join(processed, station_data)
  
  # add derived date fields to weather data
  weather_data <- weather_data %>% 
    mutate(time = as.POSIXct(PDT, format = "%m/%d/%Y"),
           year = format(time, "%Y"),
           month = format(time, "%m"),
           day = format(time, "%d")) %>%
    select(year, month, day, Zip, `Mean TemperatureF`)
  
  # append weather info
  processed <- inner_join(processed, weather_data)
  
  # add status data
  processed <- full_join(processed, status_data)
  
  # change to factors
  processed$weekday <- factor(processed$weekday)
  processed$hour <- factor(processed$hour)
  
  return(processed)
}
