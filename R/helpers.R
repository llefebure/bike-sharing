#' Retrieve paths for the relevant data files
#' 
#' @return a list containing full file paths under category names
getFilePaths = function(){
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
getProcessedTripData <- function(){
  
  file_paths <- getFilePaths()
  trip_data <- rbind(read_csv(file_paths$trip[2]),
                     read_csv(file_paths$trip[3]))
  
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
  # I use the join_key = "" to simulate Cartestian product
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
  
  # possibly add hourly weather data from forecast.io and station capacity and availability at the time
  
  # join date grid with processed df
  processed_final <- left_join(date_range, processed)
  
  # outer joins fill with NA's, so we need to convert these to 0's
  processed_final[is.na(processed_final)] = 0
  
  # add column for net change
  processed_final <- mutate(processed_final, net = arrivals - departures)
  
  # change to factors
  processed_final$weekday <- factor(processed_final$weekday)
  processed_final$hour <- factor(processed_final$hour)
  
  return(processed_final)
}
