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

#' Build the training set
#' 
#' @description Derive hourly arrivals and departures from the trip data
#' @return tbl_df with the training set
getTrainingSet <- function(){
  # currently hard coded for the data file from 9/1/14 to 8/31/15
  file_paths <- getFilePaths()
  trip_data <- read_csv(file_paths$trip[3])
  
  # add derived date fields to the trip data
  trip_data <- trip_data %>% 
    mutate(time = as.POSIXct(`Start Date`, format = "%m/%d/%Y %H:%M"),
           year = format(time, "%Y"),
           month = format(time, "%m"),
           day = format(time, "%d"),
           dow = format(time, "%a"),
           hour = format(time, "%H"),
           weekday = ifelse(format(time, "%u") < 6, "Weekday", "Weekend"))
  
  # pull out departures
  departures <- trip_data %>%
    group_by(`Start Terminal`, year, month, day, dow, hour, weekday) %>%
    summarize(departures = n()) %>%
    ungroup()
  arrivals <- trip_data %>%
    group_by(`End Terminal`, year, month, day, dow, hour, weekday) %>%
    summarize(arrivals = n()) %>%
    ungroup()
  
  # rename terminal columns to match for joining
  colnames(departures)[1] <- "station_id"
  colnames(arrivals)[1] <- "station_id"
  
  # join to combine arrivals and departures columns into one df
  training_set <- full_join(arrivals, departures)
  
  # need to pad with rows for which there were zero arrivals and departures, so
  # we first need to get a grid with all combinations of station_id, year, month, day, etc.
  date_range <- tbl_df(data.frame(date = seq.Date(from = as.Date("2014-09-01"), 
                                                  to = as.Date("2015-08-31"), 
                                                  by = 1))) %>%
    mutate(year = format(date, "%Y"),
           month = format(date, "%m"),
           day = format(date, "%d"),
           dow = format(date, "%a"),
           weekday = ifelse(format(date, "%u") < 6, "Weekday", "Weekend"),
           join_key = "") %>%
    full_join(data.frame(station_id = unique(training_set$station_id), join_key = "")) %>%
    full_join(data.frame(hour = str_pad(as.character(0:23), width = 2, side = "left", pad = "0"), 
                         join_key = "")) %>%
    select(station_id, year, month, day, dow, hour, weekday)
  
  # join to fill in gaps
  training_set_final <- left_join(date_range, training_set)
  
  # possibly add hourly weather data from forecast.io and station capacity and availability at the time
  
  # outer joins fill with NA's, so we need to convert these to 0's
  training_set_final[is.na(training_set_final)] = 0
  
  return(training_set_final)
}
