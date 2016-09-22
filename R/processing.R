getTrainingSet <- function(){
  file_paths <- getFilePaths()
  # data from 9/1/14 to 8/31/15
  trip_data <- read_csv(file_paths$trip[3])
  trip_data <- trip_data %>% 
    mutate(time = as.POSIXct(`Start Date`, format = "%m/%d/%Y %H:%M"),
           year = format(time, "%Y"),
           month = format(time, "%m"),
           day = format(time, "%a"),
           hour = format(time, "%H"),
           weekday = ifelse(format(time, "%u") < 6, "Weekday", "Weekend"))
  departures <- trip_data %>%
    group_by(`Start Terminal`, year, month, day, hour, weekday) %>%
    summarize(departures = n())
  arrivals <- trip_data %>%
    group_by(`End Terminal`, year, month, day, hour, weekday) %>%
    summarize(arrivals = n())
  colnames(departures)[1] <- "station_id"
  colnames(arrivals)[1] <- "station_id"
  training_set <- full_join(arrivals, departures)
  # need to pad with rows for which zero arrivals and departures
  # need to add minutely weather data and station capacity and availability at the time
  training_set[is.na(training_set)] = 0
}