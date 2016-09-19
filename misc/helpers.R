getFilePaths = function(){
  dir <- "~/Documents/Projects/BikeShare/data/"
  prefixes <- c("201402", "201408", "201508")
  list(trip = paste0(dir, prefixes, "_trip_data.csv"),
       status = paste0(dir, prefixes, "_status_data.csv"),
       station = paste0(dir, prefixes, "_station_data.csv"),
       weather = paste0(dir, prefixes, "_weather_data.csv"))
}
