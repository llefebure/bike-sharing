setwd("~/Documents/Projects/BikeShare/Rmd")

library(dplyr)
library(readr)
library(ggplot2)
library(scales)
source("../R/helpers.R")

file_paths <- getFilePaths()

# Read in trip data:
trip_data <- lapply(file_paths$trip, read_csv)
colnames(trip_data[[1]])[10] <- "Subscriber Type" # make colnames consistent
trip_data <- do.call("rbind", trip_data)

# Add date features
trip_data <- trip_data %>% 
  mutate(time = as.POSIXct(`Start Date`, format = "%m/%d/%Y %H:%M"),
         month = format(time, "%m"),
         day = format(time, "%a"),
         hour = format(time, "%H"))

# Most common trips and their average duration
common_routes <- trip_data %>%
  group_by(`Start Station`, `End Station`, hour) %>%
  summarize(trip_count = n(), trip_duration = mean(Duration), 
            sd_trip_duration = sd(Duration)) %>%
  ungroup() %>%
  arrange(desc(trip_count))

## Station Availability

file_paths <- getFilePaths()
status_data <- read_csv(file_paths$status[3])

status_data <- status_data %>%
  mutate(month = format(time, "%m"), 
         day = format(time, "%a"),
         hour = format(time, "%H"),
         minute = format(time, "%M"),
         weekday = ifelse(format(time, "%u") < 6, "1", "0"),
         hm = as.POSIXct(format(time, "%H:%M"), tz = "", format = "%H:%M"))

# When are stations empty?

empty <- filter(status_data, bikes_available == 0)
ggplot(empty, aes(x=as.numeric(hour))) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Hour", y = "# of Minutes Empty", title = "Empty Docks by Hour")
ggplot(empty, aes(x=day)) +
  geom_bar() + 
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x = "Day of the Week", y = "# of Minutes Empty", title = "Empty Docks by Day of the Week")

# When are stations full?

full <- filter(status_data, docks_available == 0)
ggplot(full, aes(x=as.numeric(hour))) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Hour", y = "# of Minutes Full", title = "Full Docks by Hour")
ggplot(full, aes(x=day)) +
  geom_bar() + 
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  labs(x = "Day of the Week", y = "# of Minutes Empty", title = "Full Docks by Day of the Week")

ggplot(filter(agg, station_id == 77), aes(x = hm, y = avg_bikes_available,
                                          group = weekday, color = weekday)) +
  geom_line() +
  scale_x_datetime(labels = date_format("%H:%M", tz = "America/Los_Angeles"))

