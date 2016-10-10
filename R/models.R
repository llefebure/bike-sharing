setwd("~/Documents/Projects/BikeShare/Rmd")

library(dplyr)
library(readr)
library(stringr)
library(knitr)
library(randomForest)
library(ggplot2)
source("../R/helpers.R")

# build dataset
full <- getProcessedTripData()
train <- full %>% filter(year == "2014" | (year == "2015" & month < "03"))
test <- full %>% filter(year == "2015" & month >= "03")

### model building

ARR_FORM <- arrivals ~ weekday + hour + weekday:hour + `Mean TemperatureF` + bikes_available
DEP_FORM <- departures ~ weekday + hour + weekday:hour + `Mean TemperatureF` + bikes_available
NET_FORM <- net ~ weekday + hour + weekday:hour + `Mean TemperatureF` + bikes_available

# evaluation metrics to compare methods
evaluate <- function(obs, preds) {
  mse <- mean(abs(obs - preds)**2)
  mae <- mean(abs(obs - preds))
  list(mse = mse,
       mae = mae)
}

# ols for net change directly
fitOLS <- function(train, test) {
  fit <- glm(NET_FORM, data = train)
  preds <- predict(fit, newdata = test)
  train_eval <- evaluate(train$net, predict(fit))
  test_eval <- evaluate(test$net, preds)
  return(list(train = train_eval, test = test_eval))
}

# random forest for net change directly
fitRF <- function(train, test) {
  fit <- randomForest(NET_FORM, data = train.st)
  preds <- predict(fit, newdata = test)
  train_eval <- evaluate(train$net, predict(fit))
  test_eval <- evaluate(test$net, preds)
  return(list(train = train_eval, test = test_eval))
}

# poisson regression models for arrivals and departures
fitPoisson <- function(train, test) {
  arr_fit <- glm(ARR_FORM, data = train, family = "poisson")
  dep_fit <- glm(DEP_FORM, data = train, family = "poisson")
  arr_preds <- predict(arr_fit, newdata = test, type = "response")
  dep_preds <- predict(dep_fit, newdata = test, type = "response")
  net_preds <- arr_preds - dep_preds
  train_eval <- evaluate(train$net, predict(arr_fit, type = "response") - predict(dep_fit, type = "response"))
  test_eval <- evaluate(test$net, net_preds)
  return(list(train = train_eval, test = test_eval))
}

train.st <- filter(train, station_id == 58)
test.st <- filter(test, station_id == 58)
fitOLS(train.st, test.st)
fitRF(train.st, test.st)
fitPoisson(train.st, test.st)
