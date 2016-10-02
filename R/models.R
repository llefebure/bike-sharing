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

ARR_FORM <- arrivals ~ weekday + hour
DEP_FORM <- departures ~ weekday + hour
NET_FORM <- net ~ weekday + hour

# evaluation metrics to compare methods
evaluate <- function(obs, preds) {
  mse <- mean(abs(obs - preds)**2)
  mse
}

# ols for net change directly
fitOLS <- function(train, test) {
  fit <- glm(NET_FORM, data = train)
  preds <- predict(fit, newdata = test)
  evaluate(test$net, preds)
}

# random forest for net change directly
fitRF <- function(train, test) {
  fit <- randomForest(NET_FORM, data = train.st)
  preds <- predict(fit, newdata = test)
  evaluate(test$net, preds)
}

# poisson regression models for arrivals and departures
fitPoisson <- function(train, test) {
  arr_fit <- glm(ARR_FORM, data = train, family = "poisson")
  dep_fit <- glm(DEP_FORM, data = train, family = "poisson")
  arr_preds <- predict(arr_fit, newdata = test, type = "response")
  dep_preds <- predict(dep_fit, newdata = test, type = "response")
  net_preds <- arr_preds - dep_preds
  evaluate(test$net, net_preds)
}

train.st <- filter(train, station_id == 58)
test.st <- filter(test, station_id == 58)
fitOLS(train.st, test.st)
fitRF(train.st, test.st)
fitPoisson(train.st, test.st)
