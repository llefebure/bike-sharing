# Analyzing the Bay Area Bike Share System

In this project, I experiment with data provided by the [Bay Area Bike Share](http://www.bayareabikeshare.com/open-data) (BABS) system. As a part time bike commuter and a believer in the benefits of an active commute, I think bike sharing networks are an interesting concept. There are a lot of questions that I'm interested in trying to solve with this data, so this project serves as an exploratory and educational platform for me.

The analysis in this repository is organized as follows. General data cleaning and preparation tasks are in the R directory, and more in depth analysis and visualization is contained in Rmd files in the Rmd directory.

## Data Preparation

The BABS provides data on trips and stations which I make use of in this project. The trip data has records of trips (origin station, destination station, start time, end time, etc.) and information about stations (lat/long coordinates, name, capacity, etc.). It is separated out into several files (different CSVs for different time windows) and accompanied by READMEs that have important amendments hidden in text, so the first step is to unify this data. Some challenges that I encountered during this task are:

* Different date formats across files
    * Using lubridate makes it relatively easy to resolve this issue. 
* Stations moved locations and changed names at various times
    * This wasn't reflected in the data -- only in the README -- so I had to augment the station data with the different station variants. At a high level, my solution requires that when joining a trip to its origin or destination station, both the station id and timestamp are necessary for retrieving the proper station variant.

The end result is two clean dataframes, one for stations and one for trips. My further analysis builds off of these dataframes.

## Analysis

The prepared data contains a lot of interesting information about how and when people use the BABS, so I explore various questions related to it. See below for some high level descriptions of these questions or take a look at the md files in the Rmd directory to see the actual analysis.

### Comparing Trip Times in the BABS to Google Maps Cycling Estimates
By hitting the Google Maps Directions API, I can get cycling duration and distance estimates for all station pairs by passing the lat/long coordinates of the origin and destination. Once I have this information, I compare these estimates with the true trip duration. This produces some interesting insight into the accuracy of the Google Maps estimates.
