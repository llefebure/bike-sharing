# This script hits the Google Maps Directions API to compile a
# dataset of estimated biking times between the stations in the network.

import requests
import pandas as pd

f1 = pd.read_csv("../data/201408_station_data.csv")
f2 = pd.read_csv("../data/201508_station_data.csv")

# drop unnecessary columns
full = pd.concat([f1, f2])
full = full[["station_id", "lat", "long", "landmark"]]

