---
title: "Commuters in the Bay Area Bike Share System"
output: html_document
---

I recently finished reading a book called [How Cycling Can Save the World](https://www.amazon.com/How-Cycling-Can-Save-World/dp/0143111779) by Peter Walker. As a part-time bike commuter, a lot of the material in the book resonated with me such as the health and societal benefits of a population that cycles regularly and the need for better bike infrastructure to make that happen. One component of a more bike friendly city is a bike share program. I was curious about how much of the Bay Area's own system's usage is by commuters, so in this post, I look into its data and focus in on two observations that suggest that this number is significant.



### The Bay Area Bike Share

Bike share systems have been rolled out in many cities across the United States and the world to mixed success, and they play a role in normalizing cycling as a viable public transit and commute option. Here in the Bay Area, we have the Bay Area Bike Share (BABS) which was rolled out in 2013. Riders can purchase a single day or three day pass (called Customers) or an annual pass (called Subscribers) for use of the bikes. They pay an additional overage charge for trips taking longer than thirty minutes, so continuous point to point journeys are incentivized as opposed to, for example, long discontinuous sightseeing trips.

The BABS lags behind other major cities in terms of both usage and capacity. It has had a presence in five cities along the peninsula -- San Francisco, San Jose, Redwood City, Palo Alto, and Mountain View -- but the latter three have all been discontinued due to low usage. However, there is [hope](http://www.sfchronicle.com/business/networth/article/SF-bike-share-program-gathering-speed-with-burst-10976162.php) with new investment coming and the Bay Area's amenable climate and active community.

The BABS makes data publically available [here](http://www.bayareabikeshare.com/open-data). This includes individual records for nearly a million trips made between 8/31/2013 and 8/31/2016, complete with information like origin, destination, duration, start time, and more. For this analysis, I look only at trips originating in SF.



### Observation 1: Popular Stations are near Transit Hubs

I would expect that Subscribers are more likely to be commuters. The first observation is that, among subscribers, the most popular stations are near major transit hubs. The top ten origin and destination stations are shown below, and hubs such as Caltrain, the Ferry Building, and the Transbay Terminal are well-represented. The top four stations account for about a quarter of all trips.





##### Subscriber Top Stations

|Station                                       | Trip Starts|
|:---------------------------------------------|-----------:|
|San Francisco Caltrain (Townsend at 4th)      |       68384|
|San Francisco Caltrain 2 (330 Townsend)       |       53694|
|Temporary Transbay Terminal (Howard at Beale) |       37888|
|Harry Bridges Plaza (Ferry Building)          |       36621|
|2nd at Townsend                               |       35500|
|Steuart at Market                             |       34062|
|Townsend at 7th                               |       32788|
|Market at Sansome                             |       31268|
|Embarcadero at Sansome                        |       27203|
|Market at 10th                                |       26560|

|Station                                       | Trip Ends|
|:---------------------------------------------|---------:|
|San Francisco Caltrain (Townsend at 4th)      |     87129|
|San Francisco Caltrain 2 (330 Townsend)       |     56267|
|2nd at Townsend                               |     39557|
|Harry Bridges Plaza (Ferry Building)          |     38985|
|Market at Sansome                             |     37703|
|Townsend at 7th                               |     36303|
|Temporary Transbay Terminal (Howard at Beale) |     34439|
|Steuart at Market                             |     34406|
|Embarcadero at Sansome                        |     28276|
|Market at 10th                                |     22195|

Taking a look at the same data for Customers, we see a very different pattern. First, the volume is much lower, and second, trips to and from the largest Subscriber hubs, Caltrain and the Transbay Terminal, are relatively less common. This indicates that a large number of those Subscribers are using the bikes as a leg of their commute.



##### Customer Top Stations

|Station                                  | Trip Starts|
|:----------------------------------------|-----------:|
|Embarcadero at Sansome                   |       13934|
|Harry Bridges Plaza (Ferry Building)     |       12441|
|Market at 4th                            |        5952|
|Powell Street BART                       |        5214|
|Embarcadero at Vallejo                   |        4945|
|Powell at Post (Union Square)            |        4932|
|Steuart at Market                        |        4469|
|2nd at Townsend                          |        4436|
|San Francisco Caltrain (Townsend at 4th) |        4299|
|Market at Sansome                        |        3874|

|Station                                  | Trip Ends|
|:----------------------------------------|---------:|
|Embarcadero at Sansome                   |     17920|
|Harry Bridges Plaza (Ferry Building)     |     11200|
|Market at 4th                            |      5755|
|Powell Street BART                       |      5225|
|Embarcadero at Vallejo                   |      5212|
|Steuart at Market                        |      5192|
|Powell at Post (Union Square)            |      5036|
|San Francisco Caltrain (Townsend at 4th) |      4869|
|2nd at Townsend                          |      4588|
|Grant Avenue at Columbus Avenue          |      4438|

### Observation 2: Most Trips are During Commute Hours

The second observation is that most trips occur during commute hours. The heatmaps below show the relative number of trips per month made by Customers and Subscribers split by weekday.

These plots reveal clear time-of-day usage patterns. Subscribers on weekdays make considerably more trips during the morning and afternoon commute hours suggesting that a majority of these people use the bikes to travel between their home, place of work, and/or a transit hub.











![plot of chunk commute-time-heatmap](figure/commute-time-heatmap-1.png)

Moreover, this segment is by far the most active. Subscribers on weekdays contribute about 80% of the total number of trips, and this segment sees five to ten times more trips per day than any other. Zooming in further on this segment, we can see a clearer distribution of their trip times. 


|Subscription |Day of Week | Number of Days| Count of Trips| Avg. Trips Per Day|
|:------------|:-----------|--------------:|--------------:|------------------:|
|Customer     |Weekday     |            785|          71992|           91.70955|
|Customer     |Weekend     |            314|          47631|          151.69108|
|Subscriber   |Weekday     |            785|         721147|          918.65860|
|Subscriber   |Weekend     |            314|          50453|          160.67834|



![plot of chunk commute-hist-by-hour](figure/commute-hist-by-hour-1.png)

### Fixing the Last Mile Problem

In public transit systems, the [last mile problem](https://en.wikipedia.org/wiki/Last_mile_(transportation)) refers to how people get from a transit hub to their final destination. If a bike sharing system can successfully solve this problem for people, it becomes an integral part of the public transit system itself. While the analysis above is far from rigorous, I think it does show that the BABS has filled this gap for a lot of people. Most trips occur by Subscribers during commute hours on weekdays, and a significant number of these trips connect with major transit hubs.
