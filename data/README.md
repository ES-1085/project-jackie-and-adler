# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## seabird_count_tidy

- `year` : Year, in the 2000s.
- `season` : Season the data was collected during. (Winter = December, January, and February. Spring = March, April, and May. Summer = June, July, and August. Fall = September, October, and November.)
- `observer` : The person who collected the data.
- `date` : Date the data was collected. 
- `hours` : Number of hours the observer was out collecting data.
- `time` : The time at which data collection started.
- `temp` : Temperature in Fahrenheit.
- `tide_obs` : Observers observations of the tide at the time of data collection, tidied.
- `tide_percentage` : The estimated percentage of tide height.
- `weather_obs` : Observers observations of the weather at the time of data collection, tidied.
- `weather_percentage` : The estimated percentage of cloud cover.
- `precipitation` : Type of precipitation, if any. 
- `species` : Species name.
- `count` : The number of birds observed (per species).
- `weather_obs_original` : Observers observations of the weather at the time of data collection, untidied.
- `tide_obs_original` : Observers observations of the tide at the time of data collection, untidied.
- `month_original` : The month the data was collected.
- `count_original` : The number of birds observed (per species), with NAs.
- `wind_direction_old` : Cardinal directions of the wind (north, south, east, and west), but with some improperly placed data points from the wind speed column.
- `wind_speed_old` : Speed of the wind, in MPH, but with some improperly placed data points from the wind direction column. 
- `wind_direction_new` : Cardinal directions of the wind (north, south, east, and west).
- `wind_speed_new` : Speed of the wind, in MPH.
- `time_of_day` : Time period in the day in which the data was collected. (Morning = 05:30 - 11:59. Afternoon = 12:00 - 16:59. Evening = 17:00 - 20:29. Night = 20:30 - 05:30.)
- `species_group` : General categories of waterbirds and some non-waterbirds. (Categories are gulls, loons, ducks, grebes, shorebirds, general waterbirds, and general non-waterbirds.)
- `wind_speed_group` : Wind speed grouped into ranges. (0 - 4.9; 5 - 9.9; 10 - 14.9; 15 - 19.9; 20 - 24.9, 25 - 29.9, 30)

## Write Up

We got our dataset from John Anderson, who has been collecting data since 2014 for Acadia Aquafarms. Some people wanted to start an aquaculture farm in Goose Cove, Trenton, but were recieving pushback from people who were concerned about bird safety, given the proximity to Bar Harbor Airport. People were concerned that putting an aquaculture farm in Goose Cove would attract more birds, which would put both the birds and people in danger. John’s help was requested to start documenting the number of birds, and see if there were any concerning changes to bird population numbers. 

We had a few questions that we wanted to explore using this data. These questions are:
- Do wind speed and direction influence bird presence in the cove?
- Do the tides affect the number of birds present?
- Is there any notable change in species diversity throughout the years?
- In what weather conditions will you see certain birds?

We chose to only use the data from 2015 to 2023 because those are the only years for which we had a full year’s worth of data. The variables that we used to explore our questions include month, season, species, species count, temperature, tide observations, time, wind direction, wind speed, and year. When looking at species data, we decided to group them into species groups (duck, loon, grebe, gull, shorebird, general waterbird, and general nonwaterbird) because working with seven species groups was much easier to both work with and visualize than 41 individual bird species. 

To answer our question about whether or not wind speed and direction influence bird presence in the cove, we made three plots. 

We made a jitterplot of the number of birds per day by wind speed. We added a trendline so that not only could you see the frequency of points but we could actually see if there was any significant trend in the data. We coloured the points by wind speed because it was a little easier to see the groups of points at different speeds if they were in different colours. We can see both from the clusters of points and the trendline that there are more birds present when the wind speed is lower. 

We made a bar plot of the number of birds per day by wind direction. We decided to sort the directions along the x-axis from north clockwise around the compass (n, ne, e, se, s, sw, w, nw). We noticed that there are significantly more birds present when there are west blowing winds, closely followed by south, southwest, and northwest. 

We took the same bar plot for wind direction and added a fill of wind speed. In order to do this, we grouped wind speeds into 5mph increments (0-4.9mph, 5-9.9mph, etc). We can see that the majority of birds are present per wind direction when wind speeds are lower. 

For the rest of our questions, we made one plot each. 

For the species density question, we separated the individual species into species groups then, we made the jitterplot based on the year and total count to see how the species numbers changed over time and if there was any indication of planes causing excess deaths. We noticed Gulls increased from 90 to 150 sometimes reaching over 300 birds at one time, Ducks increased from 20 to 50 and General Waterbird increased from 50 to 100. We can hypothesize that the aquaculture farm attracted more birds but there is no evidence to find that the airport is causing excess deaths. 

For the tide influence question, we organized the tides observation data then, we made a bar graph of the total count at each tide level separated by year to see how the tide level affected the number of birds present. We noticed many more birds were present at high and low tide than mid tide. We can hypothesize this is because birds are more likely to be present when they are feeding and they have higher feeding opportunities at high and low tide. 

For the spotting conditions question, we organized the months into seasons then, we made a density ridge plot of the weather observation and the temperature it occurs at, separated it by species group and colored it by season to see if there were any trend patterns in the data. We noticed that grebes are not present here in the summer, and we don't see many birds past 75 degrees or below 20 degrees other than the major outliers in general waterbirds. General non-waterbird has a bimodal graph, peaking twice at around 30 degrees in winter and again at around 70 in summer. The shorebird graph shows no data because there were not enough observations that fit the description to form a chart.


## Link to presentation

https://docs.google.com/presentation/d/1PwrnGJsQKrh9r-7Yg1mAF1qMSNxPgaN-zGtjoScp12o/edit?usp=sharing 