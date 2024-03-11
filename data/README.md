# data

Place data file(s) in this folder.

Then, include codebooks (variables, and their descriptions) for your data file(s)
using the following format.

## name of data file

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