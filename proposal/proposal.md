Project proposal
================
Adler and Jackie

``` r
library(tidyverse)
library(broom)
library(readxl)
library(dplyr)
```

## 1. Introduction

The data is of the presence of birds around an aquaculture farm, to see
if the aquaculture farm impacted the population of birds in the area,
and if those bird populations were negatively impacted by the planes
coming from the nearby airport. It was collected by John Anderson and a
group of research assistants. They went out a few days each month, and
for an hour they collected data on the environmental factors (including
temperature, wind), the tide, and what species were observed.

We are looking to explore any patterns in the presence of bird species,
and which birds are found in specific whether conditions.

## 2. Data

``` r
seabird_count <- read_excel("../data/Copy of Trenton seabird count_nov 2014-2023.xls")
```

    ## Warning: Expecting numeric in F132 / R132C6: got a date

    ## Warning: Expecting numeric in F284 / R284C6: got a date

    ## Warning: Expecting numeric in F285 / R285C6: got a date

    ## Warning: Expecting numeric in F293 / R293C6: got a date

    ## Warning: Expecting numeric in F301 / R301C6: got a date

    ## Warning: Expecting numeric in F416 / R416C6: got a date

    ## Warning: Expecting numeric in F417 / R417C6: got a date

    ## Warning: Expecting numeric in F418 / R418C6: got a date

    ## Warning: Expecting numeric in F419 / R419C6: got a date

    ## New names:
    ## • `` -> `...17`
    ## • `` -> `...59`
    ## • `` -> `...60`

``` r
seabird_count <- seabird_count %>%
  rename(observer = "OBSERVER",
         date = "DATE",
         hours = "HOURS",
         time = "TIME",
         temp = "TEMP (F)",
         wind_speed = "WIND SPEED (mph)",
         wind_direction = "WIND DIRECTION",
         tide_obs = "TIDE OBS",
         tide_percentage = "TIDE %",
         weather_obs = "WEATHER OBS",
         weather_percentage = "WEATHER %",
         precipitation = "PRECIPITATION",
         total = "Total",
         herring_gull = "Herring Gull",
         laughing_gull = "Laughing Gull",
         great_black_backed_gull = "Great Black-backed Gull",
         red_throated_loon = "red throated loon",
         common_loon = "Common Loon",
         long_tailed_duck = "Long-tailed Duck",
         red_breasted_merganser = "Red breasted Merganser",
         common_merganser = "Common Merganser",
         black_duck = "Black Duck",
         surf_scoter = "surf scoter",
         common_eider = "Common Eider",
         bufflehead = "Bufflehead",
         goldeneye = "Goldeneye",
         lesser_scaup = "Lesser Scaup",
         black_scoter = "Black Scoter",
         white_winged_scoter = "White-Winged Scoter",
         red_necked_grebe = "Red-Necked Grebe",
         horned_grebe = "Horned Grebe",
         bonapartes_gull = "Bonaparte's Gull",
         belted_kingfisher = "Belted Kingfisher",
         common_tern = "Common Tern",
         ring_billed_gull = "Ring-Billed Gull",
         canada_goose = "Canada Goose", 
         great_blue_heron = "Great Blue Heron",
         double_crested_cormorant = "Double-Crested Cormorant",
         common_crow = "Common Crow",
         black_guillemot = "Black Guillemot",
         osprey = "Osprey",
         mallard_duck = "Mallard Duck",
         spotted_sandpiper = "Spotted Sandpiper",
         semipalmated_plover = "Semipalmated Plover",
         bald_eagle = "Bald Eagle",
         red_tailed_hawk = "Red-Tailed hawk",
         turkey_vulture = "Turkey Vulture",
         rough_legged_hawk = "rough legged hawk",
         harrier = "Harrier",
         kestrel = "Kestrel",
         turkey = "Turkey",
         unidentifiable_duck = "Unidentifiable Duck",
         unidentifiable = "Unidentifiable (too far to ID)")
```

``` r
seabird_count <- seabird_count %>%
 select(-"...17", -"COMMENTS", -"...59", -"...60") %>%
  slice(-1) %>%
mutate(across(total:unidentifiable, as.numeric)) %>%
  pivot_longer(
    cols = total:unidentifiable,
      names_to = "species",
   values_to = "count")
```

    ## Warning: There were 3 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `across(total:unidentifiable, as.numeric)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

``` r
seabird_count_tidy <- seabird_count %>%
  mutate(species = case_when(str_detect(species, pattern = "Surf Scoter") ~ "surf_scoter",
                             TRUE ~ species)) %>%
  mutate(weather_obs_original = weather_obs) %>%
  mutate(weather_obs = case_when(str_detect(weather_obs, pattern = "clear|sunny") ~ "clear",
                              str_detect(weather_obs, pattern = "driz|rain") ~ "rain",
                              str_detect(weather_obs, pattern = "fog|haz") ~ "foggy",
                              str_detect(weather_obs, pattern = "over") ~ "overcast",
                              str_detect(weather_obs, pattern = "clou|clod") ~ "cloudy",
                              TRUE ~ weather_obs)) %>%
  mutate(tide_obs_original = tide_obs) %>% 
  mutate(tide_obs = case_when(str_detect(tide_obs, pattern = "half|med") ~ "mid",
                              str_detect(tide_obs, pattern = "hig|thre") ~ "high",
                              str_detect(tide_obs, pattern = "low|one|thi|qua") ~ "low",
                              TRUE ~ tide_obs)) %>% 
  mutate(month = as.character(month)) %>% 
  mutate(month_original = month) %>% 
  mutate(month = case_when(str_detect(month, pattern = "12|1|2") ~ "winter",
                           str_detect(month, pattern = "3|4|5") ~ "spring",
                           str_detect(month, pattern = "6|7|8") ~ "summer",
                           str_detect(month, pattern = "9|10|11") ~ "fall",
                           TRUE ~ month)) %>% 
  rename(season = month) %>% 
  mutate(count_original = count) %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  
  # ^creating a copy of the variable to save it, edited the original data to have correct labels 
  mutate(wind_direction_clean = case_when(wind_direction %in% c("5", "10", "15","calm") ~ wind_speed,
                                          TRUE ~ wind_direction)) %>%
  mutate(wind_speed_clean = case_when(wind_speed %in% c("n", "s", "e", "w", "sw", "nw", "ne") ~ wind_direction,
                                          TRUE ~ wind_speed)) 

  # ^wind direction and speed were switched for some observations: created a new variable for the case when in wind_direction certain observations were found and copied them to wind_speed and then in a new line doing the inverse to copy the wind_speed to wind_direction


# seabird_count_tidy %>%
#  mutate(wind_speed_new = case_when(wind_speed))
```

``` r
seabird_count_tidy <- seabird_count_tidy %>%
  mutate(wind_direction = case_when(wind_direction == "calm" ~ "0",
                                    TRUE ~ wind_direction)) %>%
  mutate(wind_speed = case_when(wind_speed == "calm" ~ "0",
                                TRUE ~ wind_speed)) %>%
  mutate(wind_speed = case_when(wind_speed == "15-20" ~ "17.5",
                                TRUE ~ wind_speed)) %>%
  mutate(wind_direction = case_when(wind_direction == "var" ~ NA, 
                                    wind_direction == "sn" ~ NA, 
                                    TRUE ~ wind_direction))

seabird_count_tidy <- seabird_count_tidy %>%
  mutate(wind_direction_new = case_when(str_starts(wind_direction, "n") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "s") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "w") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "e") == TRUE ~ wind_direction,
                                        str_starts(wind_speed, "n") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "s") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "w") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "e") == TRUE ~ wind_speed)) %>%
  mutate(wind_speed_new = case_when(str_starts(wind_direction, "0") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "1") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "2") == TRUE ~ wind_direction,
                                        str_starts(wind_direction, "3") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "4") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "5") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "6") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "7") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "8") == TRUE ~ wind_direction,
                                    str_starts(wind_direction, "9") == TRUE ~ wind_direction,
                                    str_starts(wind_speed, "0") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "1") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "2") == TRUE ~ wind_speed,
                                        str_starts(wind_speed, "3") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "4") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "5") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "6") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "7") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "8") == TRUE ~ wind_speed,
                                    str_starts(wind_speed, "9") == TRUE ~ wind_speed)) %>%
  select(-wind_speed, -wind_direction) %>%
  mutate(wind_speed_new = as.numeric(wind_speed_new))
```

``` r
seabird_count_tidy <- seabird_count_tidy %>%
  mutate(time_of_day = case_when( time >= 530 & time <= 1159 ~ "morning",
    time >= 1200 & time <= 1659 ~ "afternoon",
    time >= 1700 & time <= 2029 ~ "evening",
    time >= 2030 ~ "night",
    time <= 530 ~ "night"))
```

``` r
seabird_count_tidy <- seabird_count_tidy %>%
  mutate(species_group = case_when(species == "herring_gull" ~ "gull",
                                   species == "laughing_gull" ~ "gull",
                                   species == "great_black_backed_gull" ~ "gull",
                                   species == "bonapartes_gull" ~ "gull",
                                   species == "ring_billed_gull" ~ "gull",
                                   species == "red_throated_loon" ~ "loon",
                                   species == "common_loon" ~ "loon",
                                   species == "long_tailed_duck" ~ "duck",
                                   species == "red_breasted_merganser" ~ "duck",
                                   species == "common_merganser" ~ "duck",
                                   species == "black_duck" ~ "duck",
                                   species == "surf_scoter" ~ "duck",
                                   species == "common_eider" ~ "duck",
                                   species == "bufflehead" ~ "duck",
                                   species == "goldeneye" ~ "duck",
                                   species == "lesser_scaup" ~ "duck",
                                   species == "black_scoter" ~ "duck",
                                   species == "mallard_duck" ~ "duck",
                                   species == "unidentifiable_duck" ~ "duck",
                                   species == "common_crow" ~ "gen_non_waterbird",
                                   species == "belted_kingfisher" ~ "gen_non_waterbird", 
                                   species == "osprey" ~ "gen_non_waterbird",
                                   species == "bald_eagle" ~ "gen_non_waterbird",
                                   species == "red_tailed_hawk" ~ "gen_non_waterbird",
                                   species == "turkey_vulture" ~ "gen_non_waterbird",
                                   species == "rough_legged_hawk" ~ "gen_non_waterbird",
                                   species == "harrier" ~ "gen_non_waterbird",
                                   species == "kestrel" ~ "gen_non_waterbird",
                                   species == "turkey" ~ "gen_non_waterbird",
                                   species == "common_tern" ~ "gen_waterbird", 
                                   species == "canada_goose" ~ "gen_waterbird", 
                                   species == "great_blue_heron" ~ "gen_waterbird",
                                   species == "double_crested_cormorant" ~ "gen_waterbird",
                                   species == "black_guillemot" ~ "gen_waterbird",
                                   species == "spotted_sandpiper" ~ "shorebird",
                                   species == "semipalmated_plover" ~ "shorebird",
                                   species == "horned_grebe" ~ "grebe",
                                   species == "red_necked_grebe" ~ "grebe",
                                   species == "white_winged_scoter" ~ "duck"
                                   ))
```

``` r
seabird_count_tidy %>%
  distinct(species_group)
```

    ## # A tibble: 8 × 1
    ##   species_group    
    ##   <chr>            
    ## 1 <NA>             
    ## 2 gull             
    ## 3 loon             
    ## 4 duck             
    ## 5 grebe            
    ## 6 gen_non_waterbird
    ## 7 gen_waterbird    
    ## 8 shorebird

``` r
 ggplot(seabird_count_tidy, aes(x = species_group)) + 
  geom_bar() +
  facet_wrap(~ year)
```

![](proposal_files/figure-gfm/test_species_group_plot-1.png)<!-- -->

## 3. Ethics review

Limitations in data sources: We don’t see there to be any bias in the
data as it was lead by a known and trusted Professor and his team. There
aren’t any large gaps in the birds data but sometimes weather and tides
were missing. The data was originally quite messy but we have been
cleaning it up and improving the quality. The only issue regarding team
composition would be that the different people recording the data would
input it in different formats making it harder for us to tidy.

Positive effects on people: We will be positively affected by this
project because this will help us further our understanding of coding
and data science. The people working at the airport will be positively
affected by this project because it will let them know whether or not
there are birds at risk, and with the hopeful result of no negative
trend in the bird populations, it will let them know that there have
been no negative impacts on the birds. The people working the
aquaculture farm will also be positively affected by this project,
because hopefully the data will let them know that their farm has not
led to an increase in bird deaths.

We can communicate the positive impact by making sure that our data
visualizations are clear, and then we can send the results of our
project to John Anderson, and he can send it to the people he is
collecting this data for. We can measure the positive impact by seeing
if more aquaculture farms are put in place.

Negative effects on people: There could be negative effects on the
people running the aquaculture farm. If we find that the aquaculture
farm led to an increase in bird populations, followed by a decrease in
the bird populations, that could be due to bird deaths due to being so
close to an airport. If this is the case, the people running the
aquaculture farm may have to shut it down to stop the bird deaths.

John Anderson, who has been collecting this data for them, has been
communicating with them on a very regular basis about the findings of
this data. They knew going into it that there was a chance that there
could be negative effects on them if it was found that their aquaculture
farm was having negative effects on the populations of birds nearby.

Minimizing harm: If we see a pattern of bird gain then loss, it would be
beneficial for us to report that to John because then he can bring that
to the people running the aquaculture farm. This would reduce harm to
the birds because they could take steps to limit the bird deaths. The
only negative effect of this analysis would be the needed closing of the
aquaculture farm if it is causing an increase of bird death and there is
no way to lessen that however that question was the reason for the data
collection.

## 4. Data analysis plan

The variables we are visualizing are the species count, season,
temperature, wind speed, and tide observation. We will be looking to see
if the variables for a certain group of birds have similarities or
differences to the variables for other groups of birds.

We won’t need any additional data, but we will need to do a little
research on bird groupings (for example, ducks vs gulls).

``` r
ggplot(seabird_count, aes(x = month, fill = count)) +
  geom_bar() +
  labs(x = "Month", y = "Number of Birds", title = "Number of Birds Recorded Each Month", subtitle = "2014 - 2024")
```

    ## Warning: Removed 42 rows containing non-finite values (`stat_count()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: fill
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](proposal_files/figure-gfm/preliminary_analysis_month_count-1.png)<!-- -->

We think that bar graphs and scatter plots will be most useful in our
data visualization.
