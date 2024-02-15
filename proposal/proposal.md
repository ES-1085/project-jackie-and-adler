Project proposal
================
Adler and Jackie

``` r
library(tidyverse)
library(broom)
library(readxl)
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
         lauging_gull = "Laughing Gull",
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
glimpse(seabird_count)
```

    ## Rows: 34,104
    ## Columns: 16
    ## $ year               <dbl> 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,…
    ## $ month              <dbl> 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,…
    ## $ observer           <chr> "BDRW", "BDRW", "BDRW", "BDRW", "BDRW", "BDRW", "BD…
    ## $ date               <dttm> 2014-11-03, 2014-11-03, 2014-11-03, 2014-11-03, 20…
    ## $ hours              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ time               <dbl> 1015, 1015, 1015, 1015, 1015, 1015, 1015, 1015, 101…
    ## $ temp               <dbl> 35.5, 35.5, 35.5, 35.5, 35.5, 35.5, 35.5, 35.5, 35.…
    ## $ wind_speed         <chr> "7", "7", "7", "7", "7", "7", "7", "7", "7", "7", "…
    ## $ wind_direction     <chr> "w", "w", "w", "w", "w", "w", "w", "w", "w", "w", "…
    ## $ tide_obs           <chr> "low", "low", "low", "low", "low", "low", "low", "l…
    ## $ tide_percentage    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ weather_obs        <chr> "clear", "clear", "clear", "clear", "clear", "clear…
    ## $ weather_percentage <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ precipitation      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ species            <chr> "total", "herring_gull", "lauging_gull", "great_bla…
    ## $ count              <dbl> 133, 60, NA, 0, NA, 1, 0, 0, 0, 0, NA, 0, 53, 0, NA…

## 3. Ethics review

This research does not include people, so we are not concerned with an
ethics review.

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
