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
seabird_count <- seabird_count  %>%
  mutate(count = str_replace_na(count, "0")) %>%
  mutate(count = as.double(count)) %>%
  mutate(species = str_replace(species, "Surf Scoter", "surf_scoter")) %>%
  mutate(tide_obs = str_replace(tide_obs, "half", "mid")) %>%
  mutate(tide_obs = str_replace(tide_obs, "higgh", "high"))
```

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
    ## $ species            <chr> "total", "herring_gull", "laughing_gull", "great_bl…
    ## $ count              <dbl> 133, 60, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 53, 0, 0, 0,…

``` r
unique(seabird_count$species)
```

    ##  [1] "total"                    "herring_gull"            
    ##  [3] "laughing_gull"            "great_black_backed_gull" 
    ##  [5] "red_throated_loon"        "common_loon"             
    ##  [7] "long_tailed_duck"         "red_breasted_merganser"  
    ##  [9] "common_merganser"         "black_duck"              
    ## [11] "surf_scoter"              "common_eider"            
    ## [13] "bufflehead"               "goldeneye"               
    ## [15] "lesser_scaup"             "black_scoter"            
    ## [17] "white_winged_scoter"      "red_necked_grebe"        
    ## [19] "horned_grebe"             "bonapartes_gull"         
    ## [21] "belted_kingfisher"        "common_tern"             
    ## [23] "ring_billed_gull"         "canada_goose"            
    ## [25] "great_blue_heron"         "double_crested_cormorant"
    ## [27] "common_crow"              "black_guillemot"         
    ## [29] "osprey"                   "mallard_duck"            
    ## [31] "spotted_sandpiper"        "semipalmated_plover"     
    ## [33] "bald_eagle"               "red_tailed_hawk"         
    ## [35] "turkey_vulture"           "rough_legged_hawk"       
    ## [37] "harrier"                  "kestrel"                 
    ## [39] "turkey"                   "unidentifiable_duck"     
    ## [41] "unidentifiable"

``` r
unique(seabird_count$count)
```

    ##   [1] 133  60   0   1  53  19 115   8   2  18  85 181 123  11  42 125  73  24
    ##  [19]   4  33   9   6  90  79  49   3  15  99  76  10  45  28   5  54  40  51
    ##  [37]  23  22  46  41  47  39  12  26 163 156  78  35  21   7 129  93  16 103
    ##  [55]  68  14  34  61  29  96  63  20  64  58  56  13  27  17  32  25  30  89
    ##  [73]  65 153  55  81  44 128 101  86 164 193  84  37  31 148  70 183  91  38
    ##  [91] 135  36 142 114 143 157  92  75 100 160  74 107  66  77  48 151 140 112
    ## [109]  95  72 174 141  87  43 225 127 122 108  94  69  59  67 218 126  82  57
    ## [127] 104  52  50 118  71 177 117 113  62 106 206  80 138 159 102 187 145  98
    ## [145] 231 147 158 262 172 162 188 173  83 134 110 120 310 304  97 154 197 239
    ## [163] 202 116 121 124 249 234 176 161 192 132 137 139 109 196 155 185 149 258
    ## [181] 111 152 167 263 261 373 341 130  88 136 246 169 279 213 228 267 240 214
    ## [199] 395 293 221 285 245 217 191 178 296 294 215 195 168 184 227 223 272 250
    ## [217] 186 226 222 252 248 180 170 105 150 229 201 182 144 392 216 244 290 146
    ## [235] 254 194 256 328

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
