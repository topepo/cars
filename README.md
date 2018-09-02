Tired of `mtcars` having only 32 rows? 

This repo contains code to create up-to-date fuel economy data sets on demand. A few time-stamped versions are also contained here. More information about the raw data can be found on the [government website](https://www.fueleconomy.gov/feg/ws/index.shtml). 

Date-stamped data files are given along with the code that creates them (`import.R`). However, as with many things, the situation is complicated. There are some details below about our decisions here. You can override them in `import.R` though.

First, we save data from 2015 on.

Second, there can be multiple rows for the same car that are replicates. If the car did not change, it has the exact same mpg over different years. For example, the Chrysler 300 has rows:

```r
> car_data %>% 
+     dplyr::filter(
+         make == "Chrysler" & 
+         model == "300" & 
+         trany == "Automatic_8spd" &
+         fuelType == "Gasoline_or_E85"
+     ) %>%
+     arrange(year) %>%
+     select(year, displ, eng_dscr, trany, fuelType, city08U, comb08U)
# A tibble: 5 x 7
   year displ eng_dscr trany          fuelType        city08U comb08U
  <int> <dbl> <chr>    <chr>          <chr>             <dbl>   <dbl>
1  2015   3.6 FFV      Automatic_8spd Gasoline_or_E85    19.0    22.8
2  2016   3.6 FFV      Automatic_8spd Gasoline_or_E85    19.0    22.8
3  2017   3.6 FFV      Automatic_8spd Gasoline_or_E85    19.0    22.8
4  2018   3.6 FFV      Automatic_8spd Gasoline_or_E85    19.0    22.8
5  2019   3.6 FFV      Automatic_8spd Gasoline_or_E85    19.0    22.8
```

(all examples shown here are as of 2018/09/02)

In our code, we keep the row corresponding to the earliest year (2015 in this case). To determine when there are replicates, we group the data on a few variables: make, model, engine displacement, the engine description, transmission type, fuel type,  and the unrounded city and combined mpg (but we'd like to have a better algorithm to figure this out). A little less than 20% of car configurations have this issue. 

Third, some cars have a single fuel type (e.g. "Regular", "Midgrade", "Premium", etc.) while others have multiple ("Regular Gas or Electricity", "Regular Gas _and_ Electricity", "Electricity", and so on). These data have the mpg for the _first_ fuel type listed. This is presumably only relevant to the cars with "or" in their fuel type field (> 300 cars). We decided to use the _best_ mpg across the types tested. We are estimating the best possible mileage that the car can produce. For example, a car that can use "Regular Gas or Electricity" would have its electricity MPG used as the outcome. 

Fourth, for cars without combustion engines, the engine displacement and cylinders were assigned values of zero. 

Please contribute, especially if you:

* Find errors
* Have other car-related descriptors. We'd like to be able to automatically download them (as with the fuel economy values). No "golden spreadsheets" apart from original sources that are consistently updated.  

