library(tidyverse) # contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats, lubridate

# Read the data 
house_values = read.csv('./data/home_values.csv', header=TRUE, sep = ',')
house_forecasts = read.csv('./data/home_forecasts.csv', header=TRUE, sep = ',')
rentals = read.csv('./data/rentals.csv', header=TRUE, sep = ',')

# Check the data
head(house_values)
head(house_forecasts)
head(rentals)

# Clean the data
## Rename columns
house_values <- house_values %>% 
  rename(rank = SizeRank,
    zipcode = RegionName,
    county = CountyName, 
    city = City,
    state = State
  )
house_forecasts <- house_forecasts %>% 
  rename(zipcode = RegionName,
         state = StateName, 
         county = CountyName,
         city = CityName, 
         date = ForecastedDate, 
         appreciation = ForecastYoYPctChange
  )
rentals <- rentals %>% 
  rename(rank = SizeRank,
         zipcode = RegionName,
         city_state = MsaName, 
  )
## All zip codes are missing leading 0s (where applicable)
house_values$zipcode <- sapply(house_values$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
house_forecasts$zipcode <- sapply(house_forecasts$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
rentals$zipcode <- sapply(rentals$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})

## Reshape the data so date columns are one per row
house_values <- pivot_longer(house_values, cols=10:315, names_to = "date", values_to = "price")
rentals <- pivot_longer(rentals, cols=5:94, names_to = "date", values_to = "price")

## Make dates readable as dates
house_values$date <- sub(".", "", house_values$date) # remove the leading "X"
house_values$date <- as_date(house_values$date)
house_forecasts$date <- as_date(house_forecasts$date) 
rentals$date <- sub(".", "", rentals$date) # remove the leading "X"
rentals$date <- parse_date_time(rentals$date, "ym") # convert to date_time so day value is added
rentals$date <- as_date(rentals$date) # covert to date to drop time value
rentals$date <- rentals$date-1 # subtract 1 day so values align with other datasets 

## Remove all house values before 2013-12-31 because we do not have a rent value to associate with
house_values_trim <- house_values %>% filter(date > "2013-12-01")

## Combine rentals and home values into one dataframe
house_all <- house_values_trim %>% full_join(rentals, by = c('zipcode', 'date'), copy = FALSE, suffix = c(".x", ".y"))
head(house_all)

## Rename the columns after spot checking data
house_all <- house_all %>% 
  rename(ID = RegionID.x,
         rank = rank.x,
         price = price.x, 
         rent = price.y)

## Drop columns not of interest
house_all <- select(house_all, -c(RegionType, StateName, RegionID.y, rank.y, city_state))


