library(tidyverse) # contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats
library(lubridate) # for manipulating dates

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
  rename(rank = SizeRank,
         zipcode = RegionName,
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
head(house_values$zipcode, 1000)


  