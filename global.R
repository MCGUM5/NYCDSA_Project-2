
library(dplyr)
library(lubridate)

allzips <- readRDS("data/superzip.rds")
Homes_all <- read_csv("./data/Homes_all.csv")
Homes_2022 <- read_csv("./data/Homes_2022.csv")

allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

allzips <- inner_join(allzips, Homes_2022, by = c('zipcode'), copy = FALSE)
allzips <- allzips %>% mutate(none = 1+ price*0)

cleantable <- allzips %>%
  select(
    City = city,
    State = state,
    Zipcode = zipcode,
    Year = year,
    Rent = rent,
    Price = price,
    Appreciation = appreciation,
    Profit_Ratio = revenue,
    None = none,
    Lat = latitude,
    Long = longitude
  ) 