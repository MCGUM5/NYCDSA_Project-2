Load Libraries
```{r}
library(tidyverse) # contains ggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats
library(MASS)
library(lubridate)
```
Read the data 
```{r}
house_values = read.csv('./data/home_values.csv', header=TRUE, sep = ',')
house_forecasts = read.csv('./data/home_forecasts.csv', header=TRUE, sep = ',')
rentals = read.csv('./data/rentals.csv', header=TRUE, sep = ',')
```
Check the data
```{r}
head(house_values)
head(house_forecasts)
head(rentals)
```
Clean the data
  Rename columns
```{r}
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
```
  All zip codes are missing leading 0s (where applicable)
```{r}
house_values$zipcode <- sapply(house_values$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
house_forecasts$zipcode <- sapply(house_forecasts$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
rentals$zipcode <- sapply(rentals$zipcode, function(x){if(nchar(x)<5){paste0(0,x)}else{x}})
```
  Reshape the data so date columns are one per row
```{r}
house_values <- pivot_longer(house_values, cols=10:315, names_to = "date", values_to = "price")
rentals <- pivot_longer(rentals, cols=5:94, names_to = "date", values_to = "price")
```
  Make dates readable as dates
```{r}
house_values$date <- sub(".", "", house_values$date) # remove the leading "X"
house_values$date <- as_date(house_values$date)
house_forecasts$date <- as_date(house_forecasts$date) 
rentals$date <- sub(".", "", rentals$date) # remove the leading "X"
rentals$date <- parse_date_time(rentals$date, "ym") # convert to date_time so day value is added
rentals$date <- as_date(rentals$date) # covert to date to drop time value
rentals$date <- rentals$date-1 # subtract 1 day so values align with other datasets 
```
  Remove all house values before 2013-12-31 because we do not have a rent value to associate with
```{r}
house_values_trim <- house_values %>% filter(date > "2013-12-01")
```
  Combine rentals and home values into one dataframe
```{r}
house_all <- house_values_trim %>% full_join(rentals, by = c('zipcode', 'date'), copy = FALSE, suffix = c(".x", ".y"))
head(house_all)
```
  Rename the columns after spot checking data
```{r}
house_all <- house_all %>% 
  rename(ID = RegionID.x,
         rank = rank.x,
         price = price.x, 
         rent = price.y)
```
  Drop columns not of interest
```{r}
house_all <- house_all %>% select_('ID', 'rank', 'zipcode', 'state', 'city', 'Metro', 'county', 'date', 'price', 'rent')
```
  Drop NA values on key columns
```{r}
house_all <- house_all %>% drop_na(price, rent, zipcode, date)
```
EDA - exploratory data analysis
  Univariate analysis
```{r}
str(house_all)
summary(house_all)
```
    View mean and standard deviations of price & rent at each zipcode
```{r}
house_all %>% group_by(zipcode) %>% 
  summarize(mean_price=mean(price), 
            sd_price=sd(price), 
            mean_rent=mean(rent), 
            sd_rent=sd(rent)
            )
```
    View a boxplot of house price by states
```{r}
boxplot(house_all$rent)
boxplot(house_all$price)
boxplot(price ~ state, data = house_all)
boxplot(price ~ state, data = (house_all %>% filter(state=='NY' | state=='NJ' | state=='CT' | state=='PA')))
```
    Initial Pearson correlation 
```{r}
cor(house_all$price, house_all$rent)
#### Initial Pearson correlation is 0.8436554 (hints at a significant correlation)
```
    View frequency plots
```{r}
hist(house_all$price, xlab = 'Price (USD)', main = 'Property Price Distribution')
hist(house_all$rent, xlab = 'Rent (USD)', main = 'Rent Cost Distribution')
```
  Bivariate analysis
```{r}
ggplot(data = house_all, mapping = aes(x = price, y = rent)) + theme_bw() + 
  geom_point() + ggtitle("Rent vs. Price") + xlab('Price (USD)') + ylab('Rent (USD)') + stat_smooth(method = lm, se=FALSE)
```
    General linear trend but variance appears to increases with price due to outliers. 
    Maybe HOA fees lowering purchase price but keeping rent high.

Linear regression
  Null Hypothesis: Price has no correlation with Rent.

  Run linear regression and summarize output
```{r}
model = lm(rent ~ price, data=house_all)
summary(model)
```
    Rent = (1.514e-03)*Price + 1.023e+03
    R-squared = 0.7118
    All P-values are significantly low so we can reject the null hypothesis
    RSE = $350
    
  Look at assumptions of linearity
```{r}
plot(model)
```
Some issues with our assumptions to linear regression
  1. Data is linear
  2. Residual errors are normally distributed
  3. Residuals have constant variance (homoscedasticity)
  4. Independence 

Applying the Box-Cox Transformation
```{r}
bc <- boxcox(rent ~ price, data=house_all)
lambda <- bc$x[which.max(bc$y)]
lambda
# lambda = 0.4646465
```
  Fit new linear regression model using the Box-Cox transformation
```{r}
model_bc <- lm(((rent^(lambda)-1)/lambda) ~ price, data = house_all)
summary(model_bc)
```

Plot the transformed linear regression
```{r}
plot(model_bc)
```

Compare the two linear regressions against our assumptions
  Check if data is linear using Q-Q plots
```{r}
op <- par(pty = "s", mfrow = c(1,2))
qqnorm(model$residuals)
qqline(model$residuals)
qqnorm(model_bc$residuals)
qqline(model_bc$residuals)
par(op)
```
The Box-Cox transformation appears to have had a minimal effect on all assumptions for linear regression. 
  1. Data is linear (look at Q-Q plot)
  2. Residual errors are normally distributed (look at residuals vs fitted plot)
  3. Residuals have constant variance (homoscedasticity)
  4. Independence 


Maybe there is another variable influence we are not addressing?

First what is going on when rent is over 7,000 USD?
```{r}
house_all %>% filter(rent > 7000)
# All rent over $7000 is from one zipcode!
house_all %>% filter(price > 3500000)
# All price over 3500000 is from two zipcodes!
```

In the context of creating a general equation, remove the two zip codes that are significant outliers 
```{r}
house_trim <- house_all %>% filter(zipcode != 90265 & zipcode != 94301)
```

Check histograms without outliers for normal distribution
```{r}
hist(house_trim$price, xlab = 'Price (USD)', main = 'Property Price Distribution')
hist(house_trim$rent, xlab = 'Rent (USD)', main = 'Rent Cost Distribution')
```
Still skewed right so...
Re-run Box-Cox transformation on trimmed dataset
```{r}
bc <- boxcox(rent ~ price, data=house_trim)
lambda_trim <- bc$x[which.max(bc$y)]
lambda_trim
```

Re-run linear regression on trimmed dataset.
```{r}
model_trim <- lm(((rent^(lambda_trim)-1)/lambda_trim) ~ price, data = house_trim)
summary(model_bc)
```

Plot the linear regression to check assumptions
```{r}
plot(model_trim)
```

Write the dataframe to a csv file for the Shiny app. 
```{r}
write.csv(house_trim, "./data/house_trim.csv", row.names = TRUE)
```


