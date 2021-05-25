getwd()
setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Barwon South West Victoria")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)
library(simputation)


# import melbourne listings into compiled dataframe, remove unwanted columns
listings <- dir('data', full.names = T) %>% map_df(read_csv)
listings_sum <- listings[c(1, 4, 40, 49, 50, 61, 83)]
listings_sum$city <- 'Barwon South West'

# adding column of listing by months, change neighbourhood to factor
listings_sum$last_scraped <- as.Date(listings_sum$last_scraped, format = "%Y-%m-%d")
listings_sum$months <- format(as.Date(listings_sum$last_scraped),'%Y-%m')
listings_sum$months <- as.Date(as.yearmon(listings_sum$months))

# turning currency with a dollar sign and commas into numeric
listings_sum$price <- as.factor(gsub(",", "", listings_sum$price))
listings_sum$price <- as.numeric(gsub("\\$", "", listings_sum$price))

listings_sum$neighbourhood_cleansed <- as.factor(listings_sum$neighbourhood_cleansed)
listings_sum$room_type <- as.factor(listings_sum$room_type)

# removing listing with outlier prices

inter_quartile_range = summary(listings_sum$price)[["3rd Qu."]]-summary(listings_sum$price)[["1st Qu."]]
inter_quartile_range
upper_bound = summary(listings_sum$price)[["3rd Qu."]] + 1.5*(inter_quartile_range)

listings_sum_inlier <- listings_sum[listings_sum$price<upper_bound,]

########Data Cleanup Ended#################

########## Imputation ############ many missing months


listings_sum_inlier <- listings_sum_inlier %>%
  complete(months = seq.Date(min(months), max(months), by='month'), id)

screenshot = test1[c(2, 7, 8, 9, 1)]

listings_sum_inlier <- listings_sum_inlier %>%
  group_by(id) %>% 
  mutate(neighbourhood_cleansed = na_if(neighbourhood_cleansed, NA)) %>% 
  fill(neighbourhood_cleansed)

listings_sum_inlier <- listings_sum_inlier %>%
  group_by(id) %>% 
  mutate(city = na_if(city, NA)) %>% 
  fill(city)


listings_sum_inlier <- listings_sum_inlier %>%
  arrange(months) %>%
  arrange(id)


#######simputation########

### 1 is missing price, 0 if not####
missing_index <- which(is.na(listings_sum_inlier$price))
listings_sum_inlier$missing <- 0
listings_sum_inlier$missing[missing_index] <- 1

test1 <- filter(listings_sum_inlier, id == 2115)
test1 <- listings_sum_inlier

test1 <- test1 %>%
  group_by(id) %>%
  impute_lm(price~months)

test1 <- impute_lm(test1, price ~ months | id)

listings_sum_inlier <- test1


# same again but with no outliers

avg_price_month_in <- aggregate(price~months, listings_sum_inlier, mean)
avg_price_month_in$state <- 'Victoria'
write.csv(avg_price_month_in, 'average_price_monthly.csv')

avg_reviewcount_month_in <- aggregate(number_of_reviews~months, listings_sum_inlier, mean)
sum_change_review_in <- aggregate(review_count_change~months, listings_sum_inlier_change, sum)

monthly_aggregates <- merge(avg_price_month_in, avg_reviewcount_month_in, by="months", all.x = TRUE)
monthly_aggregates <- merge(monthly_aggregates, sum_change_review_in, all.x = TRUE)
monthly_aggregates$city <- 'Melbourne'


# finding average price and review count for each neighbourhood per month
avg_price_neighbourhood_month <- aggregate(price~months+neighbourhood_cleansed, listings_sum, mean)
avg_reviewcount_neighbourhood_month <- aggregate(number_of_reviews~months+neighbourhood_cleansed, listings_sum, mean)

# same again but with no outliers
avg_price_neighbourhood_month_in <- aggregate(price~months+neighbourhood_cleansed, listings_sum_inlier, mean)
avg_price_neighbourhood_month_in$state <- 'Victoria'

write.csv(avg_price_neighbourhood_month_in, 'average_neighbourhood.csv')




