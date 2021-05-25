getwd()
setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Tasmania Tasmania")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)

# import sydney listings into compiled dataframe, remove unwanted columns
listings <- dir('data', full.names = T) %>% map_df(read_csv)
listings_sum <- listings[c(1, 4, 40, 42, 44, 46, 49, 50, 52, 53, 61, 83)]
listings_sum$city <- 'Tasmania'

# adding column of listing by months, change neighbourhood to factor
listings_sum$last_scraped <- as.Date(listings_sum$last_scraped, format = "%Y-%m-%d")
listings_sum$months <- format(as.Date(listings_sum$last_scraped),'%Y-%m')
listings_sum$months <- as.Date(as.yearmon(listings_sum$months))

# turning currency with a dollar sign and commas into numeric
listings_sum$price <- as.factor(gsub(",", "", listings_sum$price))
listings_sum$price <- as.numeric(gsub("\\$", "", listings_sum$price))

listings_sum$neighbourhood_cleansed <- as.factor(listings_sum$neighbourhood_cleansed)
#listings_sum$room_type <- as.factor(listings_sum$room_type)

# removing listing with outlier prices

inter_quartile_range = summary(listings_sum$price)[["3rd Qu."]]-summary(listings_sum$price)[["1st Qu."]]
inter_quartile_range
upper_bound = summary(listings_sum$price)[["3rd Qu."]] + 1.5*(inter_quartile_range)

listings_sum_inlier <- listings_sum[listings_sum$price<upper_bound,]

########Data Cleanup Ended#################

########## Imputation ############ no need here


listings_sum_inlier <- listings_sum_inlier %>%
  complete(months = seq.Date(min(months), max(months), by='month'), id)

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

# missing month review count

listings_sum_inlier$number_of_reviews <- ifelse(is.na(listings_sum_inlier$number_of_reviews), (lag(listings_sum_inlier$number_of_reviews) +
                                                                                                 lead(listings_sum_inlier$number_of_reviews))/2, listings_sum_inlier$number_of_reviews)

# missing month price

listings_sum_inlier$price <- ifelse(is.na(listings_sum_inlier$price), (lag(listings_sum_inlier$price) +
                                                                         lead(listings_sum_inlier$price))/2, listings_sum_inlier$price)




# finding change in review counts
change_in_reviews = listings_sum_inlier %>%
  group_by(id) %>%
  arrange(months) %>%
  summarise(review_change = number_of_reviews - lag(number_of_reviews), .groups = 'drop')

# covnerts from matrix to dataframe
#change_in_reviews <- do.call(data.frame, change_in_reviews)

listings_sum_inlier_change <- listings_sum_inlier

listings_sum_inlier_change <- listings_sum_inlier_change %>%
  arrange(months) %>%
  arrange(id) #%>%
#mutate(review_count_change = change_in_reviews[[2]])

listings_sum_inlier_change$review_count_change <- change_in_reviews[[2]]

# removing negative changes

listings_sum_inlier_change$review_count_change[listings_sum_inlier_change$review_count_change < 0] <- 0


# finding average price and review count per month
avg_price_month <- aggregate(price~months, listings_sum, mean)
avg_reviewcount_month <- aggregate(number_of_reviews~months, listings_sum, mean)


# same again but with no outliers

avg_price_month_in <- aggregate(price~months, listings_sum_inlier, mean)
avg_reviewcount_month_in <- aggregate(number_of_reviews~months, listings_sum_inlier, mean)
sum_change_review_in <- aggregate(review_count_change~months, listings_sum_inlier_change, sum)

monthly_aggregates <- merge(avg_price_month_in, avg_reviewcount_month_in, by="months", all.x = TRUE)
monthly_aggregates <- merge(monthly_aggregates, sum_change_review_in, all.x = TRUE)
monthly_aggregates$city <- 'Tasmania'


# finding average price and review count for each neighbourhood per month
avg_price_neighbourhood_month <- aggregate(price~months+neighbourhood_cleansed, listings_sum, mean)
avg_reviewcount_neighbourhood_month <- aggregate(number_of_reviews~months+neighbourhood_cleansed, listings_sum, mean)

# same again but with no outliers
avg_price_neighbourhood_month_in <- aggregate(price~months+neighbourhood_cleansed, listings_sum_inlier, mean)
avg_reviewcount_neighbourhood_month_in <- aggregate(number_of_reviews~months+neighbourhood_cleansed, listings_sum_inlier, mean)
sum_change_review_neighbourhood_in <- aggregate(review_count_change~months+neighbourhood_cleansed, listings_sum_inlier_change, sum)

monthly_neighbourhood_aggregates <- merge(avg_price_neighbourhood_month_in, 
                                          avg_reviewcount_neighbourhood_month_in, 
                                          by=c('months', 'neighbourhood_cleansed'), all.x = TRUE)


monthly_neighbourhood_aggregates <- merge(monthly_neighbourhood_aggregates, sum_change_review_neighbourhood_in,
                                          by=c('months', 'neighbourhood_cleansed'),
                                          all.x = TRUE)

monthly_neighbourhood_aggregates$city <- 'Tasmania'

plot(sum_change_review_neighbourhood_in$months, sum_change_review_neighbourhood_in$review_count_change)





### Combines Aggregate Dataframe #### #ignore this already did it with merge, this
# includes outliers as well

avg_price_counts_month <- avg_price_month
avg_price_counts_month$average_reviewcount <- avg_reviewcount_month$number_of_reviews
avg_price_counts_month$city <- 'Melbourne'

avg_neighbourhood_price_counts_month <- avg_price_neighbourhood_month
avg_neighbourhood_price_counts_month$average_reviewcount <- avg_reviewcount_neighbourhood_month$number_of_reviews
avg_neighbourhood_price_counts_month$city <- 'Melbourne'

# same again but with no outliers
avg_price_counts_month_in <- avg_price_month
avg_price_counts_month_in$average_reviewcount <- avg_reviewcount_month$number_of_reviews
avg_price_counts_month_in$city <- 'Melbourne'

avg_neighbourhood_price_counts_month_in <- avg_price_neighbourhood_month
avg_neighbourhood_price_counts_month_in$average_reviewcount <- avg_reviewcount_neighbourhood_month$number_of_reviews
avg_neighbourhood_price_counts_month_in$city <- 'Melbourne'

####################################

# histograms

hist(listings_sum[listings_sum$months=='2020-06-01',]$price)
hist(listings_sum[listings_sum$months=='2021-03-01',]$price)


# histograms without high priced listings

hist(listings_sum[listings_sum$months=='2020-05-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-06-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-07-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-08-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-09-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-10-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-11-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2020-12-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2021-01-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2021-02-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2021-03-01' & listings_sum$price <=1100,]$price)
hist(listings_sum[listings_sum$months=='2021-04-01' & listings_sum$price <=1100,]$price)


#histograms without outliers

hist(listings_sum_inlier[listings_sum_inlier$months=='2020-05-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-06-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-07-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-08-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-09-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-10-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-11-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2020-12-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2021-01-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2021-02-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2021-03-01' & listings_sum_inlier$price <=1100,]$price)
hist(listings_sum_inlier[listings_sum_inlier$months=='2021-04-01' & listings_sum_inlier$price <=1100,]$price)


# Plots
plot(avg_price_month$months, avg_price_month$price)
plot(avg_reviewcount_month$months, avg_reviewcount_month$number_of_reviews)

plot(avg_price_neighbourhood_month$months, avg_price_neighbourhood_month$price)
plot(avg_reviewcount_neighbourhood_month$months, avg_reviewcount_neighbourhood_month$number_of_reviews)

# plots without outliers

plot(avg_price_month_in$months, avg_price_month_in$price)
plot(avg_reviewcount_month_in$months, avg_reviewcount_month_in$number_of_reviews)

plot(avg_price_neighbourhood_month_in$months, avg_price_neighbourhood_month_in$price)
plot(avg_reviewcount_neighbourhood_month_in$months, avg_reviewcount_neighbourhood_month_in$number_of_reviews)

######################### Imputation #########################

monthly_aggregates <- monthly_aggregates %>%
  complete(months = seq.Date(min(months), max(months), by='month'))


monthly_neighbourhood_aggregates <- monthly_neighbourhood_aggregates %>%
  complete(months = seq.Date(min(months), max(months), by='month'), neighbourhood_cleansed)



# Writing Csv
write.csv(listings_sum, 'melbourne_listins.csv') 
write.csv(avg_price_counts_month, 'melbourne_avg_price_count_month.csv') 
write.csv(avg_neighbourhood_price_counts_month, "melbourne_avg_neighbourhood_price_counts_month.csv")

# writing CSV without outliers
write.csv(listings_sum_inlier, 'tasmania_listings-inlier.csv') 
write.csv(monthly_aggregates, 'tasmania_monthly_agg_in.csv') 
write.csv(monthly_neighbourhood_aggregates, "tasmania_monthly_neighbourhood_agg_in.csv")




plot(monthly_aggregates$months, monthly_aggregates$review_count_change)
plot(monthly_neighbourhood_aggregates$months, monthly_neighbourhood_aggregates$review_count_change)


