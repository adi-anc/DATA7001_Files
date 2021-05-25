getwd()
setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Australia")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)
library(lubridate)

# import sydney listings into compiled dataframe, remove unwanted columns
listings <- read_csv('data/listings.csv')
reviews <- read_csv('data/reviews.csv')
neighbourhoods <- read_csv('data/neighbourhoods.csv')

listings_sum <- listings[c(1, 74, 75, 77)]
reviews_sum <- reviews[c(1, 3, 4)]


listings_neighbourhood <- listings_sum %>%
  group_by(id, region_parent_name)

reviews_sum$neighbourhood <- listings_neighbourhood$
  region_name[match(reviews_sum$listing_id,listings_neighbourhood$id)]
reviews_sum$neighbourhood_id <- listings_neighbourhood$
  region_id[match(reviews_sum$listing_id, listings_neighbourhood$id)]
reviews_sum$state <- listings_neighbourhood$
  region_parent_name[match(reviews_sum$listing_id, listings_neighbourhood$id)]


write.csv(reviews_sum, 'reviews_australia.csv')


#### change in traffic before covid to during covid


## subset 

dec <- reviews_sum %>% 
  filter(date >= '2019-12-01' & date <= '2019-12-31')

april <- reviews_sum %>% 
  filter(date >= '2020-04-01' & date <= '2020-05-15')
 

dec_state <- dec %>% 
  group_by(state) %>% 
  tally()

dec_state$dec_count <- dec_state$n
dec_state <- dec_state[c(1,3)]


april_state <- april %>% 
  group_by(state) %>% 
  tally()

april_state$april_count <- april_state$n
april_state <- april_state[c(1,3)]

review_change <- merge(dec_state, april_state, all.x = TRUE)
review_change$difference <- review_change$april_count - review_change$dec_count 
review_change$relative <- review_change$difference/review_change$dec_count
review_change$percentage <- review_change$relative*100

write.csv(review_change, 'state_dec_apr_change.csv')

##### Neighbourhood Change

dec <- reviews_sum %>% 
  filter(date >= '2019-12-01' & date <= '2019-12-31')

april <- reviews_sum %>% 
  filter(date >= '2020-04-01' & date <= '2020-05-15')


dec_neighbourhood <- dec %>% 
  group_by(state,neighbourhood) %>% 
  tally()

dec_neighbourhood$dec_count <- dec_neighbourhood$n
dec_neighbourhood<- dec_neighbourhood[c(1,2,4)]


april_neighbourhood <- april %>% 
  group_by(state, neighbourhood) %>% 
  tally()

april_neighbourhood$april_count <- april_neighbourhood$n
april_neighbourhood <- april_neighbourhood[c(1,2,4)]

review_change_neighbourhood <- merge(dec_neighbourhood, april_neighbourhood, all.x = TRUE)
review_change_neighbourhood$difference <- review_change_neighbourhood$april_count - review_change_neighbourhood$dec_count 
review_change_neighbourhood$relative <- review_change_neighbourhood$difference/review_change_neighbourhood$dec_count
review_change_neighbourhood$percentage <- review_change_neighbourhood$relative*100

#removing NAs

review_change_neighbourhood <- na.omit(review_change_neighbourhood)


write.csv(review_change_neighbourhood, 'neighbourhood_dec_apr_change.csv')

# Time to recovery

### state












