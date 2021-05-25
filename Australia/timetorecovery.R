setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Australia")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)
library(lubridate)
library(plyr)

# import sydney listings into compiled dataframe, remove unwanted columns
listings <- read_csv('data/listings.csv')
reviews <- read_csv('data/reviews.csv')


listings_sum <- listings[c(1, 74, 75, 77)]
reviews_sum <- reviews[c(1, 3, 4)]

listings_neighbourhood <- listings_sum %>%
  group_by(id, region_parent_name)

reviews_sum$neighbourhood <- listings_neighbourhood$region_name[match(reviews_sum$listing_id, listings_neighbourhood$id)]
reviews_sum$neighbourhood_id <- listings_neighbourhood$region_id[match(reviews_sum$listing_id, listings_neighbourhood$id)]
reviews_sum$state <- listings_neighbourhood$region_parent_name[match(reviews_sum$listing_id, listings_neighbourhood$id)]



df1 <- filter(reviews_sum, date >= '2020-01-25' & date <= '2021-01-24')

df2 <- transform(df1, date= format(date, format = "%y%U"))

df3 <- df2 %>% 
  group_by(neighbourhood, date) %>% 
  tally()

adelaide <- filter(df3, neighbourhood == 'Adelaide')


plot(adelaide$date, adelaide$n)




hood_stats <- df3 %>% 
  group_by(neighbourhood) %>% 
  tally()
 


before <- df3 %>% 
  filter(date == '2004')
before$before <- before$n
before <- before[c(1,4)]



after <- df3 %>% 
  filter(date == '2100')
after$after <- after$n
after <- after[c(1,4)]


before_after <- merge(before, after, by.x = 'neighbourhood')


before_after$change <- before_after$after/before_after$before

write.csv(before_after, 'before_after.csv')

############ sum month


jan_2020 <- filter(reviews_sum, date >= '2020-01-01' & date <= '2020-01-30')
jan_2021 <- filter(reviews_sum, date >= '2021-01-01' & date <= '2021-01-30')


j2020 <- jan_2020 %>% 
  group_by(neighbourhood) %>% 
  tally()
j2020$before <- j2020$n
j2020 <- j2020[c(1,3)]

j2021 <- jan_2021 %>% 
  group_by(neighbourhood) %>% 
  tally()
j2021$after <- j2021$n
j2021 <- j2021[c(1,3)]


month_before_after <- merge(j2020, j2021)
month_before_after$change <- month_before_after$after/month_before_after$before


write.csv(month_before_after, 'month_before_after.csv')







