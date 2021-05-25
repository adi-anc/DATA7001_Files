setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Australia\\statedata")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)

dir('Neighbourhood', full.names = T)
melb <- read_csv('Neighbourhood/melbourne_monthly_neighbourhood_agg_in.csv')
melb$state <- 'Victoria'
melb <- melb[c(2, 3, 4, 7, 8)]

write.csv(melb, 'Neighbourhood/melbourne_monthly_neighbourhood_agg_in.csv')


syd <- read_csv("Neighbourhood/sydney_monthly_neighbourhood_agg_in.csv")
syd$state <- 'New South Wales'
syd <- syd[c(2, 3, 4, 7, 8)]

write.csv(syd, 'Neighbourhood/sydney_monthly_neighbourhood_agg_in.csv')


tas <- read_csv("Neighbourhood/tasmania_monthly_neighbourhood_agg_in.csv")
tas$state <- 'Tasmania'
tas <- tas[c(2, 3, 4, 7, 8)]

write.csv(tas, 'Neighbourhood/tasmania_monthly_neighbourhood_agg_in.csv')


wa <- read_csv("Neighbourhood/WA_average_neighbourhood.csv")
wa$state <- 'Western Australia'

write.csv(wa, "Neighbourhood/WA_average_neighbourhood.csv")



prices <- dir('Neighbourhood', full.names = T) %>% map_df(read_csv)
prices <- prices[c(2, 3, 4, 5)]


write.csv(prices, "neighbourhoodprices.csv")


