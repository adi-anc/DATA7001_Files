setwd("C:\\Users\\adijo\\Desktop\\NationalCrisis\\Airbnb Datasets\\Australia")

library(tidyverse)
library(fs)
library(zoo)
library(dplyr)

dir('statedata', full.names = T)


melb <- read_csv('statedata/melbourne_monthly_agg_in.csv')
melb$state <- 'Victoria'
melb <- melb[c(2, 3, 6, 7)]
write.csv(melb, 'statedata/melbourne_monthly_agg_in.csv')


syd <- read_csv('statedata/sydney_monthly_agg_in.csv')
syd$state <- 'New South Wales'
syd <- syd[c(2, 3, 6, 7)]
write.csv(syd, 'statedata/sydney_monthly_agg_in.csv')


tasman <- read_csv('statedata/tasmania_monthly_agg_in.csv')
tasman$state <- 'Tasmania'
tasman <- tasman[c(2, 3, 6, 7)]
write.csv(tasman, 'statedata/tasmania_monthly_agg_in.csv')

prices <- dir('statedata/state', full.names = T) %>% map_df(read_csv)
prices <- prices[c(2, 3, 4)]


write.csv(prices, "stateprices.csv")
















