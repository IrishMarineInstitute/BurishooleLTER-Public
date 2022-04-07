
rm(list=ls())

library(dplyr)
library(tidyverse)

fs<-read.csv("daily_fish_migration_burrishoole_1970_2020_processed.csv")

fs$date <-as.POSIXct(fs$date ,format="%Y-%m-%d", tz="UTC")
names(fs)

###only look at station_id==Combined_MillRace_SalmonLeap_Trap  for summaries. 
#This dataset also inlude the salmon upstream counts through the Mill race and Salmon leap as seperate entries,
#but these are only used for timing.

fs <-filter(fs , station_id=="Combined_MillRace_SalmonLeap_Trap")

summary(fs)
str(fs)

summary<- fs %>%
  group_by(species, stream) %>%
  summarize(total = sum(daily_count, na.rm = TRUE))
total<-sum(fs$daily_count, na.rm=TRUE)


plot(fs$date, fs$daily_count)
#plot each of the species seperately

ds_eel <-filter(fs , stream=="downstream" & species=="Anguilla anguilla")
plot(ds_eel$date, ds_eel$daily_count, type="l")

ds_sa <-filter(fs , stream=="downstream" & species=="Salmo salar")
plot(ds_sa$date, ds_sa$daily_count, type="l")

ds_tr <-filter(fs , stream=="downstream" & species=="Salmo trutta")
plot(ds_tr$date, ds_tr$daily_count, type="l")

us_tr <-filter(fs , stream=="upstream" & species=="Salmo trutta")
plot(us_tr$date, us_tr$daily_count, type="l")

us_sa <-filter(fs , stream=="upstream" & species=="Salmo salar")
plot(us_sa$date, us_sa$daily_count, type="l")


