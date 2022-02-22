##Feb 2022

#2 There are weather stations in Furncae, the automatic ststaion and the manual station.

#The manual station started recording in 1960, and records daily. A QA/QCed subset of this can be downloaded straight from the met eireann server, 
#using the script below (part 1). It can alos be downloaded manually from the met eireann website
#https://www.met.ie/climate/available-data/historical-data (select daily -> mayo -> Newport -furnace ) 
#Some of the variables that are measured are not downlaodable using this script(e.g. sunlight hours)
#get in touch with Elvira if you are missing something. There is a lag to allow Met Eireann to do QA/QC

#The automatic station reocrds at minute frequency, and data can be downloaded in 2 ways.
##Part 2 script downloads QA/QCed data from the met eireann server, but there is a month or twos  lag 
##Part 3 script downlaods the same data but from the Marine Institute's ERDDAP data server. There is no lag 
#(e.g. you can doload yesterdays data), but the data have not been QA/QCEd.

###met eireann data is available for doanload under licecne ccby https://creativecommons.org/licenses/by/4.0/
##shoudl always be prpoerly attributed


rm(list=ls())

#set your own WD
setwd("H:/Elvira/InventWater/Ricardo")


library(lubridate) #for timeseries
library(scales) #labels and axis
library(ggplot2)
library(tidyverse)
library(dplyr)

######################################################################################

#################Part 1  downlaod daily data from manual station via met eireann######
#################dly833 is daily data from staion 833 (ie. furnace manual)###################

######################################################################################

download.file(url = 'https://cli.fusio.net/cli/climate_data/webdata/dly833.zip', destfile = "furnace.zip")

unzip("furnace.zip")

##take a looka t the first 20 lines
readLines("dly833.csv", 
          n=20)
#need to skip the first 12 rows that have metadata in them

fur<-read.csv("dly833.csv", skip = 12)
fur<-fur[c(1,3,5,7)]  ##extract some columns

fur$date <- as.POSIXct(fur[,1], format = '%d-%b-%Y', tz = 'UTC')
str(fur)

plot(fur$maxt~fur$date)

#remove outliers in max t
idx <- which(fur$maxt <= -10) # Create an index
fur[idx, 3] <- NA
plot(fur$maxt~fur$date)

##calcualte meant 
fur$meant<-(fur$maxt+fur$mint)/2
plot(fur$mint~fur$date)
plot(fur$meant~fur$date)
plot(fur$rain~fur$date)

#create "Year" & "Day"columns from Date
fur[, "year"] <- as.numeric(format(fur[,"date"], "%Y"))
fur[, "month"] <- as.numeric(format(fur[,"date"], "%m"))
fur[, "DOY"] <- as.numeric(yday(fur$date))

#tkae out 1959
fur<- fur %>%
  filter(year>=1960)
##(1959 is incomplete)

##check for odd data
summary(fur)

#extract 2021
fur<- fur %>%
  filter(year>=2021)

write.csv(fur,"fur_2021_met_manual.csv",  row.names=FALSE)

##########################################################################################

#################Part 2  downlaod hourly data from automatic station via met eireann######
################ hly1175 is hourly data from staion 1175 (ie. furnace automatic)###################

###########################################################################################


download.file(url = 'https://cli.fusio.net/cli/climate_data/webdata/hly1175.zip', destfile = "furnace.zip")

unzip("furnace.zip")

##take a looka t the first 20 lines
readLines("hly1175.csv", 
          n=20)
#need to skip the first 17 rows that have metadata in them

fur<-read.csv("hly1175.csv", skip = 17)
fur<-fur[c(1,3,5,7,8,9,10,11,13,15)]  ##extract some columns

fur$date <- as.POSIXct(fur[,1], format = '%d-%b-%Y', tz = 'UTC')
str(fur)


#create "Year" & "Day"columns from Date
fur[, "year"] <- as.numeric(format(fur[,"date"], "%Y"))
fur[, "month"] <- as.numeric(format(fur[,"date"], "%m"))
fur[, "DOY"] <- as.numeric(yday(fur$date))


##check for odd data
summary(fur)

#extract 2021
fur<- fur %>%
  filter(year>=2021)

write.csv(fur,"fur_2021_met_auto.csv",  row.names=FALSE)


##########################################################################################

#################Part 3  downlaod hourly data from automatic statiosn via ERDDAP##########

##########################################################################################

#this downlaods hourly data for various variables
#if you want other variables, you should go on the erdaap site https://erddap.marine.ie/erddap/tabledap/imiFurnaceWSHourly.html
#and select the oens you want, then go down and copy the url link, and paste it in to line below
##see here for more info https://erddap.marine.ie/erddap/tabledap/documentation.html


download.file('https://erddap.marine.ie/erddap/tabledap/imiFurnaceWSHourly.csv?time%2CAtmosphericPressure%2CTotalRain%2CWindSpeed%2CsolarRad%2CMeanAirTemperature%2CHumidity',"met_hrly.csv")

unzip("met_hrly.csv")

readLines("met_hrly.csv", 
          n=20)

fur<-read.csv("met_hrly.csv")

units<-fur[1,]##extartc the "unit" row
fur<-fur[-1,]###remove this row from the dataset

##convert factors to number
fact.col <- names(Filter(is.factor,fur))[-1]
#Convert factors to columns
for(i in fact.col){
  fur[,i] <- as.numeric(as.character(fur[,i]))
  print(paste0(i,' converted from factor to numeric'))
}
print(summary(fur))


##fix up the time column = - this is particular to ERDDAP
fur$time <- as.character(fur$time) #Convert from factor to character
fur$time <- gsub('T',' ',fur$time) #Remove the T 
fur$time <- gsub('Z','',fur$time) #Remove the Z


fur$time <- as.POSIXct(fur$time, format="%Y-%m-%d %H:%M:%S" , tz = 'UTC') #Format into datetime

min(fur$time)
max(fur$time)


##extract some dates
fur<- fur %>%
  filter(time<= "2021-12-31 23:59:00")

fur<- fur %>%
  filter(time>= "2021-01-01 00:00:00")


##check for odd data
summary(fur)
#convert windspeed from knots to m/sec
fur$WindSpeed_msec<-fur$WindSpeed** 0.514444444

#convert sol rad from j.cm.2 to wm2


fur$solarRad_wm2 <- (fur$solarRad  * 10000) / (60 * 60) # j/cm2 -> W/m2

#get in order
fur<-fur[,c(1,6,7,8,9,3)]

idx <- which(fur$solarRad_wm2 < "0") # Create an index
fur[idx, 5] <- NA

summary(fur)

plot(fur$MeanAirTemperature)
plot(fur$Humidity)
plot(fur$WindSpeed_msec)
plot(fur$solarRad_wm2)
plot(fur$TotalRain)

write.csv(fur,"fur_2021_met_auto_erddap.csv",  row.names=FALSE)
