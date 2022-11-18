
setwd("H:/Elvira/Gleon/ML shuqi don")

rm(list=ls())

library(rLakeAnalyzer)

#download th4 latest data from here : https://epawebapp.epa.ie/hydronet/#32070


fgh = read.csv("complete_daymean.csv", sep=";")

#remove first couple of lines
fgh<-fgh[-c(1:10),]
#remove blacnk rows

fgh<- fgh[!apply(fgh == "", 1, all), ]   

colnames(fgh) <- c('datetime',"level")
fgh$level<-as.numeric(as.character(fgh$level))

fgh$datetime <- as.POSIXct(fgh$datetime, format = '%Y-%m-%d ', tz = 'UTC') #Format into datetime
plot(fgh$datetime, fgh$level)
summary(fgh)
str(fgh)

#interploate gaps

library(zoo)
summary(fgh)

ds<-zoo(fgh[,c(2)],fgh$datetime) #The data frame must be a zoo object for this to work
ds<-na.approx(ds) #linear interpolation
summary(ds)

level.fill<-fortify.zoo(ds)
summary(level.fill)
str(level.fill)

colnames(level.fill) <- c('datetime',"level")
plot(level.fill$datetime, level.fill$level)



###open seans correcetd dataset
wl<-read.csv("https://raw.githubusercontent.com/IrishMarineInstitute/BurishooleLTER-Public/master/Random%20Burrishoole%20LTER%20useful%20information/Daily_feeagh_mean_1976-2018.csv")
wl$date <-as.POSIXct(wl$date  ,format="%Y-%m-%d", tz="UTC")


plot(wl, type="l")

summary(wl)

colnames(wl) <- c('date',"level")

#add data from 26th July 2018 onwards


jul_2018<- level.fill[format.Date(level.fill$datetime, "%Y-%m-%d")>"2018-07-26" & !is.na(level.fill$datetime),]

wtr.lineseries(jul_2018)
min(jul_2018$datetime)
max(jul_2018$datetime)

print(colSums(is.na(jul_2018)))
colnames(jul_2018) <- c('date',"level")
jul_2018[,1] <- format(jul_2018[,1], format = '%Y-%m-%d')
###subtract 10.504 from the daily means toa ccount for OD malin
jul_2018$level<-jul_2018$level-10.504

plot(jul_2018$level)

jul_2018$date <- as.POSIXct(jul_2018$date, format = '%Y-%m-%d ', tz = 'UTC') #Format into datetime

###the data from jul 2018 on to sean fixed data

com<-rbind(wl,jul_2018)
summary(com)
plot(com)

##to covert to disharge from Feeagh into Furnace, we use the Rating curve from Sean and Brians work, 
###based on flow measurements taken in Mill race and Salmon Leap and combined

####################################
####   Q (m3s???1) =15.5*(Feeagh Height (m))^1.71###
###################################


com$disharge<-15.5*(com$level)^1.71

plot(com$date, com$level,type="l", xlab="Date", ylab= expression(paste("Discharge m"^"3"*"s"^"-1")), col="blue")

write.csv(com, file = "H:/Elvira/Gleon/ML shuqi don/feeagh_out_1976_2021.csv",row.names = F, col.names = TRUE, sep = '\t', quote = F)

