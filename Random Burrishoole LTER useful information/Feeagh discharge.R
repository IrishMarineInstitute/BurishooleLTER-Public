##Rating curve from Sean and Brians work, based on flow measurements taken in Mill race and Salmon Leap and combined
rm(list=ls())
####################################
####15.5*(Feeagh Height (m))^1.71###
###################################

#download the lake  height from seans corrected version.
##link here https://github.com/IrishMarineInstitute/BurishooleLTER-Public/tree/master/Random%20Burrishoole%20LTER%20useful%20information


wl<-read.csv("https://raw.githubusercontent.com/IrishMarineInstitute/BurishooleLTER-Public/master/Random%20Burrishoole%20LTER%20useful%20information/Daily_feeagh_mean_1976-2018.csv")
wl$date <-as.POSIXct(wl$date  ,format="%Y-%m-%d", tz="UTC")


plot(wl, type="l")

##to covert to disharge from Feeagh into Furnace, we use the Rating curve from Sean and Brians work, 
###based on flow measurements taken in Mill race and Salmon Leap and combined

####################################
####   Q (m3s???1) =15.5*(Feeagh Height (m))^1.71###
###################################


wl$disharge<-15.5*(wl$feeLvl_m)^1.71

plot(wl$date, wl$disharge,type="l", xlab="Date", ylab= expression(paste("Discharge m"^"3"*"s"^"-1")), col="blue")


