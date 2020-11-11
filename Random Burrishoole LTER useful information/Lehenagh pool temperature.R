
setwd("H:/Elvira/R stuff/HFM data stuff/Lehenagh pool")

#install.packages('rerdapp') #Run this if you need to install the package
##or try this if install doesnt work  
#devtools::install_github("ropensci/rerddap")

library(rerddap)

#Set environment erddap url, means you don't need to specify the url in your functions
Sys.setenv(RERDDAP_DEFAULT_URL="http://erddap.marine.ie/erddap/")

#find the datasetID on the ERDDAP site

#have a look at it

info("ICTempNetwork")

(out <- info("ICTempNetwork"))



df<-tabledap("ICTempNetwork", "time>=2018-01-01", "time<=2019-01-01")

temp<-df[c(1,7,8,9)]
temp<-as.data.frame(temp)
str(temp)

##just take out Lenhenagh
le<-subset(temp, SiteName =='Lehanagh Pool')
str(le)

#Convert time to Datetime object and inspect for NA's
#Give us the time range

le$time <- as.character(le$time) #Convert from factor to character
le$time <- gsub('T',' ',le$time) #Remove the T 
le$time <- gsub('Z','',le$time) #Remove the Z
le$time<-as.POSIXct(le$time, format = '%Y-%m-%d %H:%M:%S')

le$Depth<-as.numeric(le$Depth)
le$SeaTemperature<-as.numeric(le$SeaTemperature)

#convert long to wide
library(tidyverse)

wide = le%>% 
  spread(Depth,SeaTemperature)

#sumamrise to 5 mins to get them all ont he same row

library(openair)
wide$date<-wide$time
temp<- timeAverage(wide, avg.time = "1 hour", statistic="max")

temp<-temp[c(-2)]
temp<-temp[c(1,6,5,4,3,2)]
colnames(temp)<-c("datetime", "wtr_1", "wtr_4","wtr_8", "wtr_12","wtr_16")

library(rLakeAnalyzer)
get.offsets(temp)
str(temp)
temp<-as.data.frame(temp)

wtr.lineseries(temp)
wtr.heat.map(temp)


pdf(file="Le.temp.pdf",width=12,height=10) 

#water temperature (line)

par(mfrow=c(2,1))
wtr.lineseries(temp)
wtr.heat.map(temp)

dev.off()


write.table(temp, file = "Le.wtr", sep = "\t",
            row.names = FALSE)
