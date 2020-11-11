setwd("C:/Users/edeeyto/Desktop/data.marine.ie updates/AWQMS updates/2019")
afst<-read.csv( "Feeagh_2019.csv",header = TRUE)
afst$time<-as.POSIXct(afst$time, format="%d/%m/%Y %H:%M") 
afst <- afst[-c(1),] 
str(afst) #check each variable - some very dodgy values so need to clean these
summary(afst)


#make all varibales numeric
fact.col <- names(Filter(is.factor,afst))[-1]
#Convert factors to columns
for(i in fact.col){
  afst[,i] <- as.numeric(as.character(afst[,i]))
  print(paste0(i,' converted from factor to numeric'))
}
str(afst)
names(afst)

plotraw<-afst[,-c(1,2,4, 26, 27,39,40,41,42,43,44,45 )]
str(plotraw)
summary(plotraw)

####fix outliers#####

library(data.table)#need data.table library
#following function basically allows to replace values outside of a user defined range as NA
outlierReplace = function(dataframe, cols, rows, newValue = NA) {if (any(rows)) {set(dataframe, rows, cols, newValue)}}

plot<-plotraw
names(plot)
summary(plot)

#watertemp
for(i in c(2:13)){
  n <- which(plot[,i] < 0 | plot[,i] >40)
  plot[n,i] <- NA
}

#air temperature (-20-40)
outlierReplace(plot, "AirTemperature", which(plot$AirTemperature < -20 | plot$AirTemperature > 40,NA))
plot(plot$time,plot$AirTemperature,type="l") 

#Barometric_Pressure_1 (900-1100)
outlierReplace(plot, "Barometric_Pressure_1", which(plot$Barometric_Pressure_1 < 900 | plot$Barometric_Pressure_1 > 1100,NA))
plot(plot$time,plot$Barometric_Pressure_1,type="l") 

#Nephelometer (80-140)
outlierReplace(plot, "Nephelometer", which(plot$Nephelometer < 0 | plot$Nephelometer > 200,NA))
plot(plot$time,plot$Nephelometer,type="l") 

#Chlorophyll_A (400-900)
outlierReplace(plot, "Chlorophyll_A", which(plot$Chlorophyll_A < 100 | plot$Chlorophyll_A > 1000,NA))
plot(plot$time,plot$Chlorophyll_A,type="l")

#CDOM(50-200)
outlierReplace(plot, "CDOM", which(plot$CDOM < 50 | plot$CDOM > 300,NA))
plot(plot$time,plot$CDOM,type="l")

#Pyranometer(0-2000)
outlierReplace(plot, "Pyranometer", which(plot$Pyranometer < 0| plot$Pyranometer > 1500,NA))
plot(plot$time,plot$Pyranometer,type="l")

#Surface_PFD(0-3000)
outlierReplace(plot, "Surface_PFD", which(plot$Surface_PFD < 0| plot$Surface_PFD > 3500,NA))
plot(plot$time,plot$Surface_PFD,type="l")

#Underwater_PFD(0-1000)
outlierReplace(plot, "Underwater_PFD", which(plot$Underwater_PFD< 0| plot$Underwater_PFD > 1000,NA))
plot(plot$time,plot$Underwater_PFD,type="l")

#Anemometer(0-50)
outlierReplace(plot, "Anemometer", which(plot$Anemometer< 0| plot$Anemometer > 100,NA))
plot(plot$time,plot$Anemometer,type="l")

#Bearing(~300)  perhpas dont take out outliers here, as if there is an outlier, it indicates a real swing
outlierReplace(plot, "Bearing", which(plot$Bearing< 0| plot$Bearing > 360,NA))
plot(plot$time,plot$Bearing,type="l")

#Wind vane(0-360)
outlierReplace(plot, "Wind_Vane", which(plot$Wind_Vane< 0| plot$Wind_Vane > 360,NA))
plot(plot$time,plot$Wind_Vane,type="l")

#Corrected_Wind_Direction(0-360)
outlierReplace(plot, "Corrected_Wind_Direction", which(plot$Corrected_Wind_Direction< 0| plot$Corrected_Wind_Direction > 360,NA))
plot(plot$time,plot$Corrected_Wind_Direction,type="l")

#Sonde Depth(~2.4 ) purely indicative. its not actually at 2.4 metres
outlierReplace(plot, "Sonde_Depth", which(plot$Sonde_Depth< 0| plot$Sonde_Depth > 10,NA))
plot(plot$time,plot$Sonde_Depth,type="l")

#Sonde_Temperature(0-40)
outlierReplace(plot, "Sonde_Temperature", which(plot$Sonde_Temperature< 0| plot$Sonde_Temperature > 40,NA))
plot(plot$time,plot$Sonde_Temperature,type="l")

#Sonde_SpCo(0-1)
outlierReplace(plot, "Sonde_SpCo", which(plot$Sonde_SpCo< 0| plot$Sonde_SpCo > 1,NA))
plot(plot$time,plot$Sonde_SpCo,type="l")

#Sonde_Salinity(0-1) (meaningless for feeagh)
outlierReplace(plot, "Sonde_Salinity", which(plot$Sonde_Salinity< 0| plot$Sonde_Salinity > 1,NA))
plot(plot$time,plot$Sonde_Salinity,type="l")

#Sonde_DOPC(0-120)
outlierReplace(plot, "Sonde_DOPC", which(plot$Sonde_DOPC< 0| plot$Sonde_DOPC > 120,NA))
plot(plot$time,plot$Sonde_DOPC,type="l")

#Sonde_DO_mgl(0-14)
outlierReplace(plot, "Sonde_DO_mgl", which(plot$Sonde_DO_mgl< 0| plot$Sonde_DO_mgl > 14,NA))
plot(plot$time,plot$Sonde_DO_mgl,type="l")

#Sonde_pH(0-14)
outlierReplace(plot, "Sonde_pH", which(plot$Sonde_pH< 5| plot$Sonde_pH > 14,NA))
plot(plot$time,plot$Sonde_pH,type="l")

#Sonde_ChlA(0-??)
outlierReplace(plot, "Sonde_ChlA", which(plot$Sonde_ChlA< 0| plot$Sonde_ChlA > 1,NA))
plot(plot$time,plot$Sonde_ChlA,type="l")


###maybe do daily avergae just for plots
library(openair)
colnames(plot)[1] <- "date"
daily <- timeAverage(plot, avg.time = "day")
summary(daily)

str(daily)

daily<- as.data.frame(daily) 

write.csv(daily,"daily.csv",  row.names=FALSE)

#library(readr)
#daily <- read.csv("daily.csv", header = TRUE, colClasses=c("date"="Date"))
#class(daily) # check daily is a "data.frame"...
#class(daily$date) # check daily$date is a "Date"...

plot(daily$Water_Temp_2m~daily$date)

figures_directory <- "C:/Users/edeeyto/Desktop/data.marine.ie updates/AWQMS updates/2019/figures/,"

# SET THIS TO YOUR DATAFRAME AND RUN LOOP BELOW (e.g., df <- zooplankton_data)
df <- daily
str(df)
names(df)
##need to take out air temp
df<-df[-c(14)]

# -------------------------------------------------------------------------------------------------------
# EXPORT PLOTS AS PDF
# -------------------------------------------------------------------------------------------------------
# as lower resolution pdf
lapply(2:32, FUN = function(x){
  pdf(file = paste0(figures_directory,colnames(df)[x]," 2019.pdf"))
   plot(x = df[,1], y = df[,x],
       xlab = colnames(df)[1], 
       ylab = colnames(df)[x],
       tck = 0.02)
  dev.off()
})



