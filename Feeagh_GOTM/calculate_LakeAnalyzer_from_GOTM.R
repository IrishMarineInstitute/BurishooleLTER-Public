#' Reading in GOTM output and convert to LakeAnalyzer format
#' Author: Tadhg Moore
#' Date: 2021-03-15

rm(list=ls())
setwd("H:/Elvira/Lake modelling/GOTM_EdeE/GOTM_EdeE/feeagh_2020 - sean")

library(gotmtools)
library(rLakeAnalyzer)
# library(plyr) # If you want to load the packages - code is written so you don't have to
# library(tidyr)

# Need to be in the GOTM_2020 folder
out <- "output.nc" # Select GOTM output
wtemp <- get_vari(ncdf = out, var = 'temp') # Load in water temperature
z <- get_vari(ncdf = out, var = 'z') # Load in corresponding depths
df <- wide2long(data = wtemp, depths = z) # Convert from wide to long
length(unique(df$depths)) # How many unique depths there are  with varying water level. there can be >1,000 uniques depths
df$depths <- round(df$depths) # round to nearest meter for simplicity
ref_depth <- max(df$depths) # reference depth  to calculate depth from the surface
df$depths <- (ref_depth - df$depths) # Correct the depths qith surface = 0m

# Calculate the average if there are duplicate depths because of rounding
temp_avg <- plyr::ddply(df, c("date", "depths"), function(x) mean(x$temp))
head(temp_avg)

# Go from long to wide format
df2 <- tidyr::pivot_wider(temp_avg, names_from = depths, names_prefix = "wtr_", values_from = V1)
df2 <- as.data.frame(df2) # Convert from tibble to data.frame (annoying tidyverse shite)

str(df2)

# Now is ready for LakeAnalyzer functions
td <- ts.thermo.depth(df2, na.rm = TRUE)
plot(td, col = 1, type = "l", ylim = c(45,0))

# Load GOTM hypsograph and prepare for rLakeAnalyzer
fgh.bath <- read.delim("hypsograph.dat")
colnames(fgh.bath) <- c("depths", "areas")
fgh.bath$depths <- abs(fgh.bath$depths)

#Calculate Schmidt
ss <- ts.schmidt.stability(df2, fgh.bath, na.rm = TRUE)
plot(ss, type = "l")

ss_sean<-ss

####compare to mill race temp

mr<-read.csv("millrace_temp_jan_2021.csv")


mr$datetime<-as.POSIXct(mr$datetime, format = '%Y-%m-%d ', tz = 'UTC') #Format into datetime

srf<-df2[c(1,2,3,4,30)]
colnames(srf)[1] <- 'datetime'

library(dplyr)

com<-left_join(srf, mr, by=c("datetime"))



par(mfrow=c(1,1), mar=c(2,4.5,1,5), oma=c(1,0,0,0)) 
plot(com$datetime, com$wtr_1, type="l", col="deepskyblue3", xaxt = "n", ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))

axis.POSIXct(1, at = seq(as.POSIXct("2019-10-25"), max(com$datetime)+1, "months"),
             labels = TRUE, tcl = -0.6, srt=45, format="%m-%y")

par(new=TRUE)

plot(com$datetime, com$temp, type="l",col="darkorange", xaxt = "n", lwd=2, ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))

par(new=TRUE)
plot(com$datetime, com$wtr_28, type="l",col="red", xaxt = "n", ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))


legend("bottomleft", c( "Modelled wtr_1", "Modelled wtr_28", "Mill race temp"),
       fill = c("deepskyblue3", "red", "darkorange"), bty="n", cex=0.6)

com_sean<-com



names(df2)[1] <- 'datetime'
write.table(df2, file = "modelled_2020.wtr", sep = "\t", row.names = FALSE)

write.table(df2, file = "H:/Elvira/R stuff/HFM data stuff/Lake analyser sumnmary graphs/modelled_2020.wtr", sep = "\t", row.names = FALSE)

















#' Reading in GOTM output and convert to LakeAnalyzer format
#' Author: Tadhg Moore
#' Date: 2021-03-15
setwd("H:/Elvira/Lake modelling/GOTM_EdeE/GOTM_EdeE/feeagh_2020")

library(gotmtools)
library(rLakeAnalyzer)
# library(plyr) # If you want to load the packages - code is written so you don't have to
# library(tidyr)

# Need to be in the GOTM_2020 folder
out <- "output.nc" # Select GOTM output
wtemp <- get_vari(ncdf = out, var = 'temp') # Load in water temperature
z <- get_vari(ncdf = out, var = 'z') # Load in corresponding depths
df <- wide2long(data = wtemp, depths = z) # Convert from wide to long
length(unique(df$depths)) # How many unique depths there are  with varying water level. there can be >1,000 uniques depths
df$depths <- round(df$depths) # round to nearest meter for simplicity
ref_depth <- max(df$depths) # reference depth  to calculate depth from the surface
df$depths <- (ref_depth - df$depths) # Correct the depths qith surface = 0m

# Calculate the average if there are duplicate depths because of rounding
temp_avg <- plyr::ddply(df, c("date", "depths"), function(x) mean(x$temp))
head(temp_avg)

# Go from long to wide format
df2 <- tidyr::pivot_wider(temp_avg, names_from = depths, names_prefix = "wtr_", values_from = V1)
df2 <- as.data.frame(df2) # Convert from tibble to data.frame (annoying tidyverse shite)

str(df2)

# Now is ready for LakeAnalyzer functions
td <- ts.thermo.depth(df2, na.rm = TRUE)
plot(td, col = 1, type = "l", ylim = c(45,0))

# Load GOTM hypsograph and prepare for rLakeAnalyzer
fgh.bath <- read.delim("hypsograph.dat")
colnames(fgh.bath) <- c("depths", "areas")
fgh.bath$depths <- abs(fgh.bath$depths)

#Calculate Schmidt
ss <- ts.schmidt.stability(df2, fgh.bath, na.rm = TRUE)
plot(ss, type = "l")

ss_tadhg<-ss

####compare to mill race temp

#mr<-read.csv("millrace_temp_jan_2021.csv")


#mr$datetime<-as.POSIXct(mr$datetime, format = '%Y-%m-%d ', tz = 'UTC') #Format into datetime

srf<-df2[c(1,2,29)]
colnames(srf)[1] <- 'datetime'

library(dplyr)

com<-left_join(srf, mr, by=c("datetime"))



par(mfrow=c(1,1), mar=c(2,4.5,1,5), oma=c(1,0,0,0)) 
plot(com$datetime, com$wtr_1, type="l", col="deepskyblue3", xaxt = "n", ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))

axis.POSIXct(1, at = seq(as.POSIXct("2019-10-25"), max(com$datetime)+1, "months"),
             labels = TRUE, tcl = -0.6, srt=45, format="%m-%y")

par(new=TRUE)

plot(com$datetime, com$temp, type="l",col="darkorange", xaxt = "n", lwd=2, ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))

par(new=TRUE)
plot(com$datetime, com$wtr_28, type="l",col="red", xaxt = "n", ylim=c(0,20),xlab="", ylab= expression(paste("watertemp")))


legend("bottomleft", c( "Modelled wtr_1", "Modelled wtr_28", "Mill race temp"),
       fill = c("deepskyblue3", "red", "darkorange"), bty="n", cex=0.6)


com_tadhg<-com


###compare 2 models with mill race temp


par(mfrow=c(1,1), mar=c(2,4.5,1,5), oma=c(1,0,0,0)) 
plot(com_sean$datetime, com_sean$wtr_1, type="l", col="deepskyblue3", xaxt = "n", ylim=c(0,25),xlab="", ylab= expression(paste("watertemp")))

axis.POSIXct(1, at = seq(as.POSIXct("2019-10-25"), max(com$datetime)+1, "months"),
             labels = TRUE, tcl = -0.6, srt=45, format="%m-%y")

par(new=TRUE)

plot(com_sean$datetime, com_sean$temp, type="l",col="darkorange", xaxt = "n", lwd=2, ylim=c(0,25),xlab="", ylab= expression(paste("watertemp")))

par(new=TRUE)
plot(com_tadhg$datetime, com_tadhg$wtr_1, type="l",col="green", xaxt = "n", ylim=c(0,25),xlab="", ylab= expression(paste("watertemp")))


legend("bottomleft", c("Modelled wtr_1_no cc or discharge", "Modelled wtr_1 with cc and discharge", "Mill race temp"),
       fill = c("deepskyblue3", "green","darkorange"), bty="n", cex=0.6)





#####compare ss

par(mfrow=c(1,1), mar=c(2,4.5,1,5), oma=c(1,0,0,0)) 
plot(ss_sean$datetime, ss_sean$schmidt.stability, type="l", col="deepskyblue3", xaxt = "n",lwd=2, ylim=c(0,800),xlab="", ylab= expression(paste("ss")))

axis.POSIXct(1, at = seq(as.POSIXct("2019-10-25"), max(ss_sean$datetime)+1, "months"),
             labels = TRUE, tcl = -0.6, srt=45, format="%m-%y")

par(new=TRUE)

plot(ss_tadhg$datetime, ss_tadhg$schmidt.stability, type="l",col="darkorange", xaxt = "n", lwd=2, ylim=c(0,800),xlab="", ylab= expression(paste("ss")))


legend("topleft", c("-discharge - cc ", "+discharge +cc" ),
       fill = c("deepskyblue3", "darkorange"), bty="n", cex=0.6)

