# 23rd Oct 2018
# Resolving long-term EPA Feeagh staff gauge dataset (1976 - 2007)
# 
# Am using the hi-res EPA files sent by email to EdeE/MD by EPA (folder location S:\Feeagh Hydrographic data from EPA)
setwd("S:/Feeagh Hydrographic data from EPA/Sean_2018_reanalysis") # have saved a back-up version as txt files to work on
# setwd("~/Desktop/Temp_WD/Feeagh level analysis")

readLines("L.FEEAGH.W March 1976 to August 2001.txt", 
          n=20) #preview first ten rows - also very handy to check first datetime to see if follows in correct order
fee.1<-read.table("L.FEEAGH.W March 1976 to August 2001.txt", 
                  header=TRUE, skip=17, sep = "", fill = T)

fee.2 <- read.table("L.FEEAGH.W.15 August 2001 to December 2007.txt", 
                    header=TRUE, skip=17, sep = "", fill = T)
# the col headers all match up, so can bind:
fee <- rbind.data.frame(fee.1,fee.2)
#
str(fee) # the last few QC columns are empty - remove and rename remaining cols
fee$date<-as.POSIXct(paste(fee$Date, fee$Time), format="%d/%m/%Y %H:%M:%S", tz="UTC")
# re-order/arrange:
fee <- fee[,c(10,3:5)]
names(fee) <- c('date','wLvl','X.m','W.1') # the W.1 is handy as a QC flag (not sure what the 'X.m' column numbers do? Keep just in case
str(fee)
# sort data type (all messed up because of the weird text strings they used for missing data)
W.1 <- fee$W.1
indx <- sapply(fee, is.factor) #for any column that is a factor apply following function;
fee[indx] <- lapply(fee[indx], function(x) as.numeric(as.character(x))) #i.e. convert to numeric data, IF NOT A NUMBER = NA
str(fee)
fee$W.1 <- W.1
write.csv(fee,"Feeagh Level Mar1976 - Dec2007 EPA raw ts.csv", row.names=F)
###############
# raw ts plot:
plot(fee$date, fee$wLvl,type="l")
abline(h=0.2,lty=2)
abline(v=as.POSIXct("1995-09-23 00:00:00"),lty=2,col="red") #the shift is apparent after this date

# 1. create a uniform timestep (earlier (up till 2000s?) the timeseries uses random recording intervals)
# For uniform time intervals, only way is to average over 15-min intervals:
library("openair", lib.loc="~/R/win-library/3.3")
dBU <- fee # data backup... as the averaged data file won't have default QC flags per raw reading
fee <- timeAverage(fee, avg.time = "15 min",start.date = "1976-03-03 11:30:00")
plot(fee$date,fee$wLvl, type="l")
abline(h=0.18,lty=2,col="red")
abline(v=as.POSIXct("1995-09-23 00:00:00"),lty=2,col="red") #the shift is apparent after this date but may have happened anytime during the 1995 dry spell...
write.csv(fee, "Feeagh Level Mar1976 - Dec2007 EPA 15-min.csv", row.names=F)
##
# 2. Take daily averages:
feeDaily <- timeAverage(fee, avg.time = "24 hour",start.date = "1976-03-03 00:00:00")
plot(feeDaily$date,feeDaily$wLvl, type="l")
abline(h=0.18,lty=2,col="red")
abline(v=as.POSIXct("1995-09-23 00:00:00"),lty=2,col="red") #the shift is apparent after this date but may have happened anytime 1995 - 2000ish...

##
# 3. Add in the post-2007 level data which has been gap-fill using linear regression with MR  
fee.2008 <- read.csv('Feeagh Level Dec2007 - JULY2018 DAILY with 2008-09 filled with MRmodel.csv') # 
str(fee.2008)
plot(fee.2008$Lvl)
fee.2008$Date <- as.POSIXct(fee.2008$Date,format='%d/%m/%Y',tz='UTC')
summary(fee.2008)
plot(fee.2008$Date,fee.2008$Lvl,type="l") ###THIS IS THE FROM 2008 - NOW; WHERE THE EPA GAUGE WAS OUT HAS BEEN FILLED USING SIMPLE LINEAR REGRESSION WITH MR

names(fee.2008) <- c('date','wLvl','X.m') # rename before binding to the 1976-2007 data

feeDaily <- rbind.data.frame(feeDaily, fee.2008)
str(feeDaily)
plot(feeDaily$date,feeDaily$wLvl,type='l') # this is the full one, with small gaps still in and breakpoint

# as most remaining gaps are pretty small, safe enough to linearly interp for now.
date <- feeDaily$date
m <- as.matrix(feeDaily[,2])
library('zoo')
m.fill <- na.approx(m)
summary(m.fill)
m<-as.data.frame(m.fill)
feeDaily.fill <- cbind(date,m)
summary(feeDaily.fill)
rm(m,m.fill,date)
plot(feeDaily.fill$date, feeDaily.fill$V1,type='l') # looks OKAY FOR NOW

# CREATE THE TIMESERIES OBJECT:
fee.ts <- ts(feeDaily.fill$V1,
             start=c(1976,3,3), frequency = 365.25)
plot(fee.ts)

fee.tsComponents <- decompose(fee.ts)
plot(fee.tsComponents) # can see the increasing trend over time but this is the breakpoint shift in the timeseries where everything goes up

### Pettit's Test ###
library("trend", lib.loc="~/anaconda3/lib/R/library")
data(fee.ts) ; plot(fee.ts)
s.res <- pettitt.test(fee.ts)
n <- s.res$nobs # number of obs in timeseries
i <- s.res$estimate # observation number of first significant breakpoint
s.1 <- mean(fee.ts[1:i]) # mean of observations prior to breakpoint
s.2 <- mean(fee.ts[(i+1):n]) # mean of observations following breakpoint
s <- ts(c(rep(s.1,i), rep(s.2,(n-i))))
tsp(s) <- tsp(fee.ts) # assigns the same timespance as the fee.ts series
lines(s, lty=2, col='red',lwd=3) # line showing the two averages before and after as continuous timeseries
print(s.res) # obs number 7848 or 1997-08-27 is probably breakpoint down to day...

# so try adding the difference between the two means before and after the breakpoint:
s.diff = s.2-s.1
# now need to isolate the before component:
fee.tsPre <- fee.ts[1:7848]
fee.tsPre <- ts(fee.tsPre,
                start=c(1976,3,3), frequency = 365.25)

plot(fee.tsPre)
fee.tsPre <- fee.tsPre + s.diff # add the difference in means before and after to shift upward...
lines(fee.tsPre,col='red',lty=2) # looks about right, now take Fee ts post

fee.tsPost <- fee.ts[7849:15487]
fee.tsPost <- ts(fee.tsPost,
                 start=c(1997,08,28), frequency = 365.25) # so day after the breakpoint!
plot(fee.tsPost) # will circle back around to this and fill in the gap with a Mill Race regression like Ola

fee.tsMod <- append(fee.tsPre,fee.tsPost) # add the two together
fee.tsMod <- ts(fee.tsMod,
                start=c(1976,3,3), frequency = 365.25)

# now plot altogether:
plot(fee.tsMod,lwd=3) # full timeseries axes
lines(fee.tsPre,col='red') # this is the pre-breakpoint timeseries with the shift value incorporated
lines(fee.tsPost,col='blue') # this is the post-breakpoint timeseries, with nothing done to the raw values
lines(fee.ts,col='red',lwd=0.5)
plot(fee.tsMod,type='n',xaxs='i')
plot(fee.ts)
lines(fee.ts, lwd=0.5,lty=3,col='red')

plot(fee.tsMod)
s.res <- pettitt.test(fee.tsMod)
n <- s.res$nobs # number of obs in timeseries
i <- s.res$estimate # observation number of first significant breakpoint
s.1 <- mean(fee.ts[1:i]) # mean of observations prior to breakpoint
s.2 <- mean(fee.ts[(i+1):n]) # mean of observations following breakpoint
s <- ts(c(rep(s.1,i), rep(s.2,(n-i))))
tsp(s) <- tsp(fee.ts) # assigns the same timespance as the fee.ts series
lines(s, lty=2, col='red',lwd=3) # line showing the two averages before and after as continuous timeseries
print(s.res) # obs number 7848 or 1997-08-27 is probably breakpoint down to day...

fee.tsComponents <- decompose(fee.ts) # with the breakpoint
plot(fee.tsComponents) # can see the increasing trend over time but this is the breakpoint shift in the timeseries where everything goes up

fee.tsModComponents <- decompose(fee.tsMod) # without the breakpoint
plot(fee.tsModComponents) # no real increasing trend over time

# Check Mann-Kendall test before and after (for trend significance)
mk.test(fee.ts)
mk.test(fee.tsMod) # not significant

cs.test(fee.ts) # highly significant trend - monotonic trend between 1st and last third apparent
cs.test(fee.tsMod) # not significant (p = 0.222). Reject the alternative hypothesis of a monotonic trend - nothing apparent!!! Great.


### finished here - sort out these plots
date <- feeDailyRaw$date
y <- c(1:15487)
i <- feeDailyRaw$fee.ts
par(mfrow=c(2,1))
plot(y, i, type="l", col="gray",
     xlab="Year",ylab="Level (m)")

C <- cut(y, breaks=seq(ceiling(min(y)), # cut() not defined for POSIXct objects... use a number index corresponding to days from 1976-03-03
                       floor(max(y)), 365.25)) # use 1-year windows
ys <- split(y, C)
is <- split(i, C)

library(plyr)
ymean <- laply(ys, mean)
imean <- laply(is, mean)
lines(ymean, imean, lwd=2)


# now do one with the Feeagh modified:
date <- feeDailyMod$date
y <- c(1:15487)
i <- feeDailyMod$fee.tsMod
#par(mfrow=c(2,1))
plot(y, i, type="l", col="gray",
     xlab="Year",ylab="Level (m)")

C <- cut(y, breaks=seq(ceiling(min(y)), # cut() not defined for POSIXct objects... use a number index corresponding to days from 1976-03-03
                       floor(max(y)), 365.25))
ys <- split(y, C)
is <- split(i, C)

library(plyr)
ymeanMod <- laply(ys, mean)
imeanMod <- laply(is, mean)
lines(ymeanMod, imeanMod, lwd=2)
lines(ymean, imean, lwd=2,col='red') # add in the un-modified timeseries

# would have liked to test using the Buishand (1984) method for hydrological timeseries
# however daily keeps crashing the MACBOOK!
# TRY MONTHLY
summary(feeMonth) # couple of gaps to fill:
date <- feeMonth$date
m <- as.matrix(feeMonth[,2])
library('zoo')
m.fill <- na.approx(m)
summary(m.fill)
m<-as.data.frame(m.fill)
feeMonth.fill <- cbind(date,m)
summary(feeMonth.fill)
rm(m,m.fill,date)
plot(feeMonth.fill$date, feeMonth.fill$wLvl,type='l') # looks OKAY FOR NOW

fee.tsMonthly <- ts(feeMonth.fill$wLvl,
                start=c(1976,3), frequency = 12)
plot(fee.tsMonthly)

res <- bu.test(fee.tsMonthly)
par(mfrow=c(2,1))
plot(fee.tsMonthly); plot(res)

# last thing - look at the trendline for the daily ts
plot(feeDaily$date,feeDaily$wLvl,type='p')
feeTrend <- lm(feeDaily$wLvl~feeDaily$date)
summary(feeTrend)
abline(feeTrend,col='red',lwd=4) # see if increasing over time.
# Good.

### next thing is isolate the winter months and plot 2015 in 
# historical context, so essentially winter 76/77 -> winter 2017/18,
# daily mean and max/min?
write.csv(feeDailyMod,'Dataframe Feeagh Daily Mean 1976-2018 with pre-1997 shift correction.csv',row.names=F)

# I had done this in ggplot before... start a new script

