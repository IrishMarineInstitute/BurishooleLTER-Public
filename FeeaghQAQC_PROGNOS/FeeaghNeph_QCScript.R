# 18 July 2019
# Sean Kelly
# Feeagh Surface Nephelometer (turbidity data) data cleaning process

###
# Can download Feeagh AWQMS files from http://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.3757
# setwd("~/Desktop/Temp_WD/Feeagh_O2_QAQC")
###

# Start with 2015 (need to open .csv file first and get rid of the second row (units))
fee15 <- read.csv('FEEAGH_2015.csv')
str(fee15)
fee15$time <- as.POSIXct(fee15$time, format='%d/%m/%Y %H:%M', tz='UTC')

# take the nephelometer data + timestep:
feeNeph <- fee15[,c('time','Nephelometer')]
names(feeNeph) <- c('date','neph')

plot(feeNeph$date, feeNeph$neph,type='l') # very noisy/spiky

# first, average to 15 minute segments, as this is the highest frequency available for river flows anyway:
# NB not necessary to average to use the method below but you will want to play around with the smoothing parameters

library(openair) # handy time averaging function
feeNph15 <- timeAverage(feeNeph,avg.time='15 min') # 
plot(feeNph15$date, feeNph15$neph,type='l') # straight off, averaging smooths out some dodgy looking readings:
abline(h=c(40,125)) # looks reasonable to cut above and below

library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {if (any(rows)) {set(dataframe, rows, cols, newValue)}} # define function
# to exclude values outside a user defined range

outlierReplace(feeNph15, "neph", which(feeNph15$neph < 40,NA)) # set values to NA
outlierReplace(feeNph15, "neph", which(feeNph15$neph > 125,NA))

plot(feeNph15$date, feeNph15$neph,type='l') # should look much better

#######

# to despike remaining data - tricky to define a universal cut-off criteria, 
# as the 'spikes' are time/locally relative 
# e.g. a faulty spike at one point in the year may ~equal to normal values during an event at another time
# e.g. for 2015, compare a March spike to mean January values

#######

# A solution may be to create an over-smooth timeseries first:

# take the time and neph data
t <- feeNph15$date
x <- feeNph15$neph
plot(t,x,type='l') # this is the 15 min with extreme outliers removed

# need to deal with missing values for this to work correctly - for now carrying last obs. forward works best given what
# trying to achieve (may need to reconsider for longer gaps - will try a year with much missing data next and see)
library('zoo')
x <- na_locf(x) # i.e. 'replace missing values by carrying the last available value forward until next real value occurrs'
# ?na_locf()

# to create a very smoothed version, one option is to take a rolling average
xs <- rollmean(x, k=15*4*6, # here I go with 6 hours (set by k)
         align = 'center',na.pad=TRUE) # centre it (a trailing/leading not as applicable here)
xs <- na_locf(xs, option = "locf", na_remaining = "rev") # pad out beginning and end of series with last value
lines(t,xs,col='red',type='l') # this is our oversmoothed version - can see that it omits all the 'bad' data but 
# also higher freq. changes that may be valuable to keep

# So, to capture some of the realistic higher frequency detail:
# base QC version on the difference between the raw and the filtered (over-smooth) dataset.

hist(x-xs, breaks=100, main="") # histogram of raw minus the smoothed version 
# (i.e. shows the distribution of raw data departure from a smooth signal, set by the rolling mean)
n <- length(x)

# define cut-off criteria function:
A <- 0.3 # this is the key param to decrease/increase sensitivity of keeping/dropping noisey data 
# i.e. increase this to pass more noise/spikes, decrease to remove (smoothen)

dev <- x - xs
lambda <- A * qt(1 - 1/(2*n), df=n-1) # take the size of tha dataset into account, 
# when defining a statistical probability that an outlier is actually real

abline(v=mean(dev) + sd(dev) * lambda*c(-1, 1), lty=2) # data distributed L/R of dashed lines is cut 
# (tweak alpha argument above to visualise for different rejection criteria)

bad <- which((abs(dev - mean(dev))
              > (lambda * sd(dev)))) # define 'bad' spikes based on a probablility of finding outliers outside the smoothed version
xx <- x
xx[bad] <- xs[bad]
par(mfrow=c(2,1))
plot(t, x, type="l") # raw unfiltered
plot(t,xx,type='l') # with 'bad' spikes omitted - 
# the drop-outs in December when the sensor was acting up get filtered out which is handy!

# method is by no means perfect (e.g. I wonder if the high spike in December is real?) but perhaps a starting point 
# for automating larger sets of data. 

# Remember also that you can tweak how strict the rejection of spikes are by changing 'A' value (line 75)

feeNph15$nephClean <- xx # add in with timestep and noisier version


