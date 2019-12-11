# 3rd Dec 2019
# Author: Sean Kelly (sean.kelly@dkit.ie)
# Code for deep-water ventilation predictive model #

###----------------------------------------------###
###----------------------------------------------###

# load necessary libraries (if not installed
# these must be installed first through command 'install.packages(Package_Name)')
library('zoo')
library("dplyr")
library("openair")
library("randomForest")

###----------------------------------------------###
#         read in the necessary datasets
###----------------------------------------------###

# set working directory folder to location with downloaded files
setwd("INSERT_FOLDER_ADDRESS_HERE")
# load in necessary datasets:
d <- read.csv('Daily_forcing_data.csv') # contains the necessary driver variables at daily ts
d$date <- as.POSIXct(d$date,format='%Y-%m-%d',tz='UTC') # create time object for date column
do <- read.csv('DO_2009-18_DAILY.csv') # contains the long-term oxygen profiles at daily ts
do$date <- as.POSIXct(do$date,format='%Y-%m-%d',tz='UTC')

###----------------------------------------------###
# construct a dataset with all of the model params
###----------------------------------------------###

# assign each of the model driver variables:
date = d$date # take the daily timestamp
fw = d$discharge # daily freshwater discharge
tauxClew = d$tau.xClew1# alongshore windstress Clew Bay ERA5
tauyClew = d$tau.yClew1 # across-shore windstress Clew Bay ERA5
cbl = d$msl # msl from ERA5
varsRF = data.frame(date = date, fw = fw,tauxClew=tauxClew,tauyClew=tauyClew,cbl=cbl) # add to model dataframe
# create 30-day trailing averages:
varsRF$fwMA <- rollmean(varsRF$fw,30,fill=median(varsRF$fw),align='right') # also add in a 30 day trailing moving average of freshwater discharge
varsRF$cblMA <- rollmean(varsRF$cbl,30,fill=median(varsRF$cbl),align='right') # also add in a 30 day trailing moving average of surface pressure
varsRF$tauxClewMA <- rollmean(varsRF$tauxClew,30,fill=median(varsRF$tauxClew),align='right') # also add in a 30 day trailing moving average of tau-x
varsRF$tauyClewMA <- rollmean(varsRF$tauyClew,30,fill=median(varsRF$tauyClew),align='right') # also add in a 30 day trailing moving average of tau-y

# take the bottom oxygen reading as indicator of ventilation events (i.e. when changes from anoxia)
doBott <- doDeep[,c(1,84)] 
names(doBott) <- c('date','do')

# now add the output (oxygen) to the input variable dataframe and call 'model':
model <- left_join(varsRF, doBott, by=c('date'))
summary(model)
model <- timeAverage(model,avg.time = '1 week') # average everything to weekly

###----------------------------------------------###
# N.B. the following section is specific to this 
# dataset; may not be required for other datasets
###----------------------------------------------###

# the long-term oxygen timeseries has missing data 
# i.e. gap filling required for model:
plot(model$date,model$do,type='l',col='red') # can see obvious ventilation events - 
# some have gaps between them so need to fill these in.

# using oxygen consumption rate in manuscript, takes ~ 2-3 months to revert to anoxia 
# i.e. set bottom DO to zero 2 months after a ventilation with missing data and interpolate
# otherwise get unrealistically long oxygen depletion rate e.g. for 2014-2015 gap
# a ventilation occurred in Sept/Oct then a data gap; we will assume anoxic by end of December
which(model$date=='2014-12-22') # row 313; should be back to anoxic conditions at this point
model$do[c(1,313,314)] <- c(0.09,0.09,0.09) # set to typical stagnant value

model$doFill <- na.spline(model$do) # now gap fill using a spline function
lines(model$date,model$doFill,type='l',col='blue') # 

# create a categorical variable for stagnant vs ventilated regime:
model$doCat <- cut(model$doFill, c(-1,0.9,15),labels=c(0,1)) # 
# this created two categories; one where bottom oxygen is < 0.9 mg l (stagnant)
# (and > -1 to account for spurious values assigned by the spline function)
# and one where it is greater than 0.9 (with 15 as an upper bound)
summary(model$doCat) # here 0 is stagnant, 1 is ventilated
plot(model$date,model$doCat) # see here the 5 ventilation events (2010, 2013, 2014, 2017, 2018)

###----------------------------------------------###
# construct predictive model and interpret results
###----------------------------------------------###

# Now begin to construct the predictive model
set.seed(17) # for reproducibility

# here are the final model variables for constructing 
# the random forest:
modelRF <- 
  model[,c(6,7,8,12)] # variables included: fwMA, cblMA (era5) 
                      # and tauxMA(era5) and response is doCategory

# construct the random forest algorithm:
classifier <- randomForest(doCat~.,data=modelRF,
                           ntree=5000,mtry=2,importance=TRUE,do.trace=TRUE) 
# 5000 classification trees with 2 nodes per tree
# see ?randomForest() for arguments
predict(classifier, type="prob") #'predict' (type '?predict()') but specify a probability 
# this gives the percent probability of stagnation (0) or ventilation (1)

print(classifier) # summary of model including out-of-bag error rate (11%)

# rank model variables in terms of importance on oxygen prediction:
varImpPlot(classifier,main='Variable Importance') # first plot shows for each variable, 
# if it is assigned values by random permutation, how much the MSE increases; 
# second plot is node purity neasured by Gini index - measures difference in 
# RSS before and after the split on that variable

###----------------------------------------------###
#        generalise the model results 
###----------------------------------------------###


# now use the random forest algorithm to predict a probablility of ventilation using
# the timeseries of driving variables and compare output to observed deep oxygen (Fig. 9 in MS)
summary(model)
predGen <- predict(classifier,newdata=model,type='prob') # generalised predictions, with the full model dataset as input predictors

xTicks = seq(from=as.POSIXct("2009-01-01 00:00:00",tz="UTC"),to=as.POSIXct("2018-01-01 00:00:00",tz="UTC"),by="12 month") # define the time axis format

pdf('R_figure.pdf',width=12,height=8)

par(mar=c(5, 5, 4, 1),mfrow=c(2,1)) # 
plot(model$date,predGen[,2],type='p',col='black',xlab='',ylab='Probability',
     xaxt='n',cex.main=2,cex.lab=1.5,cex.axis=1.3,cex=1.3,pch=16)
axis.POSIXct(1,at=xTicks,tcl=-0.5,cex.axis=1.45,format='%Y')
abline(h=0.8,col='black',lty=3,lwd=2) # values above 80% probability
mtext('(a)', side = 3, line = -.05, adj = -0.075, cex = 1.25,
      col = "black")

plot(model$date,model$doFill,type='l',col='black',xlab='Year',
     ylab=expression(paste('Oxygen'~(mg~l^{-1}))),xaxt='n',cex.main=2,
     cex.lab=1.5,cex.axis=1.3,lwd=2)
axis.POSIXct(1,at=xTicks,tcl=-0.5,cex.axis=1.45,format='%Y')
mtext('(b)', side = 3, line = -.05, adj = -0.075, cex = 1.25,
      col = "black")

# to add the legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('center',legend=c('Modelled ventilation probability','Observed Bottom oxygen (inner basin)'),
       xpd=TRUE,horiz = TRUE, inset=c(0,0),bty='n',pch=c(16,NA),
       lty=c(0,1),lwd=c(0,2),col=c('black','black'),cex = 1.3)

dev.off()

###----------------------------------------------###
###----------------------------------------------###





