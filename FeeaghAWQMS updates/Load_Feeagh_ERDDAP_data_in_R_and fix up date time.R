
#install.packages('rerdapp') #Run this if you need to install the package


library(rerddap)

#Set environment erddap url, means you don't need to specify the url in your functions
Sys.setenv(RERDDAP_DEFAULT_URL="http://erddap.marine.ie/erddap/")

buoy <- 'IMINewportBuoys' #dataset id -buoy

#View variables
info(buoy)$variables

#Set start and stop dates to pull data from
start = '2019-08-20'
stop = '2019-10-10'

wtemp_max = 30
wtemp_min = 0

#List desired variable names- i.e. water temperature profile

samp<-tabledap('IMINewportBuoys' , paste0('time>=',start), paste0('time<=',stop))

#If you prefer to work in dataframes rather than tibble ;)
df <- as.data.frame
str(df)


fgh <- samp[,-(1:5)]
fgh<-fgh[,-(2:4)]
print(summary(fgh))
```

Convert factors to numeric 
```{r}
fact.col <- names(Filter(is.factor,fgh))[-1]
#Convert factors to columns
for(i in fact.col){
  fgh[,i] <- as.numeric(as.character(fgh[,i]))
  print(paste0(i,' converted from factor to numeric'))
}
```

str(fgh)

#Convert time to Datetime object and inspect for NA's
#Give us the time range
```{r}
fgh$time <- as.character(fgh$time) #Convert from factor to character
fgh$time <- gsub('T',' ',fgh$time) #Remove the T 
fgh$time <- gsub('Z','',fgh$time) #Remove the Z

#Select which datetime format for formatting the date time
if(!is.na(as.POSIXct(fgh$time[nrow(fgh)], format = '%d/%m/%Y %H:%M:%S'))){
fmt = '%d/%m/%Y %H:%M:%S'
}else if(!is.na(as.POSIXct(fgh$time[1], format = '%Y-%m-%d %H:%M:%S'))){
fmt = '%Y-%m-%d %H:%M:%S'
}

fgh$time <- as.POSIXct(fgh$time, format = fmt, tz = 'UTC') #Format into datetime
date.na = sum(is.na(fgh$time))
print(paste0('Start: ', range(fgh$time, na.rm = T)[1], '; End: ', range(fgh$time, na.rm = T)[2]))
print(paste('No. of NAs in time:',date.na))
```


#Check timesteps - fix if neccessary. The feeagh AWQMS recors in 2 min intervals
```{r}
print(dim(fgh))
print(summary(fgh$time))
dates = seq(from = range(fgh$time)[1],to = range(fgh$time)[2], by =120)
dif <- as.numeric(fgh[2:nrow(fgh),'time']-fgh[1:(nrow(fgh)-1),'time'])
sam <- fgh[order(fgh[,'time']),]
if(length(dates) != nrow(fgh)){
df = data.frame(time = dates,
test =rep(NA,length(dates)), 
stringsAsFactors=FALSE) 
df = merge(df,sam, by ='time', all.x = T)
df$test <- NULL
fgh <- df
print('Filled in missing dates with NAs')
}
dif2 <- fgh[2:nrow(fgh),'time']-fgh[1:(nrow(fgh)-1),'time']
if(max(dif)>2 | min(dif) < 2){
par(mfrow=c(2,1))
plot(dif, main = 'Time Difference - Raw', ylab = 'sec')
plot(dif2, main = 'Time Difference - Corrected', ylab = 'sec')
print('Timestep has been corrected')
}
sam <- NULL
if(date.na != 0){
snd.na = sum(is.na(fgh$Sonde_Temperature))
anem.na = sum(is.na(fgh$Anemometer))
if(date.na == snd.na & date.na == anem.na){
dates = seq(fgh$time[1], fgh$time[nrow(fgh)], by = 120)
}
}

```

#Replace Nan with NA
```{r}
for(i in 2:ncol(fgh)){
n = which(is.nan(fgh[,i]))
#print(n) ##To check the file
#}
if(length(n) == 0){
print(paste('No NaN in',colnames(fgh)[i]))
next
}else{
fgh[n,i] <- NA
print(paste('NaN replaced with NAs in',colnames(fgh)[i]))
}
}


str(fgh)
```
fgh<-write.csv(fgh,"A_FHTRuls.csv", row.names = F, quote = F)
