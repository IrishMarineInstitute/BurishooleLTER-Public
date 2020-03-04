# Sean Kelly
# Download hi-res met eireann variables; directly access zip files and extact monthly data

setwd("~/Desktop/Temp_WD")

temp <- "~/Desktop/Temp_WD/temp.zip" # CREATE a temporary zip file to store the downloaded met data
download.file("http://cli.met.ie/cli/climate_data/webdata/mindata/mindata_1175_2018_07.zip",temp) # download the file
# in this example we are pulling down July 2018, 1175 is the station ID
data.1 <- read.csv(unz(temp, "data_1min_1175_2018_07.csv")) # unzip and save the 1min csv as "data"
unlink(temp) # this removes the temporary file directory where original file was saved

str(data.1)
# create a good timestamp as the default date is awkward format
library(lubridate)
data.1$timestamp <- with(data.1, ymd_hm(paste(year, month, day, hour, minute, sep= ' ')))
str(data.1)
summary(data.1)
# take columns of interest (e.g. time, relhum, air pressure, shortwave, rain, dry  bulb)
data.1 <- data.1[,c(46,39,41,43,45,23)]
plot(data.1$timestamp,data.1$cbl,type="h",yaxs="i") # good to check nothing strange
summary(data.1)

### UPDATE ###
# from February 2019, a new weblink is required to access 1-minute resolution files
# e.g.
download.file("https://cli.fusio.net/cli/climate_data/webdata/mindata/mindata_1175_2019_02.zip",temp) # this is the correct address as of 4th March 2020
data.2 <- read.csv(unz(temp, "data_1min_1175_2019_02.csv")) 
# this will then assign a 1-minute csv file with the same format as before and can be treated using the same code above from
# line 10 onwards. 

