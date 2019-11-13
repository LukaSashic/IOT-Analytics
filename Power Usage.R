
## Load libraries

library(RMySQL)
library(dplyr)
library(funModeling)
library(caret)      
library(tidyverse)  
library(lubridate)  
library(VIM)        
library(Hmisc)     
library(GGally)    
library(scales)
library(forecast)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,c('yr_2006','yr_2007', 'yr_2008', 'yr_2009', 'yr_2010'))

## Use attribute names to specify specific attributes for download
yr_2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")



# Basic statistics 2006

str(yr_2006)
summary(yr_2006) 
head(yr_2006)
tail(yr_2006)

# Basic statistics 2007

str(yr_2007)
summary(yr_2007) 
head(yr_2007)
tail(yr_2007)

# Basic statistics 2008

str(yr_2008)
summary(yr_2008) 
head(yr_2008)
tail(yr_2008)

# Basic statistics 2009

str(yr_2009)
summary(yr_2009) 
head(yr_2009)
tail(yr_2009)

# Basic statistics 2010

str(yr_2010)
summary(yr_2010) 
head(yr_2010)
tail(yr_2010)

## Combine tables into one dataframe using dplyr
smart <- bind_rows(yr_2007, yr_2008, yr_2009)

str(smart)
summary(smart) 
head(smart)
tail(smart)

# Convert to a tibble
as_tibble(smart)

# Combine Date and Time Features

## Combine Date and Time attribute values in a new attribute column

smart <-cbind(smart,paste(smart$Date,smart$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name

colnames(smart)[11] <-"DateTime"

## Move the DateTime attribute within the dataset

smart <- smart[,c(ncol(smart), 1:(ncol(smart)-1))]
head(smart)

smart$id <- NULL

## Convert DateTime from POSIXlt to POSIXct 
smart$DateTime <- as.POSIXct(smart$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(smart$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(smart)
tz(smart)



## Create "year" attribute with lubridate

smart$year <- year(smart$DateTime)
smart$month <- month(smart$DateTime)
smart$quarter <- quarter(smart$DateTime)
smart$week <- week(smart$DateTime)
smart$day <- day(smart$DateTime)
smart$hour <- hour(smart$DateTime)





# Remove rows with NA's

smart <- na.omit(smart)
sum(is.na(smart))



# Rename sub-emeters

smart<-smart%>% dplyr::rename(Kitchen=Sub_metering_1, Laundry=Sub_metering_2, EWAC=Sub_metering_3) 

# Other_Rooms represents the active energy consumed every minute (in watt hour) in the household by electrical equipment not measured in sub-meterings 1, 2 and 3

smart$Other_Rooms <- ((smart$Global_active_power*1000)/60 - smart$Sub_metering_1 - smart$Sub_metering_2 - smart$Sub_metering_3)

smart$Other_Rooms <- round(smart$Other_Rooms, 0)


# Basic visualization

plot_num(smart)






