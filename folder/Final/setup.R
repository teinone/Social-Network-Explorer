#==============================================================================
# MASTER DATA FILE: dt.reddit.master
#==============================================================================
# Creates the master dataset to be used for subsetting.

#Required libraries
library(data.table)     # Run once per session
library(ggplot2)        # Run once per session
library(stringr)        # Run once per session
library(igraph)         # Run once per session
library(plyr)           # Run once per session

# Load the complete subreddit hyperlink network data.
load("reddit_complete.RData")

# The PROPERTIES is a column with string containing >100 properties. We extract 
# the relevant properties and create columns for Readability, Positive/Negative
# and Compound Sentiments
dt.reddit.prop18 <- 
  dt.reddit.both[, Readability := 
                   str_split_fixed(dt.reddit.both$PROPERTIES, ",", 86)
                 [, 18]]

dt.reddit.prop19 <-
  dt.reddit.prop18[, Positive_Sentiment := 
                     str_split_fixed(dt.reddit.both$PROPERTIES, ",", 86)
                   [, 19]]

dt.reddit.prop20 <- 
  dt.reddit.prop19[, Negative_Sentiment := 
                     str_split_fixed(dt.reddit.both$PROPERTIES, ",", 86)
                   [, 20]]

dt.reddit.prop21 <- 
  dt.reddit.prop20[, Compound_Sentiment := 
                     str_split_fixed(dt.reddit.both$PROPERTIES, ",", 86)
                   [, 21]]

# The sentiment values are type Char, format as numeric.
dt.reddit.prop21$Positive_Sentiment <- as.numeric(dt.reddit.prop21$Positive_Sentiment)
dt.reddit.prop21$Negative_Sentiment <- as.numeric(dt.reddit.prop21$Negative_Sentiment)
dt.reddit.prop21$Compound_Sentiment <- as.numeric(dt.reddit.prop21$Compound_Sentiment)


# Add columns of "date" and "time" to the data table, splited from TIMESTAMP
# Note: only use this column to filter the event period. When doing other analysis,
# please go back to the TIMESTAMP column.
dt.reddit.subset <- 
  dt.reddit.prop21[, date := str_split_fixed(dt.reddit.both$TIMESTAMP, " ", 2)[,1]]

dt.reddit.subset.time <- 
  dt.reddit.prop21[, time := str_split_fixed(dt.reddit.both$TIMESTAMP, " ", 2)[,2]]

# change the date and time to a dateTime format
dt.reddit.subset.time$date <- 
  as.Date(dt.reddit.subset.time$date, format = "%Y-%m-%d")

# Not used in the application: set timestamp to a dateTime format
# dt.reddit.subset.time$time <- as.Date(dt.reddit.subset.time$time, format = "%H:%M:%S")

# remove the column of "PROPERTIES", which is unnecessary.
dt.reddit.subset.time[,6] <- NULL
dt.reddit.master <- dt.reddit.subset.time

# dt.reddit.master will be the raw data with specific properties and date/time.
# This set will be subsetted for specific events and timeframes of analysis.

# Write the master dataset into an RDS file.
saveRDS(dt.reddit.master, file = "dt.reddit.master.RDS")

#==============================================================================
# HOUSEKEEPING: clearing unnecessary data.tables from memory
#==============================================================================
rm(dt.reddit.both,
   dt.reddit.prop18,
   dt.reddit.prop19,
   dt.reddit.prop20,
   dt.reddit.prop21,
   dt.reddit.subset,
   dt.reddit.subset.time)

#==============================================================================
# SUBSETTING for events from dt.reddit.master
#==============================================================================
# The event subsets are all for two weeks based on the timeline file on Google Drive

# Ferguson Unrest==============================================================
dt.reddit.ferguson <- 
  subset(dt.reddit.master,
         date > "2014-08-02" & date < "2014-08-16")[order(TIMESTAMP)]

# Save and drop table
saveRDS(dt.reddit.ferguson, file = "dt.reddit.ferguson.RDS")
rm(dt.reddit.ferguson)

# Obama Visiting Cuba==========================================================
dt.reddit.obama <- 
  subset(dt.reddit.master, 
         date > "2016-03-17" & date < "2016-03-31")[order(TIMESTAMP)]

# Save and drop table
saveRDS(dt.reddit.obama, file = "dt.reddit.obama.RDS")
rm(dt.reddit.obama)

# Orlando Shooting=============================================================
dt.reddit.orlando <- 
  subset(dt.reddit.master, 
         date > "2016-06-05" & date < "2016-06-19")[order(TIMESTAMP)]

# Save and drop table
saveRDS(dt.reddit.orlando, file = "dt.reddit.orlando.RDS")
rm(dt.reddit.orlando)


# Trump named TIME's Person of the Year========================================
dt.reddit.trump <- 
  subset(dt.reddit.master, 
         date > "2016-11-24" & date < "2016-12-08")[order(TIMESTAMP)]

saveRDS(dt.reddit.trump, file = "dt.reddit.trump.RDS")
rm(dt.reddit.trump)

#==============================================================================
# Drop master
rm(dt.reddit.master)

# Now we have a master data set with the necessary variables, and subsets for 
# each event.



#=============================================================================
#FUNCTION: TIME SUBSETTING each event for timeframes
#=============================================================================


# Clear namespace:
f.subset.time <- NULL


#This the function to subset an event dataset into different timeframes

f.subset.time <- function(dt.x, startdate, enddate) {
  
  #Ensure that startdate and enddate are dateTime objects
  startdate <- as.Date(startdate, format = "%Y-%m-%d")
  enddate   <- as.Date(enddate,   format = "%Y-%m-%d")
  
  #Subset dt with start and and end date, order by TIMESTAMP
  dt.subset.time <- 
    subset(dt.x, 
           date > toString(startdate) & 
             date < toString(enddate)
    )[order(TIMESTAMP)]  
  #Return subsetted dt.
  return(dt.subset.time)
}
#=============================================================================

# Example use: 
# dt.reddit.obama <- readRDS("~/R/NDA/Team/dt.reddit.obama.RDS")
# dt.obama.3d <- f.subset.time(dt.reddit.obama, "2016-03-20", "2016-03-22")
#=============================================================================

# SOURCING:
# Load a subsetting script f.subset.time.R from an external script file.
# This script takes the dt., start date and end date and returns a subset dt.
# Call using f.subset.time(dt.x, YYYY-MM-DD, YYYY-MM-DD)
# dt.something <- f.subset.time(...) 

#source('~/R/NDA/Team/f.subset.time.R')
#source('~/functions/f.subset.time.R')

#=============================================================================
# FERGUSON 2014-08-09
#=============================================================================

# 15 day timeftame b ~ before, a ~ after, f ~ full
dt.r.ferguson.15d.b <- f.subset.time(dt.reddit.ferguson, "2014-08-01","2014-08-08")
dt.r.ferguson.15d.f <- f.subset.time(dt.reddit.ferguson, "2014-08-01","2014-08-16")
dt.r.ferguson.15d.a <- f.subset.time(dt.reddit.ferguson, "2014-08-09","2014-08-16")


# 10 day timeftame
dt.r.ferguson.10d.b <- f.subset.time(dt.reddit.ferguson, "2014-08-03","2014-08-08")
dt.r.ferguson.10d.f <- f.subset.time(dt.reddit.ferguson, "2014-08-03","2014-08-14")
dt.r.ferguson.10d.a <- f.subset.time(dt.reddit.ferguson, "2014-08-09","2014-08-14")


# 5 day timeftame
dt.r.ferguson.5d.b <- f.subset.time(dt.reddit.ferguson,  "2014-08-06","2014-08-08")
dt.r.ferguson.5d.f <- f.subset.time(dt.reddit.ferguson,  "2014-08-06","2014-08-11")
dt.r.ferguson.5d.a <- f.subset.time(dt.reddit.ferguson,  "2014-08-09","2014-08-11")

#=============================================================================
# OBAMA 2016-03-24
#=============================================================================
# 15 day timeftame b ~ before, a ~ after, f ~ full
dt.r.obama.15d.b <- f.subset.time(dt.reddit.obama, "2016-03-16","2016-03-23")
dt.r.obama.15d.f <- f.subset.time(dt.reddit.obama, "2016-03-16","2016-03-31")
dt.r.obama.15d.a <- f.subset.time(dt.reddit.obama, "2016-03-24","2016-03-31")

# 10 day timeftame
dt.r.obama.10d.b <- f.subset.time(dt.reddit.obama, "2016-03-18","2016-03-23")
dt.r.obama.10d.f <- f.subset.time(dt.reddit.obama, "2016-03-18","2016-03-29")
dt.r.obama.10d.a <- f.subset.time(dt.reddit.obama, "2016-03-24","2016-03-29")

# 5 day timeftame
dt.r.obama.5d.b <- f.subset.time(dt.reddit.obama,  "2016-03-21","2016-03-23")
dt.r.obama.5d.f <- f.subset.time(dt.reddit.obama,  "2016-03-21","2016-03-26")
dt.r.obama.5d.a <- f.subset.time(dt.reddit.obama,  "2016-03-24","2016-03-26")


#=============================================================================
# ORLANDO 2016-06-12
#=============================================================================
# 15 day timeftame b ~ before, a ~ after, f ~ full
dt.r.orlando.15d.b <- f.subset.time(dt.reddit.orlando, "2016-06-04","2016-06-11")
dt.r.orlando.15d.f <- f.subset.time(dt.reddit.orlando, "2016-06-04","2016-06-19")
dt.r.orlando.15d.a <- f.subset.time(dt.reddit.orlando, "2016-06-12","2016-06-19")

# 10 day timeftame
dt.r.orlando.10d.b <- f.subset.time(dt.reddit.orlando, "2016-06-06","2016-06-11")
dt.r.orlando.10d.f <- f.subset.time(dt.reddit.orlando, "2016-06-06","2016-06-17")
dt.r.orlando.10d.a <- f.subset.time(dt.reddit.orlando, "2016-06-12","2016-06-17")

# 5 day timeftame
dt.r.orlando.5d.b <- f.subset.time(dt.reddit.orlando,  "2016-06-08","2016-06-11")
dt.r.orlando.5d.f <- f.subset.time(dt.reddit.orlando,  "2016-06-08","2016-06-14")
dt.r.orlando.5d.a <- f.subset.time(dt.reddit.orlando,  "2016-06-12","2016-06-14")

#=============================================================================
# TRUMP 2016-12-01
#=============================================================================

# 15 day timeftame b ~ before, a ~ after, f ~ full
dt.r.trump.15d.b <- f.subset.time(dt.reddit.trump, "2016-11-23","2016-11-30")
dt.r.trump.15d.f <- f.subset.time(dt.reddit.trump, "2016-11-23","2016-12-08")
dt.r.trump.15d.a <- f.subset.time(dt.reddit.trump, "2016-12-01","2016-12-08")

# 10 day timeftame
dt.r.trump.10d.b <- f.subset.time(dt.reddit.trump, "2016-11-25","2016-11-30")
dt.r.trump.10d.f <- f.subset.time(dt.reddit.trump, "2016-11-25","2016-12-06")
dt.r.trump.10d.a <- f.subset.time(dt.reddit.trump, "2016-12-01","2016-12-06")

# 5 day timeftame
dt.r.trump.5d.b <- f.subset.time(dt.reddit.trump,  "2016-11-28","2016-11-30")
dt.r.trump.5d.f <- f.subset.time(dt.reddit.trump,  "2016-11-28","2016-12-03")
dt.r.trump.5d.a <- f.subset.time(dt.reddit.trump,  "2016-12-01","2016-12-03")

#=============================================================================
# SAVE DATA.TABLES INTO dt.reddit.time.RData
#=============================================================================

save(
  #Ferguson tables:
  dt.r.ferguson.15d.b,
  dt.r.ferguson.15d.a,
  dt.r.ferguson.15d.f,
  dt.r.ferguson.10d.b, 
  dt.r.ferguson.10d.a, 
  dt.r.ferguson.10d.f,
  dt.r.ferguson.5d.b, 
  dt.r.ferguson.5d.a, 
  dt.r.ferguson.5d.f,
  #Obama tables:
  dt.r.obama.15d.b,
  dt.r.obama.15d.a,
  dt.r.obama.15d.f,
  dt.r.obama.10d.b, 
  dt.r.obama.10d.a, 
  dt.r.obama.10d.f,
  dt.r.obama.5d.b, 
  dt.r.obama.5d.a, 
  dt.r.obama.5d.f,
  #Orlando tables:
  dt.r.orlando.15d.b,
  dt.r.orlando.15d.a,
  dt.r.orlando.15d.f,
  dt.r.orlando.10d.b, 
  dt.r.orlando.10d.a, 
  dt.r.orlando.10d.f,
  dt.r.orlando.5d.b, 
  dt.r.orlando.5d.a, 
  dt.r.orlando.5d.f,
  #Trump tables:
  dt.r.trump.15d.b,
  dt.r.trump.15d.a,
  dt.r.trump.15d.f,
  dt.r.trump.10d.b, 
  dt.r.trump.10d.a, 
  dt.r.trump.10d.f,
  dt.r.trump.5d.b, 
  dt.r.trump.5d.a, 
  dt.r.trump.5d.f,
  file = "dt.reddit.time.RData")


rm(
  #Ferguson tables:
  dt.r.ferguson.15d.b,
  dt.r.ferguson.15d.a,
  dt.r.ferguson.15d.f,
  dt.r.ferguson.10d.b, 
  dt.r.ferguson.10d.a, 
  dt.r.ferguson.10d.f,
  dt.r.ferguson.5d.b, 
  dt.r.ferguson.5d.a, 
  dt.r.ferguson.5d.f,
  #Obama tables:
  dt.r.obama.15d.b,
  dt.r.obama.15d.a,
  dt.r.obama.15d.f,
  dt.r.obama.10d.b, 
  dt.r.obama.10d.a, 
  dt.r.obama.10d.f,
  dt.r.obama.5d.b, 
  dt.r.obama.5d.a, 
  dt.r.obama.5d.f,
  #Orlando tables:
  dt.r.orlando.15d.b,
  dt.r.orlando.15d.a,
  dt.r.orlando.15d.f,
  dt.r.orlando.10d.b, 
  dt.r.orlando.10d.a, 
  dt.r.orlando.10d.f,
  dt.r.orlando.5d.b, 
  dt.r.orlando.5d.a, 
  dt.r.orlando.5d.f,
  #Trump tables:
  dt.r.trump.15d.b,
  dt.r.trump.15d.a,
  dt.r.trump.15d.f,
  dt.r.trump.10d.b, 
  dt.r.trump.10d.a, 
  dt.r.trump.10d.f,
  dt.r.trump.5d.b, 
  dt.r.trump.5d.a, 
  dt.r.trump.5d.f)

# Sweet, now we have all the raw data we need for the application.


#==============================================================================
# The next step runs a number of function calls and then runs these functions
# all the above new data.tables
# NOTE: Running these functions can be heavy. Be advised.
#==============================================================================

#Fetch Functions
source("f.network.graph.R")
source("f.sentiment.change.R")
source("f.link.prediction.R")
source("f.stats.R")
source("f.plot.combo.R")
source("f.plot.hist.R")
source("f.degree.distribution.R")


# Create network dataset:
# NOTE: each of these calls creates tens of objects.
# The whole sequence can take up to two minutes to run.

# source("load.trump.R")
# source("load.obama.R")
# source("load.orlando.R")
# source("load.ferguson.R")
# source("load.scatter.R")

# All these objects are then stored into data.RData

