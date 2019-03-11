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
