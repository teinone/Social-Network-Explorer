#=============================================================================
#TIME SUBSETTING each event for timeframes
#=============================================================================


# Clear namespace:
f.subset.time <- NULL


#This the function to subset an event dataset into different timeframes

f.subset.time <- function(dt.x, startdate, enddate) {
  
  #Ensure that startdate and enddate are dateTime objects
  startdate <- as.Date(startdate, format = "%Y-%m-%d")
  eddate    <- as.Date(enddate,   format = "%Y-%m-%d")
  
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
dt.r.trump.15d.b <- f.subset.time(dt.reddit.trump, "2016-11-23","2016-12-31")
dt.r.trump.15d.f <- f.subset.time(dt.reddit.trump, "2016-11-23","2016-12-08")
dt.r.trump.15d.a <- f.subset.time(dt.reddit.trump, "2016-12-01","2016-12-08")

# 10 day timeftame
dt.r.trump.10d.b <- f.subset.time(dt.reddit.trump, "2016-11-25","2016-12-31")
dt.r.trump.10d.f <- f.subset.time(dt.reddit.trump, "2016-11-25","2016-12-06")
dt.r.trump.10d.a <- f.subset.time(dt.reddit.trump, "2016-12-01","2016-12-06")

# 5 day timeftame
dt.r.trump.5d.b <- f.subset.time(dt.reddit.trump,  "2016-11-28","2016-12-31")
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

