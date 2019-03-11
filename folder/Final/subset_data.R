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
# FERGUSON
#=============================================================================
# Load data:
dt.reddit.orlando <- readRDS("~/R/NDA/Team/dt.reddit.orlando.RDS")

# 15 day timeftame b ~ before, a ~ after, f ~ full
dt.r.orlando.15d.b <- f.subset.time(dt.reddit.orlando, "2016-06-04","2016-06-11")
dt.r.orlando.15d.a <- f.subset.time(dt.reddit.orlando, "2016-06-12","2016-06-19")
dt.r.orlando.15d.f <- f.subset.time(dt.reddit.orlando, "2016-06-04","2016-06-19")

saveRDS(dt.r.orlando.15d.b, file = "dt.r.orlando.15d.b.RDS")
saveRDS(dt.r.orlando.15d.a, file = "dt.r.orlando.15d.a.RDS")
saveRDS(dt.r.orlando.15d.f, file = "dt.r.orlando.15d.f.RDS")

# 10 day timeftame
dt.r.orlando.10d.b <- f.subset.time(dt.reddit.orlando, "2016-06-06","2016-06-11")
dt.r.orlando.10d.a <- f.subset.time(dt.reddit.orlando, "2016-06-12","2016-06-17")
dt.r.orlando.10d.f <- f.subset.time(dt.reddit.orlando, "2016-06-06","2016-06-17")

saveRDS(dt.r.orlando.10d.b, file = "dt.r.orlando.10d.b.RDS")
saveRDS(dt.r.orlando.10d.a, file = "dt.r.orlando.10d.a.RDS")
saveRDS(dt.r.orlando.10d.f, file = "dt.r.orlando.10d.f.RDS")

# 5 day timeftame
dt.r.orlando.5d.b <- f.subset.time(dt.reddit.orlando, "2016-06-08","2016-06-11")
dt.r.orlando.5d.a <- f.subset.time(dt.reddit.orlando, "2016-06-12","2016-06-14")
dt.r.orlando.5d.f <- f.subset.time(dt.reddit.orlando, "2016-06-08","2016-06-14")

saveRDS(dt.r.orlando.5d.b, file = "dt.r.orlando.5d.b.RDS")
saveRDS(dt.r.orlando.5d.a, file = "dt.r.orlando.5d.a.RDS")
saveRDS(dt.r.orlando.5d.f, file = "dt.r.orlando.5d.f.RDS")

