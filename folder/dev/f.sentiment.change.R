# =============================================================================
#  FUNCTION: Calculate sentiment change for dt.before & dt.after
# =============================================================================  

f.sentiment.change <- function (dt.before, dt.after) {

  # Aggregate and merge:
  # Calculate sums for sentiment by source|target
  dt.before.aggregate <-  dt.before[ , base::sum(Compound_Sentiment),
                                       by = sourcetarget]

  dt.after.aggregate <-  dt.after[ , base::sum(Compound_Sentiment),
                                     by = sourcetarget]
  
    
  # Merge with the original subset to get aggregate sentiment by pair
  dt.before.merge <- merge(dt.before,
                           dt.before.aggregate, 
                           by = "sourcetarget", 
                           all.x = TRUE)
  
  dt.after.merge <- merge(dt.after,
                           dt.after.aggregate, 
                           by = "sourcetarget", 
                           all.x = TRUE)
  
  
  # Creates a new attribute "sent" which takes the value of compound sentiment
  # after the event, matches with the unique sourcetarget combination and adds
  # them to the dataset before the event.
  
  dt.before.merge$sent <- 
    dt.after.merge$Compound_Sentiment[match(dt.before.merge$sourcetarget, 
                                            dt.after.merge$sourcetarget)]
  
  # Omit all rows with NA in this column
  dt.before.transformed <- na.omit(dt.before.merge, cols = "sent")
  
  # Check links than were positive before the event and became negative after
  dt.negative <- subset(dt.before.transformed, V1 >= 0 & sent < 0)[1:10]
  
  # Fix bug of a repeating edge. Rename the columns.
  dt.change.sentiment <- dt.negative[!2, c(2,3,14,15)]
  names(dt.change.sentiment) <- c("Source",
                                  "Target",
                                  "Before_Sentiment",
                                  "After_Sentiment")
  return(dt.change.sentiment)
}

# # Example:
# dt.orlando.change <- f.sentiment.change(dt.r.ferguson.10d.b, dt.r.ferguson.10d.a)
  