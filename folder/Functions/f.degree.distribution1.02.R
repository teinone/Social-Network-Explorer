#==============================================================================
# FUNCTION: returns the degree distribution list for a dt.
#==============================================================================
f.degree.distribution <- function(dt.x) {
  
  # Select unique Source values:
  dt.unique <- dt.x[, list(unique(SOURCE_SUBREDDIT))]
  
  # Change column name to Source
  setnames(dt.unique, c("V1"), c("SOURCE_SUBREDDIT"))
  
  # Create a new dt with total sentiment column aggregated by Source
  dt.total.sentiment <- 
    setDT(dt.x)[, .(total_sentiment = sum(Compound_Sentiment)), 
                  by = SOURCE_SUBREDDIT]
  
  # Merge by Source 
  dt.sentiment <- merge(dt.x, 
                        dt.total.sentiment, 
                        by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.network <- graph.data.frame(dt.x, directed = TRUE)
  
  # Add a sentiment attribute by matching source name to the original table
  V(g.network)$ag_sentiment <- 
    as.numeric(dt.sentiment$total_sentiment[match(V(g.network)$name, 
                                                  dt.x$SOURCE_SUBREDDIT)])
  
  # Take degree distribution: non-cumulative, in and out degree
  l.ddistribution <- degree(g.network, mode = "all")

  return(l.ddistribution)
}

#Example code:
l.ddistr.ferguson <- f.degree.distribution(dt.reddit.ferguson)
qplot(l.ddistr.ferguson)
