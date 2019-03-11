#==============================================================================
# FUNCTION: get the degree distribution for a dt.
#==============================================================================
f.degree.distribution <- function(dt.x) {

  # Select unique Source values:
  dt.unique <- dt.x[, list(unique(SOURCE_SUBREDDIT))]
  
  # Change column name to source
  setnames(dt.unique, c("V1"), c("SOURCE_SUBREDDIT"))
  
  # Create a new dt with total sentiment column aggregated by source
  dt.total.sentiment <- 
    setDT(dt.x)[, .(total_sentiment = sum(Compound_Sentiment)), 
                  by = SOURCE_SUBREDDIT]
  
  # Merge 
  dt.sentiment <- merge(dt.x, 
                        dt.total.sentiment, 
                        by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.network <- graph.data.frame(dt.x, directed= TRUE)
  
  # Add a sentiment attribute by matching source name to the original table
  V(g.network)$ag_sentiment <- 
    as.numeric(dt.sentiment$total_sentiment[match(V(g.network)$name, 
                                                  dt.x$SOURCE_SUBREDDIT)])
  
  #Take degree distribution
  l.ddistribution <- degree_distribution(g.network, cumulative=T, mode="all")
  
  plot.ddistribution <-
    plot( x = 0:max(degree(g.network)),
          y = 1 - l.ddistribution,
          pch = 19, 
          cex = 1.2, 
          col = "orange", 
          xlab = "Degree", 
          ylab = "Cumulative Frequency")
  
  return(plot.ddistribution)
}
#Example code:
f.degree.distribution(dt.reddit.orlando)
