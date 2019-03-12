output$degree.distribution.ferguson <- renderPlot({ # start degree.distribution.ferguson
  dt.ferguson.sentiment <- dt.reddit.ferguson[,list(unique(SOURCE_SUBREDDIT))]
  setnames(dt.ferguson.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
  total_sentiment_ferguson <- setDT(dt.reddit.ferguson)[, .(total_sentiment_ferguson = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
  
  # Update Event 
  dt.ferguson.sentiment <- merge(dt.reddit.ferguson, total_sentiment_ferguson, by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.ferguson <- graph.data.frame(dt.reddit.ferguson, directed= TRUE)
  V(g.ferguson)$ag_sentiment <- as.numeric(dt.ferguson.sentiment$total_sentiment_ferguson[match(V(g.ferguson)$name,dt.reddit.ferguson$SOURCE_SUBREDDIT)])
  
  degree.dist.ferguson <- degree_distribution(g.ferguson, cumulative=T, mode="all")
  plot.degree.distribution.ferguson <-
    plot(x=0:max(degree(g.ferguson)), y=1-degree.dist.ferguson, pch=20, cex=1.2, col="orange", 
         xlab="Degree", ylab="Frequency")
  plot.degree.distribution.ferguson
  
}) #end degree.distribution.ferguson

#start of degree.distribution.orlando
output$degree.distribution.orlando <- renderPlot({ # start degree.distribution.orlando
  dt.orlando.sentiment <- dt.reddit.orlando[,list(unique(SOURCE_SUBREDDIT))]
  setnames(dt.orlando.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
  total_sentiment_orlando <- setDT(dt.reddit.orlando)[, .(total_sentiment_orlando = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
  
  # Update Event 
  dt.orlando.sentiment <- merge(dt.reddit.orlando, total_sentiment_orlando, by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.orlando <- graph.data.frame(dt.reddit.orlando, directed= TRUE)
  V(g.orlando)$ag_sentiment <- as.numeric(dt.orlando.sentiment$total_sentiment_orlando[match(V(g.orlando)$name,dt.reddit.orlando$SOURCE_SUBREDDIT)])
  
  degree.dist.orlando <- degree_distribution(g.orlando, cumulative=T, mode="all")
  plot.degree.distribution.orlando <-
    plot(x=0:max(degree(g.orlando)), y=1-degree.dist.orlando, pch=20, cex=1.2, col="orange", 
         xlab="Degree", ylab="Frequency")
  plot.degree.distribution.orlando
  
}) #end degree.distribution.orlando

# start degree.distribution.obama
output$degree.distribution.obama <- renderPlot({ 
  dt.obama.sentiment <- dt.reddit.obama[,list(unique(SOURCE_SUBREDDIT))]
  setnames(dt.obama.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
  total_sentiment_obama <- setDT(dt.reddit.obama)[, .(total_sentiment_obama = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
  
  # Update Event 
  dt.obama.sentiment <- merge(dt.reddit.obama, total_sentiment_obama, by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.obama <- graph.data.frame(dt.reddit.obama, directed= TRUE)
  V(g.obama)$ag_sentiment <- as.numeric(dt.obama.sentiment$total_sentiment_obama[match(V(g.obama)$name,dt.reddit.obama$SOURCE_SUBREDDIT)])
  
  degree.dist.obama <- degree_distribution(g.obama, cumulative=T, mode="all")
  plot.degree.distribution.obama <-
    plot(x=0:max(degree(g.obama)), y=1-degree.dist.obama, pch=20, cex=1.2, col="orange", 
         xlab="Degree", ylab="Frequency")
  plot.degree.distribution.obama
  
}) #end degree.distribution.obama
# start degree.distribution.trump
output$degree.distribution.trump <- renderPlot({ 
  dt.trump.sentiment <- dt.reddit.trump[,list(unique(SOURCE_SUBREDDIT))]
  setnames(dt.trump.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
  total_sentiment_trump <- setDT(dt.reddit.trump)[, .(total_sentiment_trump = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]
  
  # Update Event 
  dt.trump.sentiment <- merge(dt.reddit.trump, total_sentiment_trump, by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.trump <- graph.data.frame(dt.reddit.trump, directed= TRUE)
  V(g.trump)$ag_sentiment <- as.numeric(dt.trump.sentiment$total_sentiment_trump[match(V(g.trump)$name,dt.reddit.trump$SOURCE_SUBREDDIT)])
  
  degree.dist.trump <- degree_distribution(g.trump, cumulative=T, mode="all")
  plot.degree.distribution.trump <-
    plot(x=0:max(degree(g.trump)), y=1-degree.dist.trump, pch=20, cex=1.2, col="orange", 
         xlab="Degree", ylab="Frequency")
  plot.degree.distribution.trump
  
}) #end degree.distribution.trump