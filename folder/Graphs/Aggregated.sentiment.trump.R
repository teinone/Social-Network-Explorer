library(data.table)
library(ggplot2)
library(igraph)
load("reddit.masters.RData")

# Aggrated sentiment : trump
dt.trump.sentiment <- dt.reddit.trump[,list(unique(SOURCE_SUBREDDIT))]

setnames(dt.trump.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
total_sentiment_trump <- setDT(dt.reddit.trump)[, .(total_sentiment_trump = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]

# Update Event 
dt.trump.sentiment <- merge(dt.reddit.trump, total_sentiment_trump, by = "SOURCE_SUBREDDIT")

#Plot Network degree distribution
g.trump <- graph.data.frame(dt.reddit.trump, directed= TRUE)
V(g.trump)$ag_sentiment <- as.numeric(dt.trump.sentiment$total_sentiment_trump[match(V(g.trump)$name,dt.reddit.trump$SOURCE_SUBREDDIT)])

degree.dist.trump <- degree_distribution(g.trump, cumulative=T, mode="all")

plot( x=0:max(degree(g.trump)), y=1-degree.dist.trump, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")


# Agregated sentiment and most connected component
cl.trump <- clusters(g.trump)
g.biggest.component.trump <- induced.subgraph(g.trump, which(cl.trump$membership == which.max(cl.trump$csize)))

# Freq for each sentiment
dt.component.sentiment.trump <- as.data.table(table((V(g.biggest.component.trump)$ag_sentiment)))


# Graph: Distribution of emotion for most active users

# This function allows me to show the negative values in log way 
# Create custom log-style y axis transformer (0,1,3,10,...)
custom_log_y_trans <- function()
  trans_new("custom_log_y",
            transform = function (x) ( log(abs(x)+1) ),
            inverse = function (y) ( exp(abs(y))-1 ),
            domain = c(0,Inf))

# Custom log y breaker (0,1,3,10,...)

ggplot(dt.component.sentiment.trump, aes(x=N, y=V1)) + 
  geom_point(size= ifelse(dt.component.sentiment.trump$V1 < 0.5 & dt.component.sentiment.trump$V1 > -0.5, 0, 
                          ifelse(dt.component.sentiment.trump$V1 < 1.5 & dt.component.sentiment.trump$V1 > -1.5, 1,
                                 ifelse(dt.component.sentiment.trump$V1 < 2.5 & dt.component.sentiment.trump$V1 > -2.5, 2, 3
                                 ))), 
             col = ifelse(dt.component.sentiment.trump$V1 < 0, "deepskyblue", "tomato")
  ) + 
  geom_line() + 
  geom_smooth(se=F) + 
  xlab("Frequency") + 
  ylab("Sentiment") +
  scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.trump$N), max(dt.component.sentiment.trump$N), by = 30), 1)) +
  scale_y_discrete(breaks = c(min(dt.component.sentiment.trump$V1), 0, 5, max(dt.component.sentiment.trump$V1)))














