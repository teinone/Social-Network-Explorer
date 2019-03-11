library(data.table)
library(ggplot2)
library(igraph)
load("reddit.masters.RData")

# Aggrated sentiment : obama
dt.obama.sentiment <- dt.reddit.obama[,list(unique(SOURCE_SUBREDDIT))]

setnames(dt.obama.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
total_sentiment_obama <- setDT(dt.reddit.obama)[, .(total_sentiment_obama = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]

# Update Event 
dt.obama.sentiment <- merge(dt.reddit.obama, total_sentiment_obama, by = "SOURCE_SUBREDDIT")

#Plot Network degree distribution
g.obama <- graph.data.frame(dt.reddit.obama, directed= TRUE)
V(g.obama)$ag_sentiment <- as.numeric(dt.obama.sentiment$total_sentiment_obama[match(V(g.obama)$name,dt.reddit.obama$SOURCE_SUBREDDIT)])

degree.dist.obama <- degree_distribution(g.obama, cumulative=T, mode="all")

plot( x=0:max(degree(g.obama)), y=1-degree.dist.obama, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")


# Agregated sentiment and most connected component
cl.obama <- clusters(g.obama)
g.biggest.component.obama <- induced.subgraph(g.obama, which(cl.obama$membership == which.max(cl.obama$csize)))

# Freq for each sentiment
dt.component.sentiment.obama <- as.data.table(table((V(g.biggest.component.obama)$ag_sentiment)))


# Graph: Distribution of emotion for most active users

# This function allows me to show the negative values in log way 
# Create custom log-style y axis transformer (0,1,3,10,...)
custom_log_y_trans <- function()
  trans_new("custom_log_y",
            transform = function (x) ( log(abs(x)+1) ),
            inverse = function (y) ( exp(abs(y))-1 ),
            domain = c(0,Inf))

# Custom log y breaker (0,1,3,10,...)

ggplot(dt.component.sentiment.obama, aes(x=N, y=V1)) + 
  geom_point(size= ifelse(dt.component.sentiment.obama$V1 < 0.5 & dt.component.sentiment.obama$V1 > -0.5, 0, 
                          ifelse(dt.component.sentiment.obama$V1 < 1.5 & dt.component.sentiment.obama$V1 > -1.5, 1,
                                 ifelse(dt.component.sentiment.obama$V1 < 2.5 & dt.component.sentiment.obama$V1 > -2.5, 2, 3
                                 ))), 
             col = ifelse(dt.component.sentiment.obama$V1 < 0, "deepskyblue", "tomato")
  ) + 
  geom_line() + 
  geom_smooth(se=F) + 
  xlab("Frequency") + 
  ylab("Sentiment") +
  scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.obama$N), max(dt.component.sentiment.obama$N), by = 30), 1)) +
  scale_y_discrete(breaks = c(min(dt.component.sentiment.obama$V1), 0, 5, max(dt.component.sentiment.obama$V1)))














