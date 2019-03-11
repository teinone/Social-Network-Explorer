library(data.table)
library(ggplot2)
library(igraph)
load("reddit.masters.RData")

# Aggrated sentiment : orlando
dt.orlando.sentiment <- dt.reddit.orlando[,list(unique(SOURCE_SUBREDDIT))]

setnames(dt.orlando.sentiment, c("V1"), c("SOURCE_SUBREDDIT"))
total_sentiment_orlando <- setDT(dt.reddit.orlando)[, .(total_sentiment_orlando = sum(Compound_Sentiment)), by = SOURCE_SUBREDDIT]

# Update Event 
dt.orlando.sentiment <- merge(dt.reddit.orlando, total_sentiment_orlando, by = "SOURCE_SUBREDDIT")

#Plot Network degree distribution
g.orlando <- graph.data.frame(dt.reddit.orlando, directed= TRUE)
V(g.orlando)$ag_sentiment <- as.numeric(dt.orlando.sentiment$total_sentiment_orlando[match(V(g.orlando)$name,dt.reddit.orlando$SOURCE_SUBREDDIT)])

degree.dist.orlando <- degree_distribution(g.orlando, cumulative=T, mode="all")

plot( x=0:max(degree(g.orlando)), y=1-degree.dist.orlando, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")


# Agregated sentiment and most connected component
cl.orlando <- clusters(g.orlando)
g.biggest.component.orlando <- induced.subgraph(g.orlando, which(cl.orlando$membership == which.max(cl.orlando$csize)))

# Freq for each sentiment
dt.component.sentiment.orlando <- as.data.table(table((V(g.biggest.component.orlando)$ag_sentiment)))


# Graph: Distribution of emotion for most active users

# This function allows me to show the negative values in log way 
# Create custom log-style y axis transformer (0,1,3,10,...)
custom_log_y_trans <- function()
  trans_new("custom_log_y",
            transform = function (x) ( log(abs(x)+1) ),
            inverse = function (y) ( exp(abs(y))-1 ),
            domain = c(0,Inf))

# Custom log y breaker (0,1,3,10,...)

ggplot(dt.component.sentiment.orlando, aes(x=N, y=V1)) + 
  geom_point(size= ifelse(dt.component.sentiment.orlando$V1 < 0.5 & dt.component.sentiment.orlando$V1 > -0.5, 0, 
                          ifelse(dt.component.sentiment.orlando$V1 < 1.5 & dt.component.sentiment.orlando$V1 > -1.5, 1,
                                 ifelse(dt.component.sentiment.orlando$V1 < 2.5 & dt.component.sentiment.orlando$V1 > -2.5, 2, 3
                                 ))), 
             col = ifelse(dt.component.sentiment.orlando$V1 < 0, "deepskyblue", "tomato")
  ) + 
  geom_line() + 
  geom_smooth(se=F) + 
  xlab("Frequency") + 
  ylab("Sentiment") +
  scale_x_continuous(breaks = round(seq(min(dt.component.sentiment.orlando$N), max(dt.component.sentiment.orlando$N), by = 30), 1)) +
  scale_y_discrete(breaks = c(min(dt.component.sentiment.orlando$V1), 0, 5, max(dt.component.sentiment.orlando$V1)))














