
# Load library
load("/Users/Desktop/NDATeam12/reddit_complete.RData")
library(ggplot2)
library(tidyr)
library(reshape2)
library(igraph)
library(dplyr)
library(data.table)
library(network)
library(ggraph)
library(ggpubr)



# Heatmap is essentially a tri-axial graph
# x= subreddits 
# y= time (by day/week/month)
# z= aggr. sentiment 

# Group subreddits by source/target
#dt.source <- dt.reddit.both %>% group_by(SOURCE_SUBREDDIT)
#dt.source <- dt.reddit.both[, n_source := .N, by="SOURCE_SUBREDDIT"]
#dt.source.unique <- dt.reddit.both[, unique_id := unique(dt.reddit.both$SOURCE_SUBREDDIT), by="SOURCE_SUBREDDIT"]
#dt.source.unique <- dt.source[, list(unique(SOURCE_SUBREDDIT), n_source)]
# Mental Notes: just repeat this, unique source_id



-----------------------------------------------------
# Preprocessing the data table: make TIMESTAMP a date, 
#add new var. "year",
# group the data by Source 
invisible(as.Date(dt.reddit.both$TIMESTAMP))
dt.reddit <- dt.reddit.both[, year := format(TIMESTAMP,'%Y')]
# Aggregated Sentiment 
total_sentiment <- setDT(dt.reddit)[, .(total_sentiment = sum(LINK_SENTIMENT)), by = SOURCE_SUBREDDIT]
# Update the original data table 
dt.reddit.unique <- dt.reddit[,list(unique(SOURCE_SUBREDDIT))]
setnames(dt.reddit.unique, c("V1"), c("SOURCE_SUBREDDIT"))
dt.reddit.sentiment <- merge(dt.reddit.unique, total_sentiment, by = "SOURCE_SUBREDDIT")
---------------------------------------
# Network: degree distribution 
g.reddit <- graph.data.frame(dt.reddit, directed= TRUE)

# Assign aggregated sentiment as a vertex attribute: aggregated sentiment  
V(g.reddit)$ag_sentiment <- as.numeric(total_sentiment$total_sentiment[match(V(g.reddit)$name,dt.reddit$SOURCE_SUBREDDIT)])

# Let's plot the degree distribution 
degree.dist <- degree_distribution(g.reddit, cumulative=T, mode="all")
plot( x=0:max(degree(g.reddit)), y=1-degree.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

--------------------------------
# Tree map of the network 
require(ggpubr)
g.june2016 <- induced.subgraph(g.reddit, which(V(g.reddit)$TIMESTAMP >= 2016-06-01))
g.june2016 <- induced.subgraph(g.june2016, which(V(g.reddit)$TIMESTAMP <= 2016-06-20))
cols <- get_palette("Dark2", nrow(dt.reddit)+1)
set.seed(123)
ggraph(g.june2016, 'treemap', weight = "ag_sentiment") + 
  geom_node_tile(aes(fill = label), size = 0.25, color = "white")+
  geom_node_text(
    aes(label = paste(label, ag_sentiment, sep = "\n"),
        size = ag_sentiment), color = "white"
  )+
  scale_fill_manual(values = cols)+
  scale_size(range = c(0, 6) )+
  theme_void()+
  theme(legend.position = "none")
------------------
# Component Analysis of this network
cl <- clusters(g.reddit)
g.biggest.component <- induced.subgraph(g.reddit, which(cl$membership == which.max(cl$csize)))

# Freq for each sentiment
dt.component.sentiment <- as.data.table(table((V(g.biggest.component)$ag_sentiment)))

# Distribution of emotion for most active users
ggplot(dt.component.sentiment, aes(x=N, y=V1))+geom_point()  + geom_line() + geom_smooth(se=F)

-------------------
# Network Matrix Heatmap 
# Assign Edge weight by the number of nodes
E(g.reddit)$weight <- 1/( length(V(g.reddit) ) -1 )

# Make the heatmap axis
#m.reddit <- as_adjacency_matrix(g.reddit, attr="weight", sparse=F)
#colnames(m.reddit) <- V(g.reddit)$ag_sentiment
#rownames(m.reddit) <- V(g.reddit)$year

#Plot
#palf <- colorRampPalette(c("gold", "dark orange")) 
#heatmap(m.reddit[,20:1], Rowv = NA, Colv = NA, col = palf(100), 
        #scale="none", margins=c(10,10) )

-------------------
  
# I found a way to add new varaibles
#FUNCTION: gather(data, key, value, ..., na.rm = FALSE, convert = FALSE)
#data:           data frame
#key:            column name representing new variable
#value:          column name representing variable values
#...:            names of columns to gather (or not gather)
#na.rm:          option to remove observations with missing values (represented by NAs)
#convert:        if TRUE will automatically convert values to logical, integer, numeric, complex or factor as appropriate
  
------------------


