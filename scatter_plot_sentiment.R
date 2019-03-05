# Load previously saved data.
load("reddit_raw(sentiment+readability).RData")

#Data frame for graph
g.reddit.raw <- graph.data.frame(dt.reddit.raw[, list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)],
                             directed = TRUE)
summary(g.reddit.raw)

# ---------------------------------------------------------------------------------
# Plot scatter plot for source_subreddits
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subreddits)
g.reddit.raw <- set_vertex_attr(g.reddit.raw, "positive", value = as.numeric(
  dt.reddit.raw$Positive_Sentiment[match(V(g.reddit.raw)$name, dt.reddit.raw$TARGET_SUBREDDIT)]))
g.reddit.raw <- set_vertex_attr(g.reddit.raw, "negative", value = as.numeric(
  dt.reddit.raw$Negative_Sentiment[match(V(g.reddit.raw)$name, dt.reddit.raw$TARGET_SUBREDDIT)]))
V(g.reddit.raw)$out_degree <- degree(g.reddit.raw, mode = "out")

# Convert the igraph to a dataframe
df.g.reddit.out <- data.frame(Vertex = as.vector(V(g.reddit.raw)), name = V(g.reddit.raw)$name, 
                              positive = V(g.reddit.raw)$positive, negative = V(g.reddit.raw)$negative, 
                              out_degree =  V(g.reddit.raw)$out_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.reddit.out <- as.data.table(df.g.reddit.out)
# Filter out the NA values
dt.g.reddit.filter.out <- dt.g.reddit.out[!positive == "NA"]
dt.g.reddit.filter.out <- dt.g.reddit.filter.out[!negative == "NA"]
# Create scatter plot
ggplot(dt.g.reddit.filter.out, aes(x = positive, y = negative)) + geom_point(
  size = dt.g.reddit.filter.out$out_degree/800, 
  col = ifelse(dt.g.reddit.filter.out$name == "subredditdrama",'red','blue'))
# ---------------------------------------------------------------------------------
# Inbound
# Set the necessary attributes: post sentiment (negative & positive), and in_degree
# (in_degree: number of links from other target subreddits)
g.reddit.raw <- set_vertex_attr(g.reddit.raw, "positive", value = as.numeric(
  dt.reddit.raw$Positive_Sentiment[match(V(g.reddit.raw)$name, dt.reddit.raw$TARGET_SUBREDDIT)]))
g.reddit.raw <- set_vertex_attr(g.reddit.raw, "negative", value = as.numeric(
  dt.reddit.raw$Negative_Sentiment[match(V(g.reddit.raw)$name, dt.reddit.raw$TARGET_SUBREDDIT)]))
V(g.reddit.raw)$in_degree <- degree(g.reddit.raw, mode = "in")

# Convert the igraph to a dataframe
df.g.reddit.in <- data.frame(Vertex = as.vector(V(g.reddit.raw)), name = V(g.reddit.raw)$name, 
                              positive = V(g.reddit.raw)$positive, negative = V(g.reddit.raw)$negative, 
                              in_degree =  V(g.reddit.raw)$in_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.reddit.in <- as.data.table(df.g.reddit.in)
# Filter in the NA values
dt.g.reddit.filter.in <- dt.g.reddit.in[!positive == "NA"]
dt.g.reddit.filter.in <- dt.g.reddit.filter.in[!negative == "NA"]
# Create scatter plot
ggplot(dt.g.reddit.filter.in, aes(x = positive, y = negative)) + geom_point(
  size = dt.g.reddit.filter.in$in_degree/800, 
  col = ifelse(dt.g.reddit.filter.in$name == "askreddit",'red','blue'))