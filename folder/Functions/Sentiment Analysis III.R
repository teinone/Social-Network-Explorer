load("dt.reddit.events.RData")
# ---------------------------------------------------------------------------------
# Plot scatter plot for source_subreddit sentiments
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# Plot scatter plot for orlando
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.orlando <- graph.data.frame(dt.reddit.orlando[, 
                                                                    list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.orlando <- set_vertex_attr(g.r.sentiment.scatter.orlando, "positive_outbound", 
                                                 value = as.numeric(dt.reddit.orlando$Positive_Sentiment[match(V(g.r.sentiment.scatter.orlando)$name, 
                                                                                                               dt.reddit.orlando$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.orlando <- set_vertex_attr(g.r.sentiment.scatter.orlando, "negative_outbound", 
                                                 value = as.numeric(dt.reddit.orlando$Negative_Sentiment[match(V(g.r.sentiment.scatter.orlando)$name, 
                                                                                                               dt.reddit.orlando$SOURCE_SUBREDDIT)]))
V(g.r.sentiment.scatter.orlando)$out_degree <- degree(g.r.sentiment.scatter.orlando, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.orlando <- data.frame(Vertex = as.vector(V(g.r.sentiment.scatter.orlando)), 
                                           name = V(g.r.sentiment.scatter.orlando)$name, 
                                           positive_outbound = V(g.r.sentiment.scatter.orlando)$positive_outbound, 
                                           negative_outbound = V(g.r.sentiment.scatter.orlando)$negative_outbound, 
                                           out_degree =  V(g.r.sentiment.scatter.orlando)$out_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.orlando <- as.data.table(df.g.r.sentiment.out.orlando)
# Filter out the NA values
dt.g.r.sentiment.filter.out.orlando <- dt.g.r.sentiment.out.orlando[!positive_outbound == "NA"]
dt.g.r.sentiment.filter.out.orlando <- dt.g.r.sentiment.filter.out.orlando[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for ferguson
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.ferguson <- graph.data.frame(dt.reddit.ferguson[, 
                                                                    list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.ferguson <- set_vertex_attr(g.r.sentiment.scatter.ferguson, "positive_outbound", 
                                                 value = as.numeric(dt.reddit.ferguson$Positive_Sentiment[match(V(g.r.sentiment.scatter.ferguson)$name, 
                                                                                                               dt.reddit.ferguson$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.ferguson <- set_vertex_attr(g.r.sentiment.scatter.ferguson, "negative_outbound", 
                                                 value = as.numeric(dt.reddit.ferguson$Negative_Sentiment[match(V(g.r.sentiment.scatter.ferguson)$name, 
                                                                                                               dt.reddit.ferguson$SOURCE_SUBREDDIT)]))
V(g.r.sentiment.scatter.ferguson)$out_degree <- degree(g.r.sentiment.scatter.ferguson, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.ferguson <- data.frame(Vertex = as.vector(V(g.r.sentiment.scatter.ferguson)), 
                                           name = V(g.r.sentiment.scatter.ferguson)$name, 
                                           positive_outbound = V(g.r.sentiment.scatter.ferguson)$positive_outbound, 
                                           negative_outbound = V(g.r.sentiment.scatter.ferguson)$negative_outbound, 
                                           out_degree =  V(g.r.sentiment.scatter.ferguson)$out_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.ferguson <- as.data.table(df.g.r.sentiment.out.ferguson)
# Filter out the NA values
dt.g.r.sentiment.filter.out.ferguson <- dt.g.r.sentiment.out.ferguson[!positive_outbound == "NA"]
dt.g.r.sentiment.filter.out.ferguson <- dt.g.r.sentiment.filter.out.ferguson[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for obama
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.obama <- graph.data.frame(dt.reddit.obama[, 
                                                                    list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.obama <- set_vertex_attr(g.r.sentiment.scatter.obama, "positive_outbound", 
                                                 value = as.numeric(dt.reddit.obama$Positive_Sentiment[match(V(g.r.sentiment.scatter.obama)$name, 
                                                                                                               dt.reddit.obama$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.obama <- set_vertex_attr(g.r.sentiment.scatter.obama, "negative_outbound", 
                                                 value = as.numeric(dt.reddit.obama$Negative_Sentiment[match(V(g.r.sentiment.scatter.obama)$name, 
                                                                                                               dt.reddit.obama$SOURCE_SUBREDDIT)]))
V(g.r.sentiment.scatter.obama)$out_degree <- degree(g.r.sentiment.scatter.obama, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.obama <- data.frame(Vertex = as.vector(V(g.r.sentiment.scatter.obama)), 
                                           name = V(g.r.sentiment.scatter.obama)$name, 
                                           positive_outbound = V(g.r.sentiment.scatter.obama)$positive_outbound, 
                                           negative_outbound = V(g.r.sentiment.scatter.obama)$negative_outbound, 
                                           out_degree =  V(g.r.sentiment.scatter.obama)$out_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.obama <- as.data.table(df.g.r.sentiment.out.obama)
# Filter out the NA values
dt.g.r.sentiment.filter.out.obama <- dt.g.r.sentiment.out.obama[!positive_outbound == "NA"]
dt.g.r.sentiment.filter.out.obama <- dt.g.r.sentiment.filter.out.obama[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for trump
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.trump <- graph.data.frame(dt.reddit.trump[, 
                                                                list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.trump <- set_vertex_attr(g.r.sentiment.scatter.trump, "positive_outbound", 
                                               value = as.numeric(dt.reddit.trump$Positive_Sentiment[match(V(g.r.sentiment.scatter.trump)$name, 
                                                                                                           dt.reddit.trump$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.trump <- set_vertex_attr(g.r.sentiment.scatter.trump, "negative_outbound", 
                                               value = as.numeric(dt.reddit.trump$Negative_Sentiment[match(V(g.r.sentiment.scatter.trump)$name, 
                                                                                                           dt.reddit.trump$SOURCE_SUBREDDIT)]))
V(g.r.sentiment.scatter.trump)$out_degree <- degree(g.r.sentiment.scatter.trump, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.trump <- data.frame(Vertex = as.vector(V(g.r.sentiment.scatter.trump)), 
                                         name = V(g.r.sentiment.scatter.trump)$name, 
                                         positive_outbound = V(g.r.sentiment.scatter.trump)$positive_outbound, 
                                         negative_outbound = V(g.r.sentiment.scatter.trump)$negative_outbound, 
                                         out_degree =  V(g.r.sentiment.scatter.trump)$out_degree)
# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.trump <- as.data.table(df.g.r.sentiment.out.trump)
# Filter out the NA values
dt.g.r.sentiment.filter.out.trump <- dt.g.r.sentiment.out.trump[!positive_outbound == "NA"]
dt.g.r.sentiment.filter.out.trump <- dt.g.r.sentiment.filter.out.trump[!negative_outbound == "NA"]

