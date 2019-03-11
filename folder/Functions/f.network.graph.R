#==============================================================================
# FUNCTION: create full network graph and set sentiment as edge weight
#==============================================================================
f.network.graph <- function(dt.x) {

# Calculate sums for sentiment by source|target
dt.aggregate <- dt.x[ , base::sum(Compound_Sentiment),
                                  by = sourcetarget]
  
# Merge with the original subset to get aggregate sentiment by pair
dt.merge <- merge(dt.x,
                  dt.aggregate, 
                  by = "sourcetarget", 
                  all.x = TRUE)

#Crete a directed graph from source-target pair
g.event <- 
  graph.data.frame(dt.merge[, list(SOURCE_SUBREDDIT,
                                   TARGET_SUBREDDIT)],
                   directed = TRUE)

#Define aggregated sentiment as weight
E(g.event)$weight <- dt.merge[, V1]

# Get the node connections
V(g.event)$degree <- degree(g.event, mode = "all", loops = TRUE)

return(g.event)
}

# Example use:
g.orlando.full <- f.network.graph(dt.reddit.orlando)

#==============================================================================
# FUNCTION: induce subgraph from g.event
#==============================================================================
f.network.subgraph <- function (g.event) {
  #Create subgraph with nodes with degree >10
  g.subgraph <- induced_subgraph(g.event, V(g.event)$degree > 10)
  return(g.subgraph)
}

# Example use:
g.orlando.subgraph <- f.network.subgraph(g.orlando.full)

par(mai=c(0,0,1,0))
plot(g.orlando.subgraph,
     layout=layout.graphopt, 
     main = g2$name,
     vertex.color = "blue", 
     vertex.size = 4, 
     vertex.label = " ", 
     edge.arrow.size = 0.2,
     edge.width=E(g.reddit.orlando.7be)$weight, 
     edge.color=ifelse(E(g.reddit.orlando.7be)$weight >= 0.00, "green", "red"))



#==============================================================================
# FUNCTION: find top 10 nodes by highest degree
#==============================================================================
f.top10.vertices <- function(g.event) {

  # Get the vertex attributes for the tables: user input element
  dt.vertex.attributes <-  as.data.table(get.vertex.attribute(g.event))
  
  #Get the 10 nodes largest degree, not needed for graph, RETURN this dt.
  dt.top10nodes <- dt.vertex.attributes[order(-degree)][1:10]
  return(dt.top10nodes)
} 

#Example use:
dt.orlando.top10 <- f.top10.vertices(g.orlando.full)


