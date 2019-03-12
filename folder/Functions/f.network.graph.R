#==============================================================================
# FUNCTION: create full network graph and set sentiment as edge weight
#==============================================================================
f.network.graph <- function(dt.x) {
  
  #TESTING DATA
  dt.x <- dt.r.ferguson.10d.f
    
  # Calculate sums for sentiment by source|target
  dt.aggregate <- dt.x[ , base::sum(Compound_Sentiment),
                                    by = sourcetarget]
    
  # Merge with the original subset to get aggregate sentiment by pair
  dt.merge <- merge(dt.x,
                    dt.aggregate, 
                    by = "sourcetarget", 
                    all.x = TRUE)

  # Turn the sentiment values used for weights into absolute values    
  dt.merge$V1 <- dt.merge[, abs(dt.merge$V1)]
  
  #Crete a directed graph from source-target pair
  g.event <- 
    graph.data.frame(dt.merge[, list(SOURCE_SUBREDDIT,
                                     TARGET_SUBREDDIT)],
                     directed = TRUE)
  
  #Define aggregated sentiment as weight
  E(g.event)$weight <- dt.merge[, V1]
  
  # Get the node connections
  V(g.event)$degree <- degree(g.event, mode = "all", loops = TRUE)
  
  # #Add centrality measures to the graphs, omit weight because igraph is a mess.
 
  V(g.event)$closeness   <-   closeness(g.event, weight = NA)
  V(g.event)$betweenness <- betweenness(g.event, weight = NA)
  
return(g.event)
}

# # Example use:
# g.orlando.full <- f.network.graph(dt.r.orlando.10d.f)



#==============================================================================
# FUNCTION: induce subgraph from g.event
#==============================================================================
f.network.subgraph <- function (g.event) {
  
  #Create subgraph with nodes with degree >10
  g.subgraph <- induced_subgraph(g.event, V(g.event)$degree > 10)

  return(g.subgraph)
}

# # Example use:
 # g.orlando.subgraph <- f.network.subgraph(g.orlando.full)



#==============================================================================
# PLOTTING FUNCTION: Takes a graph object, plots it.
#==============================================================================

f.plot.network.graph <- function(g.subgraph) {
   par(mai=c(0,0,1,0))
   plot.g <-
    plot(g.subgraph, layout = layout.fruchterman.reingold, 
       main = g.subgraph$name, 
       vertex.color = "blue", 
       vertex.size = 4, 
       vertex.label = " ", 
       vertex.label.size = 0.1,
       edge.arrow.size = 0.2,
       edge.width=E(g.subgraph)$weight, 
       edge.color=ifelse(E(g.subgraph)$weight >= 0.00, "green", "red"))
   return(plot.g)
}
# # Example:
# f.plot.network.graph(g.orlando.subgraph)



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

# #Example use:
# dt.orlando.top10 <- f.top10.vertices(g.orlando.full)


#==============================================================================
# FUNCTION: get centrality measures as dt
#==============================================================================
f.centrality <- function(g.event) {
  
  # Get all vertex attributes as a dt 
  dt.vertex.attributes <-  as.data.table(get.vertex.attribute(g.event))
  
  #Choose the centrality measures
  dt.centrality <- dt.vertex.attributes[c(1:4)]
  
  return(dt.centrality)  
}

#Example:
# dt.measures <- f.centrality(g.orlando.full)

#==============================================================================
# FUNCTION: get top subreddits by betweenness as a dt
#==============================================================================

f.top10.centrality <- function(dt.centrality) {
  
  # Get all vertex attributes as a dt 
  dt.vertex.attributes <-  as.data.table(get.vertex.attribute(g.event))
  
  #Choose the centrality measures
  dt.centrality <- dt.vertex.attributes[c(1:4)]
  
  return(dt.centrality)  
}

#Example:
# dt.measures <- f.centrality(g.orlando.full)
