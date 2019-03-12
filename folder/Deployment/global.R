#==============================================================================
# PLOTTING HISTOGRAM FUNCTION
#==============================================================================
# General function for creating plots
# Params: enter subset dt, event title as a string 


load("dt.reddit.events.RData")
load("dt.reddit.time.RData")
library(shiny)
library(markdown)
library(shinydashboard)
library(data.table)     
library(ggplot2)        
library(stringr)
#library(dygraphs)
library(plyr)
library(scales)
library(plotly)
library(shinyWidgets)
# library(dashboardthemes)
library(igraph)
f.event.hist <- function(dt.x, eventtitle){
  final.plot <- 
    ggplot(dt.x, aes(x = TIMESTAMP)) + 
    geom_histogram() + 
    scale_x_datetime(date_labels = "%m/%d",
                     breaks      = "1 day") + 
    ggtitle(toString(eventtitle)) + 
    labs(y = "Count", 
         x = "Date") + 
    theme(axis.text.x = element_text(size = 7, angle = 90))
  return(final.plot)
}



#==============================================================================
# PLOTTING COMBOCHART FUNCTION
#==============================================================================
f.combochart <- function(dt.x){
  
  # Positive/ Negative Sentiment trend line + histogram (Compound Graph)
  combo.event <- 
    ggplot(dt.x, aes(x = TIMESTAMP)) + 
    geom_histogram() + 
    scale_x_datetime(date_labels = "%m/%d", 
                     breaks      = "1 day")
  
  # add the secondary y axis, revert the above transformation
  combo.event <- combo.event + 
    scale_y_continuous(sec.axis = sec_axis(~./4000, name = "Sentiment"))
  
  # add the next layer of the trend line of Positive/ Negative Sentiment
  combo.event <- combo.event + 
    geom_smooth(aes(y = Positive_Sentiment * 4000, colour = "Positive"), 
                se = FALSE)
  
  combo.event <- combo.event + 
    geom_smooth(aes(y = Negative_Sentiment * 4000, colour = "Negative"), 
                se = FALSE)
  
  # Set colours
  combo.event <- combo.event + 
    scale_colour_manual(values = c("red", "green"))
  
  
  # assign themes and colors to the trend line
  combo.event <- combo.event + 
    labs(y = "Count", x = "Date") + 
    theme(axis.text.x = element_text(size = 9, angle = 90))
  
  # Return final graph
  return (combo.event)
}

# =============================================================================
#  FUNCTION: Calculate sentiment change for dt.before & dt.after
# ============================================================================= 

f.sentiment.change <- function (dt.before, dt.after) {
  
  # Aggregate and merge:
  # Calculate sums for sentiment by source|target
  dt.before.aggregate <-  dt.before[ , base::sum(Compound_Sentiment),
                                     by = "sourcetarget"]
  
  dt.after.aggregate <-  dt.after[ , base::sum(Compound_Sentiment),
                                   by = "sourcetarget"]
  
  
  # Merge with the original subset to get aggregate sentiment by pair
  dt.before.merge <- merge(dt.before,
                           dt.before.aggregate, 
                           by = "sourcetarget", 
                           all.x = TRUE)
  
  dt.after.merge <- merge(dt.after,
                          dt.after.aggregate, 
                          by = "sourcetarget", 
                          all.x = TRUE)
  
  
  # Creates a new attribute "sent" which takes the value of compound sentiment
  # after the event, matches with the unique sourcetarget combination and adds
  # them to the dataset before the event.
  
  dt.before.merge$sent <- 
    dt.after.merge$Compound_Sentiment[match(dt.before.merge$sourcetarget, 
                                            dt.after.merge$sourcetarget)]
  
  # Omit all rows with NA in this column
  dt.before.transformed <- na.omit(dt.before.merge, cols = "sent")
  
  # Check links than were positive before the event and became negative after
  dt.negative <- subset(dt.before.transformed, V1 >= 0 & sent < 0)[1:10]
  
  # Fix bug of a repeating edge. Rename the columns.
  dt.change.sentiment <- dt.negative[!2, c(2,3,14,15)]
  names(dt.change.sentiment) <- c("Source",
                                  "Target",
                                  "Before_Sentiment",
                                  "After_Sentiment")
  return(dt.change.sentiment)
}


#==============================================================================
# FUNCTION: create full network graph and set sentiment as edge weight
#==============================================================================
f.network.graph <- function(dt.x) {
  
  # Calculate sums for sentiment by source|target
  dt.aggregate <- dt.x[ , base::sum(Compound_Sentiment),
                        by = "sourcetarget"]
  
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
# Example:
f.plot.network.graph(g.orlando.subgraph)



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

#==============================================================================
# FUNCTION: Link prediction: This function returns predicted links for the top10 
#==============================================================================
# Returns a dt. with the Jaccard similarity of the top7 highest degrees in the 
# network

f.link.prediction <- function(g.reddit, dt.top10){
  
  # Predict the links for the top 7 most connected subreddits using Jaccard similarity
  m.predicted.links <- similarity(g.reddit, vids = c(dt.top10$name[1:7]), method = "jaccard")
  
  # Change the column names
  colnames(m.predicted.links) <- c(dt.top10$name[1:7])
  
  #Change the predicted links to a data.table
  dt.predicted.links <- as.data.table(m.predicted.links)
  
  # Add row names
  dt.predicted.links$rownames <-rownames(dt.predicted.links) <- c(dt.top10$name[1:7])
  
  # Create vectors for ordering the columns.
  c.top7  <- c(dt.top10$name[1:7])
  c.order <- c("rownames", c.top7) 
  
  #Set the order of the columns
  setcolorder(dt.predicted.links, c.order)
  
  return(dt.predicted.links)
}

#==============================================================================
# FUNCTION: returns the degree distribution list for a dt.
#==============================================================================
f.degree.distribution <- function(dt.x) {
  
  # Select unique Source values:
  dt.unique <- dt.x[, list(unique(SOURCE_SUBREDDIT))]
  
  # Change column name to Source
  setnames(dt.unique, c("V1"), c("SOURCE_SUBREDDIT"))
  
  # Create a new dt with total sentiment column aggregated by Source
  dt.total.sentiment <- 
    setDT(dt.x)[, .(total_sentiment = sum(Compound_Sentiment)), 
                by = SOURCE_SUBREDDIT]
  
  # Merge by Source 
  dt.sentiment <- merge(dt.x, 
                        dt.total.sentiment, 
                        by = "SOURCE_SUBREDDIT")
  
  #Plot Network degree distribution
  g.network <- graph.data.frame(dt.x, directed = TRUE)
  
  # Add a sentiment attribute by matching source name to the original table
  V(g.network)$ag_sentiment <- 
    as.numeric(dt.sentiment$total_sentiment[match(V(g.network)$name, 
                                                  dt.x$SOURCE_SUBREDDIT)])
  
  # Take degree distribution: non-cumulative, in and out degree
  l.ddistribution <- degree(g.network, mode = "all")
  
  # Turn into a dt for viz
  dt.ddirstribution <- as.data.table(l.ddistribution)
  
  return(dt.ddirstribution)
}

# Example code:
# l.ddistr.ferguson <- f.degree.distribution(dt.reddit.ferguson)
# qplot(l.ddistr.ferguson)

#==============================================================================
# SENTIMENT ANALYSE III (MINXIN)
#==============================================================================

# ---------------------------------------------------------------------------------
# Plot scatter plot for source_subreddit sentiments
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# Plot scatter plot for orlando
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.orlando <- 
  graph.data.frame(dt.reddit.orlando[, list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.orlando <- 
  set_vertex_attr(g.r.sentiment.scatter.orlando, 
                  "positive_outbound",
                  value = as.numeric(dt.reddit.orlando$Positive_Sentiment[match(V(g.r.sentiment.scatter.orlando)$name, 
                                                                                dt.reddit.orlando$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.orlando <- 
  set_vertex_attr(g.r.sentiment.scatter.orlando, 
                  "negative_outbound", 
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
g.r.sentiment.scatter.ferguson <- 
  graph.data.frame(dt.reddit.ferguson[, list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.ferguson <- 
  set_vertex_attr(g.r.sentiment.scatter.ferguson, 
                  "positive_outbound", 
                  value = as.numeric(dt.reddit.ferguson$Positive_Sentiment[match(V(g.r.sentiment.scatter.ferguson)$name, 
                                                                                 dt.reddit.ferguson$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.ferguson <- 
  set_vertex_attr(g.r.sentiment.scatter.ferguson, 
                  "negative_outbound", 
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
g.r.sentiment.scatter.obama <- 
  graph.data.frame(dt.reddit.obama[,list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.obama <- 
  set_vertex_attr(g.r.sentiment.scatter.obama, 
                  "positive_outbound", 
                  value = as.numeric(dt.reddit.obama$Positive_Sentiment[match(V(g.r.sentiment.scatter.obama)$name, 
                                                                              dt.reddit.obama$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.obama <- 
  set_vertex_attr(g.r.sentiment.scatter.obama, 
                  "negative_outbound", 
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
g.r.sentiment.scatter.trump <- 
  graph.data.frame(dt.reddit.trump[, list(TARGET_SUBREDDIT, TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.trump <- 
  set_vertex_attr(g.r.sentiment.scatter.trump, 
                  "positive_outbound", 
                  value = as.numeric(dt.reddit.trump$Positive_Sentiment[match(V(g.r.sentiment.scatter.trump)$name, 
                                                                              dt.reddit.trump$SOURCE_SUBREDDIT)]))
g.r.sentiment.scatter.trump <- 
  set_vertex_attr(g.r.sentiment.scatter.trump, "negative_outbound", 
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



