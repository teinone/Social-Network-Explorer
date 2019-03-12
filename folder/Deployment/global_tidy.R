#==============================================================================
# LOAD DEPENDENCIES
#==============================================================================


library(shiny)
library(markdown)
library(shinydashboard, warn.conflicts = FALSE)
library(data.table)     
library(ggplot2)        
library(stringr)
library(dygraphs)
library(plyr, warn.conflicts = FALSE)
library(scales)
library(plotly, warn.conflicts = FALSE)
library(shinyWidgets)
library(igraph, warn.conflicts = FALSE)

#==============================================================================
# LOAD MASTER DATA
#==============================================================================
load("dt.reddit.time.RData")

#==============================================================================
# PLOTTING HISTOGRAM FUNCTION
#==============================================================================
# General function for creating plots
# Params: enter subset dt, event title as a string 
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
  graph.data.frame(dt.r.orlando.15d.f[, list(TARGET_SUBREDDIT,
                                             TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.orlando <- 
  set_vertex_attr(g.r.sentiment.scatter.orlando, 
                  "positive_outbound",
                  value = 
                    as.numeric(
                      dt.r.orlando.15d.f$Positive_Sentiment[match(V(
                        g.r.sentiment.scatter.orlando)$name, 
                        dt.r.orlando.15d.f$SOURCE_SUBREDDIT)]))

g.r.sentiment.scatter.orlando <- 
  set_vertex_attr(g.r.sentiment.scatter.orlando, 
                  "negative_outbound", 
                  value = as.numeric(
                    dt.r.orlando.15d.f$Negative_Sentiment[match(V(
                      g.r.sentiment.scatter.orlando)$name, 
                      dt.r.orlando.15d.f$SOURCE_SUBREDDIT)]))

V(g.r.sentiment.scatter.orlando)$out_degree <-
  degree(g.r.sentiment.scatter.orlando, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.orlando <- 
  data.frame(Vertex            = as.vector(V(g.r.sentiment.scatter.orlando)), 
             name              = V(g.r.sentiment.scatter.orlando)$name, 
             positive_outbound = V(g.r.sentiment.scatter.orlando)$positive_outbound, 
             negative_outbound = V(g.r.sentiment.scatter.orlando)$negative_outbound, 
             out_degree        = V(g.r.sentiment.scatter.orlando)$out_degree)

# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.orlando <- as.data.table(df.g.r.sentiment.out.orlando)

# Filter out the NA values
dt.g.r.sentiment.filter.out.orlando <- 
  dt.g.r.sentiment.out.orlando[!positive_outbound == "NA"]

dt.g.r.sentiment.filter.out.orlando <- 
  dt.g.r.sentiment.filter.out.orlando[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for ferguson
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.ferguson <- 
  graph.data.frame(dt.r.ferguson.15d.f[, list(TARGET_SUBREDDIT,
                                              TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.ferguson <- 
  set_vertex_attr(g.r.sentiment.scatter.ferguson, 
                  "positive_outbound", 
                  value = 
                    as.numeric(
                      dt.r.ferguson.15d.f$Positive_Sentiment[match(V(
                        g.r.sentiment.scatter.ferguson)$name, 
                        dt.r.ferguson.15d.f$SOURCE_SUBREDDIT)]))

g.r.sentiment.scatter.ferguson <- 
  set_vertex_attr(g.r.sentiment.scatter.ferguson, 
                  "negative_outbound", 
                  value = 
                    as.numeric(
                      dt.r.ferguson.15d.f$Negative_Sentiment[match(V(
                        g.r.sentiment.scatter.ferguson)$name, 
                        dt.r.ferguson.15d.f$SOURCE_SUBREDDIT)]))

V(g.r.sentiment.scatter.ferguson)$out_degree <- 
  degree(g.r.sentiment.scatter.ferguson, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.ferguson <- 
  data.frame(Vertex            = as.vector(V(g.r.sentiment.scatter.ferguson)), 
             name              = V(g.r.sentiment.scatter.ferguson)$name, 
             positive_outbound = V(g.r.sentiment.scatter.ferguson)$positive_outbound, 
             negative_outbound = V(g.r.sentiment.scatter.ferguson)$negative_outbound, 
                   out_degree  = V(g.r.sentiment.scatter.ferguson)$out_degree)

# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.ferguson <- as.data.table(df.g.r.sentiment.out.ferguson)

# Filter out the NA values
dt.g.r.sentiment.filter.out.ferguson <- 
  dt.g.r.sentiment.out.ferguson[!positive_outbound == "NA"]

dt.g.r.sentiment.filter.out.ferguson <- 
  dt.g.r.sentiment.filter.out.ferguson[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for obama
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.obama <- 
  graph.data.frame(dt.r.obama.15d.f[,list(TARGET_SUBREDDIT, 
                                          TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.obama <- 
  set_vertex_attr(g.r.sentiment.scatter.obama, 
                  "positive_outbound", 
                  value = as.numeric(
                    dt.r.obama.15d.f$Positive_Sentiment[match(V(
                      g.r.sentiment.scatter.obama)$name, 
                      dt.r.obama.15d.f$SOURCE_SUBREDDIT)]))

g.r.sentiment.scatter.obama <- 
  set_vertex_attr(g.r.sentiment.scatter.obama, 
                  "negative_outbound", 
                  value = as.numeric(
                    dt.r.obama.15d.f$Negative_Sentiment[match(V(
                      g.r.sentiment.scatter.obama)$name, 
                      dt.r.obama.15d.f$SOURCE_SUBREDDIT)]))

V(g.r.sentiment.scatter.obama)$out_degree <- 
  degree(g.r.sentiment.scatter.obama, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.obama <- 
  data.frame(Vertex           = as.vector(V(g.r.sentiment.scatter.obama)), 
            name              = V(g.r.sentiment.scatter.obama)$name, 
            positive_outbound = V(g.r.sentiment.scatter.obama)$positive_outbound, 
            negative_outbound = V(g.r.sentiment.scatter.obama)$negative_outbound, 
            out_degree        = V(g.r.sentiment.scatter.obama)$out_degree)

# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.obama <- as.data.table(df.g.r.sentiment.out.obama)

# Filter out the NA values
dt.g.r.sentiment.filter.out.obama <- 
  dt.g.r.sentiment.out.obama[!positive_outbound == "NA"]

dt.g.r.sentiment.filter.out.obama <- 
  dt.g.r.sentiment.filter.out.obama[!negative_outbound == "NA"]

# ---------------------------------------------------------------------------------
# Plot scatter plot for trump
# ---------------------------------------------------------------------------------

#Data frame for graph
g.r.sentiment.scatter.trump <- 
  graph.data.frame(dt.r.trump.15d.f[, list(TARGET_SUBREDDIT, 
                                           TARGET_SUBREDDIT)], directed = TRUE)
# ---------------------------------------------------------------------------------
# Outbound
# Set the necessary attributes: post sentiment (negative & positive), and out_degree
# (out_degree: number of posts linking to other target subr.sentiments)
g.r.sentiment.scatter.trump <- 
  set_vertex_attr(g.r.sentiment.scatter.trump, 
                  "positive_outbound", 
                  value = as.numeric(
                    dt.r.trump.15d.f$Positive_Sentiment[match(V(
                      g.r.sentiment.scatter.trump)$name, 
                      dt.r.trump.15d.f$SOURCE_SUBREDDIT)]))

g.r.sentiment.scatter.trump <- 
  set_vertex_attr(g.r.sentiment.scatter.trump,
                  "negative_outbound", 
                  value = as.numeric(
                    dt.r.trump.15d.f$Negative_Sentiment[match(V(
                      g.r.sentiment.scatter.trump)$name,
                      dt.r.trump.15d.f$SOURCE_SUBREDDIT)]))

V(g.r.sentiment.scatter.trump)$out_degree <- 
  degree(g.r.sentiment.scatter.trump, mode = "out")

# Convert the igraph to a dataframe
df.g.r.sentiment.out.trump <- 
  data.frame(Vertex            = as.vector(V(g.r.sentiment.scatter.trump)), 
             name              = V(g.r.sentiment.scatter.trump)$name, 
             positive_outbound = V(g.r.sentiment.scatter.trump)$positive_outbound, 
             negative_outbound = V(g.r.sentiment.scatter.trump)$negative_outbound, 
             out_degree        = V(g.r.sentiment.scatter.trump)$out_degree)

# Convert the dataframe to a datatable for further analysis
dt.g.r.sentiment.out.trump <- as.data.table(df.g.r.sentiment.out.trump)

# Filter out the NA values
dt.g.r.sentiment.filter.out.trump <- 
  dt.g.r.sentiment.out.trump[!positive_outbound == "NA"]

dt.g.r.sentiment.filter.out.trump <-
  dt.g.r.sentiment.filter.out.trump[!negative_outbound == "NA"]


