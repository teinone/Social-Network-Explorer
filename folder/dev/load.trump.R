
#==============================================================================
# trump: create graphs  
#==============================================================================

#==============================================================================
# Create Graphs
#==============================================================================
# Function for 15 days before and after
g.trump.15d.b <- f.network.graph(dt.r.trump.15d.b)
g.trump.15d.a <- f.network.graph(dt.r.trump.15d.a)
g.trump.15d.f <- f.network.graph(dt.r.trump.15d.f)

# Function for 10 days before and after
g.trump.10d.b <- f.network.graph(dt.r.trump.10d.b)
g.trump.10d.a <- f.network.graph(dt.r.trump.10d.a)

# Function for 5 days before and after
g.trump.5d.b  <- f.network.graph(dt.r.trump.5d.b)
g.trump.5d.a  <- f.network.graph(dt.r.trump.5d.a)


#  Induced subgraph
g.trump.subgraph.15d.b <- f.network.graph(dt.r.trump.15d.b)
g.trump.subgraph.15d.a <- f.network.graph(dt.r.trump.15d.a)
g.trump.subgraph.15d.f <- f.network.graph(dt.r.trump.15d.f)

# Function for 10 days before and after
g.trump.subgraph.10d.b <- f.network.graph(dt.r.trump.10d.b)
g.trump.subgraph.10d.a <- f.network.graph(dt.r.trump.10d.a)

# Function for 5 days before and after
g.trump.subgraph.5d.b  <- f.network.graph(dt.r.trump.5d.b)
g.trump.subgraph.5d.a  <- f.network.graph(dt.r.trump.5d.a)


#==============================================================================
#  Plot subgraph
#==============================================================================  
# Plot  subgraph 7 days before(15 days)
g.plot.subgraph.trump.15d.b <- f.plot.network.graph(g.trump.subgraph.15d.b)

# Plot  subgraph 7 days after(15 days)
g.plot.subgraph.trump.15d.a <- f.plot.network.graph(g.trump.subgraph.15d.a)

# Plot  subgraph 5 days before(10 days)
g.plot.subgraph.trump.10d.b <- f.plot.network.graph(g.trump.subgraph.10d.b)

# Plot  subgraph 5 days after(10 days)
g.plot.subgraph.trump.10d.a <- f.plot.network.graph(g.trump.subgraph.10d.a)

# Plot  subgraph 2 days before(5 days)
g.plot.subgraph.trump.5d.b  <- f.plot.network.graph(g.trump.subgraph.5d.b)

# Plot  subgraph 2 days after(5 days)
g.plot.subgraph.trump.5d.a  <- f.plot.network.graph(g.trump.subgraph.5d.a)


##=============================================================================
# Find top 10 nodes by highest degree
##=============================================================================

dt.trump.top.10.15.b <- f.top10.vertices(g.trump.15d.b)
dt.trump.top.10.15.a <- f.top10.vertices(g.trump.15d.a)
dt.trump.top.10.15.f <- f.top10.vertices(g.trump.15d.f)

##=============================================================================
#  Get top 10 by centrality
##=============================================================================  

dt.top10.trump.centrality <- f.centrality(dt.r.trump.15d.f)
dt.centrality.trump <- f.centrality(dt.r.trump.15d.f)

##=============================================================================
#  Link prediction
##=============================================================================  

dt.top10.trump.links <- f.link.prediction(g.trump.15d.f, dt.trump.top.10.15.f)

##=============================================================================
#  Change in sentiment
##=============================================================================  

dt.trump.change <- f.sentiment.change(dt.r.trump.15d.b, dt.r.trump.15d.a)

##=============================================================================
# Summary stats
##=============================================================================  


df.trump.stats <- f.stats(g.trump.subgraph.15d.f)

