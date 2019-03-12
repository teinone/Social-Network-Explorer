
#==============================================================================
# obama: create graphs  
#==============================================================================

#==============================================================================
# Create Graphs
#==============================================================================
# Function for 15 days before and after
g.obama.15d.b <- f.network.graph(dt.r.obama.15d.b)
g.obama.15d.a <- f.network.graph(dt.r.obama.15d.a)
g.obama.15d.f <- f.network.graph(dt.r.obama.15d.f)

# Function for 10 days before and after
g.obama.10d.b <- f.network.graph(dt.r.obama.10d.b)
g.obama.10d.a <- f.network.graph(dt.r.obama.10d.a)

# Function for 5 days before and after
g.obama.5d.b  <- f.network.graph(dt.r.obama.5d.b)
g.obama.5d.a  <- f.network.graph(dt.r.obama.5d.a)


#  Induced subgraph
g.obama.subgraph.15d.b <- f.network.graph(dt.r.obama.15d.b)
g.obama.subgraph.15d.a <- f.network.graph(dt.r.obama.15d.a)
g.obama.subgraph.15d.f <- f.network.graph(dt.r.obama.15d.f)

# Function for 10 days before and after
g.obama.subgraph.10d.b <- f.network.graph(dt.r.obama.10d.b)
g.obama.subgraph.10d.a <- f.network.graph(dt.r.obama.10d.a)

# Function for 5 days before and after
g.obama.subgraph.5d.b  <- f.network.graph(dt.r.obama.5d.b)
g.obama.subgraph.5d.a  <- f.network.graph(dt.r.obama.5d.a)


#==============================================================================
#  Plot subgraph
#==============================================================================  
# Plot  subgraph 7 days before(15 days)
g.plot.subgraph.obama.15d.b <- f.plot.network.graph(g.obama.subgraph.15d.b)

# Plot  subgraph 7 days after(15 days)
g.plot.subgraph.obama.15d.a <- f.plot.network.graph(g.obama.subgraph.15d.a)

# Plot  subgraph 5 days before(10 days)
g.plot.subgraph.obama.10d.b <- f.plot.network.graph(g.obama.subgraph.10d.b)

# Plot  subgraph 5 days after(10 days)
g.plot.subgraph.obama.10d.a <- f.plot.network.graph(g.obama.subgraph.10d.a)

# Plot  subgraph 2 days before(5 days)
g.plot.subgraph.obama.5d.b  <- f.plot.network.graph(g.obama.subgraph.5d.b)

# Plot  subgraph 2 days after(5 days)
g.plot.subgraph.obama.5d.a  <- f.plot.network.graph(g.obama.subgraph.5d.a)


##=============================================================================
# Find top 10 nodes by highest degree
##=============================================================================

dt.obama.top.10.15.b <- f.top10.vertices(g.obama.15d.b)
dt.obama.top.10.15.a <- f.top10.vertices(g.obama.15d.a)
dt.obama.top.10.15.f <- f.top10.vertices(g.obama.15d.f)

##=============================================================================
#  Get top 10 by centrality
##=============================================================================  

dt.top10.obama.centrality <- f.centrality(dt.r.obama.15d.f)

##=============================================================================
#  Link prediction
##=============================================================================  

dt.top10.obama.links <- f.link.prediction(g.obama.15d.f, dt.obama.top.10.15.f)

##=============================================================================
#  Change in sentiment
##=============================================================================  

dt.obama.change <- f.sentiment.change(dt.r.obama.15d.b, dt.r.obama.15d.a)

##=============================================================================
# Summary stats
##=============================================================================  

df.obama.stats <- f.stats(g.obama.subgraph.15d.f)

