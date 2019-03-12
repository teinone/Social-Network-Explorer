
#==============================================================================
# ferguson: create graphs  
#==============================================================================

#==============================================================================
# Create Graphs
#==============================================================================
# Function for 15 days before and after
g.ferguson.15d.b <- f.network.graph(dt.r.ferguson.15d.b)
g.ferguson.15d.a <- f.network.graph(dt.r.ferguson.15d.a)
g.ferguson.15d.f <- f.network.graph(dt.r.ferguson.15d.f)

# Function for 10 days before and after
g.ferguson.10d.b <- f.network.graph(dt.r.ferguson.10d.b)
g.ferguson.10d.a <- f.network.graph(dt.r.ferguson.10d.a)

# Function for 5 days before and after
g.ferguson.5d.b  <- f.network.graph(dt.r.ferguson.5d.b)
g.ferguson.5d.a  <- f.network.graph(dt.r.ferguson.5d.a)


#  Induced subgraph
g.ferguson.subgraph.15d.b <- f.network.graph(dt.r.ferguson.15d.b)
g.ferguson.subgraph.15d.a <- f.network.graph(dt.r.ferguson.15d.a)
g.ferguson.subgraph.15d.f <- f.network.graph(dt.r.ferguson.15d.f)

# Function for 10 days before and after
g.ferguson.subgraph.10d.b <- f.network.graph(dt.r.ferguson.10d.b)
g.ferguson.subgraph.10d.a <- f.network.graph(dt.r.ferguson.10d.a)

# Function for 5 days before and after
g.ferguson.subgraph.5d.b  <- f.network.graph(dt.r.ferguson.5d.b)
g.ferguson.subgraph.5d.a  <- f.network.graph(dt.r.ferguson.5d.a)


#==============================================================================
#  Plot subgraph
#==============================================================================  
# Plot  subgraph 7 days before(15 days)
g.plot.subgraph.ferguson.15d.b <- f.plot.network.graph(g.ferguson.subgraph.15d.b)

# Plot  subgraph 7 days after(15 days)
g.plot.subgraph.ferguson.15d.a <- f.plot.network.graph(g.ferguson.subgraph.15d.a)

# Plot  subgraph 5 days before(10 days)
g.plot.subgraph.ferguson.10d.b <- f.plot.network.graph(g.ferguson.subgraph.10d.b)

# Plot  subgraph 5 days after(10 days)
g.plot.subgraph.ferguson.10d.a <- f.plot.network.graph(g.ferguson.subgraph.10d.a)

# Plot  subgraph 2 days before(5 days)
g.plot.subgraph.ferguson.5d.b  <- f.plot.network.graph(g.ferguson.subgraph.5d.b)

# Plot  subgraph 2 days after(5 days)
g.plot.subgraph.ferguson.5d.a  <- f.plot.network.graph(g.ferguson.subgraph.5d.a)


##=============================================================================
# Find top 10 nodes by highest degree
##=============================================================================

dt.ferguson.top.10.15.b <- f.top10.vertices(g.ferguson.15d.b)
dt.ferguson.top.10.15.a <- f.top10.vertices(g.ferguson.15d.a)
dt.ferguson.top.10.15.f <- f.top10.vertices(g.ferguson.15d.f)

##=============================================================================
#  Get top 10 by centrality
##=============================================================================  

dt.top10.ferguson.centrality <- f.centrality(dt.r.ferguson.15d.f)

##=============================================================================
#  Link prediction
##=============================================================================  

dt.top10.ferguson.links <- f.link.prediction(g.ferguson.15d.f, dt.ferguson.top.10.15.f)

##=============================================================================
#  Change in sentiment
##=============================================================================  

dt.ferguson.change <- f.sentiment.change(dt.r.ferguson.15d.b, dt.r.ferguson.15d.a)

##=============================================================================
#  Summary stats
##============================================================================= 

df.ferguson.stats <- f.stats(g.ferguson.subgraph.15d.f)

