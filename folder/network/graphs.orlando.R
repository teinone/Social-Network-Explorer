
#==============================================================================
# ORLANDO: create graphs
#==============================================================================

#==============================================================================
# Create Graphs
#==============================================================================
# Function for 15 days before and after
g.orlando.15d.b <- f.network.graph(dt.r.orlando.15d.b)
g.orlando.15d.a <- f.network.graph(dt.r.orlando.15d.a)
g.orlando.15d.f <- f.network.graph(dt.r.orlando.15d.f)

# Function for 10 days before and after
g.orlando.10d.b <- f.network.graph(dt.r.orlando.10d.b)
g.orlando.10d.a <- f.network.graph(dt.r.orlando.10d.a)

# Function for 5 days before and after
g.orlando.5d.b <- f.network.graph(dt.r.orlando.5d.b)
g.orlando.5d.a <- f.network.graph(dt.r.orlando.5d.a)


#  Induced subgraph
g.orlando.subgraph.15d.b <- f.network.graph(dt.r.orlando.15d.b)
g.orlando.subgraph.15d.a <- f.network.graph(dt.r.orlando.15d.a)
g.orlando.subgraph.15d.f <- f.network.graph(dt.r.orlando.15d.f)

# Function for 10 days before and after
g.orlando.subgraph.10d.b <- f.network.graph(dt.r.orlando.10d.b)
g.orlando.subgraph.10d.a <- f.network.graph(dt.r.orlando.10d.a)

# Function for 5 days before and after
g.orlando.subgraph.5d.b <- f.network.graph(dt.r.orlando.5d.b)
g.orlando.subgraph.5d.a <- f.network.graph(dt.r.orlando.5d.a)


#==============================================================================
#  Plot subgraph
#==============================================================================  
# Plot  subgraph 7 days before(15 days)
g.plot.subgraph.orlando.15d.b <- f.plot.network.graph(g.orlando.subgraph.15d.b)

# Plot  subgraph 7 days after(15 days)
g.plot.subgraph.orlando.15d.a <- f.plot.network.graph(g.orlando.subgraph.15d.a)

# Plot  subgraph 5 days before(10 days)
g.plot.subgraph.orlando.10d.b <- f.plot.network.graph(g.orlando.subgraph.10d.b)

# Plot  subgraph 5 days after(10 days)
g.plot.subgraph.orlando.10d.a <- f.plot.network.graph(g.orlando.subgraph.10d.a)

# Plot  subgraph 2 days before(5 days)
g.plot.subgraph.orlando.5d.b <- f.plot.network.graph(g.orlando.subgraph.5d.b)

# Plot  subgraph 2 days after(5 days)
g.plot.subgraph.orlando.5d.a <- f.plot.network.graph(g.orlando.subgraph.5d.a)


##=============================================================================
# Find top 10 nodes by highest degree
##=============================================================================

dt.orlando.top.10.15.b <- f.top10.vertices(g.orlando.15d.b)
dt.orlando.top.10.15.a <- f.top10.vertices(g.orlando.15d.a)
dt.orlando.top.10.15.f <- f.top10.vertices(g.orlando.15d.f)

##=============================================================================
#  Link prediction
##=============================================================================  

dt.top10.orlando.links <- f.link.prediction(g.orlando.15d.f, orlando.top.10.15.f)

##=============================================================================
#  Change in sentiment
##=============================================================================  

dt.orlando.change <- f.sentiment.change(dt.r.orlando.15d.b, dt.r.orlando.15d.a)



