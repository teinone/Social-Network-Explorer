#==============================================================================
# FUNCTION: Link prediction: This function returns predicted links for the top10 
#==============================================================================
# Returns a dt. with the Jaccard similarity of the top5 highest degrees in the 
# network

f.link.prediction <- function(g.reddit, dt.top10){

  # Predict the links for the top 7 most connected subreddits using Jaccard similarity
  m.predicted.links <- similarity(g.reddit, vids = c(dt.top10$name[1:5]), method = "jaccard")
  
  # Change the column names
  colnames(m.predicted.links) <- c(dt.top10$name[1:5])
  
  #Change the predicted links to a data.table
  dt.predicted.links <- as.data.table(m.predicted.links)
  
  # Add row names
  dt.predicted.links$rownames <-rownames(dt.predicted.links) <- c(dt.top10$name[1:5])
  
  # Create vectors for ordering the columns.
  c.top7  <- c(dt.top10$name[1:5])
  c.order <- c("rownames", c.top7) 
  
  #Set the order of the columns
  setcolorder(dt.predicted.links, c.order)
  
  return(dt.predicted.links)
}

# Example use:
#dt.orlando.links <- f.link.prediction(g.orlando.subgraph, dt.orlando.top10)
#View(dt.orlando.links)





