load("/Users/Desktop/NDATeam12/reddit.masters.RData")
library(igraph)
library(data.table)
stats <- function(g){
  g <- c(
    number.of.degree <-  as.numeric(length(degree(g))),
    number.of.nodes <- as.numeric(length(V(g))),
    average.degree.number <- mean(degree(g)),
    average.path.length <- average.path.length(g),
    median.degree <- median(degree(g)),
    diameter <- diameter(g),
    sd.degree <- sd(degree(g))
  )
  g <- data.frame(g, row.names=c("number of degree", "number of nodes", "average degree number", "avarage path lenghth", "median degee", "diameter", "standard degree"))
  return(g)
}




         
