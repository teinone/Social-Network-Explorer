stats <- function(g){
  g <- c(
    number.of.degree <-  as.numeric(length(E(g))),
    number.of.nodes <- as.numeric(length(V(g))),
    average.degree.number <- mean(degree(g)),
    average.path.length <- average.path.length(g),
    median.degree <- median(degree(g)),
    sd.degree <- sd(degree(g))
  )
  g <- as.data.table(g)
  g$rownames <-rownames(g) <- c("number of degree", "number of nodes", "average degree number", "avarage path lenghth", "median degee", "standard degree")
  setnames(g, c("g", "rownames"), c("values","measures"))
  
  g <- g[,c("measures", "values")]
  return(g)
}
