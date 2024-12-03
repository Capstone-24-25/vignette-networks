library(tidyverse)
library(igraph)

# bootstrap t-test to compare statistic of a subgraph to the overall graph
bootstrap.t.test <- function(graph, subgraph, statistic, n) {
  if (!is.character(statistic)) {
    warning("statistic must be a string")
  }
  
  # get number of nodes in graph and subgraph
  n.graph <- graph %>% length()
  n.subgraph <- subgraph %>% length()
  
  # extract function to get statistic
  statistic <- get(statistic, mode = "function")
  
  # caculate distribution of the statistic of interest through random sampling
  stat.dist <- lapply(
    1:n,
    function(i) {
      graph %>%
        subgraph(
          sample(n.graph, n.subgraph)
        ) %>%
        statistic()
    }
  ) %>% unlist()
  
  # calculate mean of the distribution
  dist.mean <- mean(stat.dist)
  
  # calculate test statistic
  test.stat <- subgraph %>% statistic()
  
  
  # calculate p-value
  pval <- sum(stat.dist >= dist.mean + abs(test.stat - dist.mean))/n + 
    sum(abs(stat.dist <= dist.mean - abs(test.stat - dist.mean)))/n
  
  stat.dist.center <- stat.dist - dist.mean
  test.stat.center <- test.stat - dist.mean
  
  out <- list(
    dist = stat.dist,
    stat = test.stat,
    pval = pval
  )
  
  return(out)
}