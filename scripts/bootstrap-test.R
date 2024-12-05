library(tidyverse)
library(igraph)

# bootstrap t-test to compare statistic of a subgraph to the overall graph
bootstrap.t.test <- function(graph, subgraph, statistic, n, type = c("upper", "lower", "two-tailed")) {
  
  # get number of nodes in graph and subgraph
  n.graph <- graph %>% length()
  n.subgraph <- subgraph %>% length()
  
  # extract function to get statistic
  statistic <- get(statistic, mode = "function")
  
  # calculate distribution of the statistic of interest through random sampling
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
  if (type == "upper") {
    pval <- sum(stat.dist >= test.stat)/n
  }
  else if (type == "lower") {
    pval <- sum(stat.dist <= test.stat)/n
  }
  else if (type == "two-tailed") {
    pval <- sum(stat.dist >= dist.mean + abs(test.stat - dist.mean))/n + 
      sum(abs(stat.dist <= dist.mean - abs(test.stat - dist.mean)))/n
  }
  
  stat.dist.center <- stat.dist - dist.mean
  test.stat.center <- test.stat - dist.mean
  
  out <- list(
    dist = stat.dist,
    stat = test.stat,
    pval = pval
  )
  
  return(out)
}

source("scripts/preprocessing.R")

cnet.igraph <- fromJSON(file = "data/congress_network/congress_network_data.json") %>%
  cnet_json_to_df() %>%
  create_cnet_igraph()

cnet.subgraph.dem <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Party"
      ) %>%
      `==`("D") 
  )

cnet.subgraph.rep <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Party"
      ) %>%
      `==`("R") 
  )

cnet.subgraph.senate <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Chamber"
      ) %>%
      `==`("Senate")
  )

cnet.subgraph.house <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Chamber"
      ) %>%
      `==`("House")
  )

cnet.subgraph.midwest <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Region"
      ) %>%
      `==`("Midwest") %>%
      replace_na(FALSE)
    
  )

cnet.subgraph.west <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Region"
      ) %>%
      `==`("West") %>%
      replace_na(FALSE)
  )

cnet.subgraph.south <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Region"
      ) %>%
      `==`("South") %>%
      replace_na(FALSE)
  )

cnet.subgraph.northeast <- cnet.igraph %>%
  subgraph(
    cnet.igraph %>%
      vertex_attr(
        "Region"
      ) %>%
      `==`("Northeast") %>%
      replace_na(FALSE)
  )

graphs <- paste(rep("cnet.subgraph", 8), 
                c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"),
                sep = ".")

metrics <- c("reciprocity", "transitivity", "edge_density", "mean_distance")

bootstrap.test.results <- matrix(
  data = rep(rep(0, length(groups)), length(metrics)),
  nrow = length(graphs), 
  ncol = length(metrics), 
  dimnames = (list(c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"), metrics))
)

for (i in 1:length(graphs)) {
  for (j in 1:length(metrics)) {
    bootstrap.test.results[i,j] <- bootstrap.t.test(cnet.igraph, get(graphs[i]), metrics[j], 10000)$pval
  }
}


ggplot() +
  geom_histogram(
    aes(
      x = bootstrap.t.test(cnet_igraph, cnet_subgraph_rep, "reciprocity", 10000, "two-tailed")$dist,
      fill = "skyblue"
    ),
    bins = 30
  ) +
  theme_minimal()


save(bootstrap.test.results, "data/bootstrap-test-results.RData")