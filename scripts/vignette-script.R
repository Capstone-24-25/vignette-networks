##### Load packages and read in the data #####

source(here::here("scripts/preprocessing.R"))
library(tidyverse)
library(rjson)
library(igraph)
library(knitr)
library(Matrix)

# Read in the JSON file
network_json <- fromJSON(file = here::here("data/congress_network/congress_network_data.json"))

# Use the preprocessing function
network_df <- cnet_json_to_df(network_json)

# Create congress twitter graph
cnet_igraph <- create_cnet_igraph(network_df)


##### Data Visualization #####
plot(cnet_igraph)

# nodes filtered by attributes (party and region)
sub_nodes <- V(g1)[Party %in% c("D", "R") | Region %in% c("Midwest", "South")]

sub_g <- induced_subgraph(g1, vids = sub_nodes)

plot(sub_g)

# sample from node attributes, then name in sample 
set.seed(123)
sample_indices <- sample(nrow(cnet_node_attributes), size = 25)
sample_nodes <- cnet_node_attributes[sample_indices,]

# want to make sure the attributes and nodes match
id_node <- sample_nodes$name
id_from <- network_df$from
id_to <- network_df$to
net_sam <- data.frame(from = double(),
                      to = integer(),
                      weight = double())

for (i in id_node){
  if (i %in% id_to | i %in% id_from){
    matches_from <- subset(network_df, from == i)
    matches_to <- subset(network_df, to == i)
    net_sam <- rbind(net_sam, matches_from, matches_to)
  }
}

net_sam <- subset(net_sam, from %in% sample_nodes$name & to %in% sample_nodes$name)

g2 <- graph_from_data_frame(net_sam,
                            directed = TRUE,
                            vertices = sample_nodes)

# colors based on party
V(g2)$color <- ifelse(V(g2)$Party == "D", "blue", 
                      ifelse(V(g2)$Party == "R", "red", "gray"))

# color based on weight
E(g2)$color <- ifelse(E(g2)$weight > .025, "darkred", "black")

plot(g2,
     edge.width = 2,
     edge.color = E(g2)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     edge.color = "black",
     vertex.color = V(g2)$color, 
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr)  

##### Centrality and Graph Metrics #####

degree_metric <- tibble(degree = degree(cnet_igraph, mode = "all"), node = 1:length(degree(cnet_igraph, mode = "all")))
closeness_metric <- tibble(closeness = closeness(cnet_igraph, mode = "all"), node = 1:length(closeness(cnet_igraph, mode = "all")))
betweenness_metric <- tibble(betweenness = betweenness(cnet_igraph), node = 1:length(betweenness(cnet_igraph)))

degree_metric %>%
  head()

closeness_metric %>%
  head()

betweenness_metric %>%
  head()

highest_5_degree <- tibble(degree = degree(cnet_igraph, mode = "all"), node = 1:length(degree(cnet_igraph))) |>
  arrange(-degree) |>
  slice(1:5)


cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_degree$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_degree) |>
  select(-node) |>
  arrange(-degree) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

highest_5_closeness <- tibble(closeness = closeness(cnet_igraph, mode = "all"), node = 1:length(closeness(cnet_igraph, mode = "all"))) |>
  arrange(-closeness) |>
  slice(1:5)

cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_closeness$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_closeness) |>
  select(-node) |>
  arrange(-closeness) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)


highest_5_betweenness <- tibble(betweenness = betweenness(cnet_igraph), node = 1:length(betweenness(cnet_igraph))) |>
  arrange(-betweenness) |>
  slice(1:5)

cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_betweenness$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_betweenness) |>
  select(-node) |>
  arrange(-betweenness) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

## Creating Subgraphs ##

cnet_subgraph_dem <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "party"
      ) %>%
      `==`("D") 
  )

cnet_subgraph_rep <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "party"
      ) %>%
      `==`("R") 
  )

cnet_subgraph_senate <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "chamber"
      ) %>%
      `==`("Senate")
  )

cnet_subgraph_house <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "chamber"
      ) %>%
      `==`("House")
  )

cnet_subgraph_midwest <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("Midwest") %>%
      replace_na(FALSE)
  )

cnet_subgraph_west <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("West") %>%
      replace_na(FALSE)
  )

cnet_subgraph_south <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("South") %>%
      replace_na(FALSE)
  )

cnet_subgraph_northeast <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("Northeast") %>%
      replace_na(FALSE)
  )

## Subgraph Metrics ##

graphs <- paste(rep("cnet_subgraph", 8), 
                c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"),
                sep = "_")

metrics <- c("reciprocity", "transitivity", "edge_density", "mean_distance")

graph_metrics <- lapply(c("cnet_igraph", graphs), 
                        function(graph) {
                          lapply(metrics,
                                 function(metric) {
                                   get(metric, mode = "function")(get(graph))
                                 })
                        })

graph_metrics %>% 
  unlist() %>%
  matrix(
    ncol = length(metrics),
    byrow = TRUE
  ) %>%
  `colnames<-`(metrics) %>%
  `rownames<-`(c("overall", c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"))) %>%
  kable()

##### Bootstrap Hypothesis Testing #####

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

bootstrap.test.results <- matrix(
  data = rep(rep(0, length(groups)), length(metrics)),
  nrow = length(graphs), 
  ncol = length(metrics), 
  dimnames = (list(c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"), metrics))
)

if(FALSE) {
  for (i in 1:length(graphs)) {
    for (j in 1:length(metrics)) {
      bootstrap.test.results[i,j] <- bootstrap.t.test(cnet_igraph, get(graphs[i]), metrics[j], 10000, "lower")$pval
    }
  }
}

load(file = "data/bootstrap-test-results.RData")

bootstrap.test.results %>%
  kable()


##### adjacency matrix #####
cnet_adj <- as_adjacency_matrix(cnet_igraph)

reorder_cnet_adj <- function(adj_mat, attribute) {
  node_attributes <- read_csv("data/congress_node_attributes.csv")
  
  new_order <- node_attributes %>% 
    mutate(
      rand_order = sample(node)
    ) %>%
    arrange(
      get(attribute), rand_order
    ) %>%
    mutate(
      new_order = row_number()
    ) %>%
    arrange(
      node
    ) %>%
    pull(
      new_order
    )
  
  new_adj_mat <- adj_mat
  for (i in 1:475) {
    for (j in 1:475) {
      new_adj_mat[new_order[i], new_order[j]] <- adj_mat[i,j]
    }
  }
  
  return(new_adj_mat)
}

cnet_adj_chamber <- reorder_cnet_adj(cnet_adj, "chamber")
cnet_adj_party <- reorder_cnet_adj(cnet_adj, "party")
cnet_adj_state <- reorder_cnet_adj(cnet_adj, "state")
cnet_adj_region <- reorder_cnet_adj(cnet_adj, "region")

cnet_adj_chamber %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL
  )

cnet_adj_party %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL
  )

cnet_adj_state %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL
  )

cnet_adj_region %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL
  )