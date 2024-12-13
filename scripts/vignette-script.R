# load required scripts and packages
source(here::here("scripts/preprocessing.R"))
library(tidyverse)
library(rjson)
library(igraph)
library(knitr)
library(Matrix)

### READ IN DATA AND CONSTRUCT NETWORK ###

# read in the JSON file
network_json <- fromJSON(file = here::here("data/congress_network/congress_network_data.json"))

# use the preprocessing function
network_df <- cnet_json_to_df(network_json)

# create congress twitter graph
cnet_igraph <- create_cnet_igraph(network_df)

### DATA VISUALIZATION ###

## Plot 1 ##

# color nodes based on party membership
V(cnet_igraph)$color <- ifelse(V(cnet_igraph)$party == "D", "blue", 
                               ifelse(V(cnet_igraph)$party == "R", "red", "gray"))

# plot congress network
plot(cnet_igraph,
     main = "Complete Network Graph")

## Plot 2 ##
=======
##### Data Visualization #####
V(cnet_igraph)$color <- ifelse(V(cnet_igraph)$party == "D", "blue", 
                               ifelse(V(cnet_igraph)$party == "R", "red", "gray"))
plot(cnet_igraph,
     main = "Complete Network Graph")


# filter nodes by attributes (party or region)
sub_nodes <- V(cnet_igraph)[party %in% c("D", "R") | region %in% c("Midwest", "South")]

sub_g <- induced_subgraph(cnet_igraph, vids = sub_nodes)

# colors based on region
V(sub_g)$color <- ifelse(V(sub_g)$region == "Midwest", "orange", 
                         ifelse(V(sub_g)$region == "South", "purple", "gray"))

plot(sub_g,
     main = "Subgraph of Filtered Nodes (Region)")


# sample from node attributes, then name in sample 
# load in node attributes
cnet_node_attributes <- read_csv("data/congress_node_attributes.csv",
                                 show_col_types = FALSE)
>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03

# filter nodes by attributes (party or region)
sub_nodes <- V(cnet_igraph)[party %in% c("D", "R") & region %in% c("Midwest", "South")]

# generate induced subgraph based on filtered nodes
sub_g <- induced_subgraph(cnet_igraph, vids = sub_nodes)

# color nodes based on region
V(sub_g)$color <- ifelse(V(sub_g)$region == "Midwest", "orange", "purple")

# plot subgraph
plot(sub_g,
     main = "Subgraph of Filtered Nodes (Region)")

## Plot 3 ##

# load in node attributes
cnet_node_attributes <- read_csv("data/congress_node_attributes.csv",
                                 show_col_types = FALSE)

# sample 25 nodes
set.seed(123)
sample_nodes <- sample(length(cnet_igraph), size = 25)

# create subgraph using sampled nodes
g2 <- subgraph(cnet_igraph, sample_nodes)
=======
# want to make sure the attributes and nodes match
id_node <- sample_nodes$node
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

net_sam <- subset(net_sam, from %in% sample_nodes$node & to %in% sample_nodes$node)

g2 <- graph_from_data_frame(net_sam,
                            directed = TRUE,
                            vertices = sample_nodes)
>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03

# colors based on party
V(g2)$color <- ifelse(V(g2)$party == "D", "blue", 
                      ifelse(V(g2)$party == "R", "red", "gray"))

# color based on weight
E(g2)$color <- ifelse(E(g2)$weight > .025, "darkred", "black")

# plot subgraph
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
     layout = layout_with_fr,
     main = "Subset of Nodes and Edges")

## Plot 4 ##

# sample from node attributes, then match to corresponding connections in the network
set.seed(123)

# filter the nodes by region (Midwest and South)
filtered_nodes <- V(cnet_igraph)[V(cnet_igraph)$region %in% c("Midwest", "South")] %>% as.vector()

# random sample from the filtered nodes
sample_nodes <- filtered_nodes[sample(length(filtered_nodes), size = 25)]

# create subgraph using sampled nodes
g2 <- subgraph(cnet_igraph, sample_nodes)

# colors based on region
V(g2)$color <- ifelse(V(g2)$region == "Midwest", "orange", 
                      ifelse(V(g2)$region == "South", "purple", "gray"))

# colors based on weight
E(g2)$color <- ifelse(E(g2)$weight > 0.025, "darkred", "black")

# plot subgraph
plot(g2,
     edge.width = 2,
     edge.color = E(g2)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     vertex.color = V(g2)$color, 
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr,
     main = "Random Sample of 25 Nodes (Filtered by Midwest and South Regions)")

### CENTRALITY METRICS ###

# calculate degrees for each node
degree_metric <- tibble(degree = degree(cnet_igraph, mode = "all"), node = 1:length(degree(cnet_igraph, mode = "all")))

# calculate the closeness for each node
closeness_metric <- tibble(closeness = closeness(cnet_igraph, mode = "all"), node = 1:length(closeness(cnet_igraph, mode = "all")))

# calculate the betweenness for each node
betweenness_metric <- tibble(betweenness = betweenness(cnet_igraph), node = 1:length(betweenness(cnet_igraph)))

# show the degree for the first several nodes
degree_metric %>%
  head()

# show the closeness for the first several nodes
closeness_metric %>%
  head()

# show the betweenness for the first several nodes
betweenness_metric %>%
  head()

# get the nodes with the highest degree
highest_5_degree <- tibble(degree = degree(cnet_igraph, mode = "all"), node = 1:length(degree(cnet_igraph))) |>
  arrange(-degree) |>
  slice(1:5)

# identify the congresspeople with the highest degree and show results
cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_degree$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_degree) |>
  select(-node) |>
  arrange(-degree) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

# get the nodes with the highest closeness
highest_5_closeness <- tibble(closeness = closeness(cnet_igraph, mode = "all"), node = 1:length(closeness(cnet_igraph, mode = "all"))) |>
  arrange(-closeness) |>
  slice(1:5)

# identify the congresspeople with the highest closeness and show results
cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_closeness$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_closeness) |>
  select(-node) |>
  arrange(-closeness) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

# get the nodes with the highest betweennes
highest_5_betweenness <- tibble(betweenness = betweenness(cnet_igraph), node = 1:length(betweenness(cnet_igraph))) |>
  arrange(-betweenness) |>
  slice(1:5)

# identify the congresspeople with the highest betweenness and show results
cnet_node_attributes |>
  filter(cnet_node_attributes$node %in% highest_5_betweenness$node) |>
  select(node, congressperson) |>
  inner_join(highest_5_betweenness) |>
  select(-node) |>
  arrange(-betweenness) |>
  kableExtra::kable() |>
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

### GRAPH/SUBGRAPH METRICS ###

# create subgraph of democrats
cnet_subgraph_dem <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "party"
      ) %>%
      `==`("D") 
  )

# create subgraph of republicans
cnet_subgraph_rep <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "party"
      ) %>%
      `==`("R") 
  )

# create subgraph of senators
cnet_subgraph_senate <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "chamber"
      ) %>%
      `==`("Senate")
  )

# create subgraph of representatives
cnet_subgraph_house <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "chamber"
      ) %>%
      `==`("House")
  )

# create subgraph of congresspeople from the midwest
cnet_subgraph_midwest <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("Midwest") %>%
      replace_na(FALSE)
  )

# create subgraph of congresspeople from the west
cnet_subgraph_west <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("West") %>%
      replace_na(FALSE)
  )

# create subgraph of congresspeople from the south
cnet_subgraph_south <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("South") %>%
      replace_na(FALSE)
  )

# create subgraph of congresspeople from the northeast
cnet_subgraph_northeast <- cnet_igraph %>%
  subgraph(
    cnet_igraph %>%
      vertex_attr(
        "region"
      ) %>%
      `==`("Northeast") %>%
      replace_na(FALSE)
  )

# create vector containing the names of all 8 subgraphs
graphs <- paste(rep("cnet_subgraph", 8), 
                c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"),
                sep = "_")

# create vector containing the names of the metrics we wish to calculate
metrics <- c("reciprocity", "transitivity", "edge_density", "mean_distance")

# compute the metrics for each subgraph
graph_metrics <- lapply(c("cnet_igraph", graphs), 
                        function(graph) {
                          lapply(metrics,
                                 function(metric) {
                                   get(metric, mode = "function")(get(graph))
                                 })
                        })

# show metrics for each subgraph and the overall network
graph_metrics %>% 
  unlist() %>%
  matrix(
    ncol = length(metrics),
    byrow = TRUE
  ) %>%
  `colnames<-`(metrics) %>%
  `rownames<-`(c("overall", c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"))) %>%
  kable()

### HYPTOHESIS TESTING ###

set.seed(12132024)

# resampling t-test to compare statistic of a subgraph to the overall graph
resample.t.test <- function(graph, subgraph, statistic, n, type = c("upper", "lower", "two-tailed")) {
  
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
  
  # prepare list for output
  out <- list(
    dist = stat.dist,
    stat = test.stat,
    pval = pval
  )
  
  # return output
  return(out)
}

# prepare matrix to store t-test results
t.test.results <- matrix(
  data = rep(rep(0, length(groups)), length(metrics)),
  nrow = length(graphs), 
  ncol = length(metrics), 
  dimnames = (list(c("dem", "rep", "house", "senate", "midwest", "west", "south", "northeast"), metrics))
)

# perform t-tests on subgraph metrics
for (i in 1:length(graphs)) {
  for (j in 1:length(metrics)) {
    t.test.results[i,j] <- resample.t.test(cnet_igraph, get(graphs[i]), metrics[j], 10000, "two-tailed")$pval
  }
}

# print results
t.test.results %>%
  kable(
    caption = "Subgraph Metric p-values"
  )

# apply benjamini-yekuteili correction and print results
t.test.results %>%
  # convert to dataframe for easier manipulatioin
  as.data.frame() %>%
  # add subgraph column
  mutate(
    subgraph = row.names(.),
    .before = "reciprocity"
  ) %>%
  # reset rownames
  `row.names<-`(NULL) %>%
  # pivot to long format
  pivot_longer(
    -subgraph,
    names_to = "metric",
    values_to = "p.value"
  ) %>%
  # sort by increasing p-value
  arrange(
    p.value
  ) %>%
  mutate(
    i = row_number()
  ) %>%
  group_by(i) %>%
  # apply benjamini-yekuteili correction
  mutate(
    p.by = nrow(.)*sum(1/1:i)*p.value/i
  ) %>%
  ungroup() %>%
  # remove columns that are no longer needed
  select(
    -i,-p.value
  ) %>%
  # pivot back to wide format for legibility
  pivot_wider(
    names_from = metric,
    values_from = p.by
  ) %>%
  relocate(
    reciprocity,
    transitivity,
    .before = "edge_density"
  ) %>%
  # show results
  kable(
    caption = "Benjamini-Yekuteili Corrected Subgraph Metric p-values"
  )

### ADJACENCY MATRICES ###

# convert igraph object into an adjacency matrix
cnet_adj <- as_adjacency_matrix(cnet_igraph)

# define helper function to reorder adjacency matrix based on particular node attributes
reorder_cnet_adj <- function(adj_mat, attribute) {
  
  # load in node attributes
  node_attributes <- read_csv("data/congress_node_attributes.csv",
                              show_col_types = FALSE)
  
  # obtain new order by arranging based on attribute
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
  
  # reorder adjancency matrix based on the new order
  new_adj_mat <- adj_mat
  for (i in 1:475) {
    for (j in 1:475) {
      new_adj_mat[new_order[i], new_order[j]] <- adj_mat[i,j]
    }
  }
  
  return(new_adj_mat)
}

# generate adjacency matrices ordered by each set of node attributes
cnet_adj_chamber <- reorder_cnet_adj(cnet_adj, "chamber")
cnet_adj_party <- reorder_cnet_adj(cnet_adj, "party")
cnet_adj_state <- reorder_cnet_adj(cnet_adj, "state")
cnet_adj_region <- reorder_cnet_adj(cnet_adj, "region")

# visualize adjacency matrix ordered by chamber
cnet_adj_chamber %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    main = "Adjacency Matrix Ordered by Chamber"
  )

# visualize adjacency matrix ordered by party
cnet_adj_party %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    main = "Adjacency Matrix Ordered by Party"
  )

# visualize adjacency matrix ordered by state
cnet_adj_state %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    main = "Adjacency Matrix Ordered by State"
  )

# visualize adjacency matrix ordered by region
cnet_adj_region %>%
  Matrix() %>%
  image(
    col.regions = c("black"),
    sub = NULL,
    xlab = NULL,
    ylab = NULL,
    main = "Adjacency Matrix Ordered by Region"
  )

### COMMUNITY DETECTION ###
=======
    ylab = NULL
  )

##### Community Detection #####
>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03

# remove edges with weight below 0.003
g2_filtered <- delete_edges(g2, E(g2)[weight < 0.003])

# perform Walktrap community detection on the filtered graph
community_walktrap <- cluster_walktrap(g2_filtered)

# assign colors to the communities
V(g2_filtered)$color <- factor(community_walktrap$membership)

# plot results
=======
>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03
plot(g2_filtered,
     edge.width = 2,
     edge.color = E(g2_filtered)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     vertex.color = V(g2_filtered)$color,
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr,
     main = "Community Detection (Filtered Graph with Walktrap)")

# total number of edges
total_edges <- ecount(g2)

# sample from edges, ensuring the sample size doesn't exceed the number of edges
set.seed(123)
sample_size <- min(100, total_edges)

# sample edges
sampled_edges <- sample(E(g2), size = sample_size)
=======
sampled_edges <- sample(E(g2), size = sample_size)  # Sample edges
>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03

# new graph with the sampled edges
g2_sampled <- subgraph.edges(g2, sampled_edges)

# walktrap community detection on the sampled graph
community_walktrap <- cluster_walktrap(g2_sampled)

# color based on community membership
V(g2_sampled)$color <- factor(community_walktrap$membership)

# plot results
=======
# Walktrap community detection on the sampled graph
community_walktrap <- cluster_walktrap(g2_sampled)

V(g2_sampled)$color <- factor(community_walktrap$membership)

>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03
plot(g2_sampled,
     edge.width = 2,
     edge.color = E(g2_sampled)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     vertex.color = V(g2_sampled)$color,
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr,
     main = "Community Detection (Sampled Graph with Walktrap)")

# sample 25 nodes from the graph
set.seed(123)
sampled_nodes <- sample(V(g2), size = 25)

# subgraph containing only the sampled nodes
g2_sampled_nodes <- induced_subgraph(g2, vids = sampled_nodes)

# perform walktrap community detection on the sampled graph
community_walktrap <- cluster_walktrap(g2_sampled_nodes)

# color based on community membership
V(g2_sampled_nodes)$color <- factor(community_walktrap$membership)

# plot results
=======
# perform Walktrap community detection on the sampled graph
community_walktrap <- cluster_walktrap(g2_sampled_nodes)

V(g2_sampled_nodes)$color <- factor(community_walktrap$membership)

>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03
plot(g2_sampled_nodes,
     edge.width = 2,
     edge.color = E(g2_sampled_nodes)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     vertex.color = V(g2_sampled_nodes)$color,
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr,
     main = "Community Detection (Sampled Nodes with Walktrap)")

=======

>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03
# remove edges between nodes with low degrees
low_degree_nodes <- V(g2)[degree(g2) < 3]
g2_pruned <- delete_edges(g2, E(g2)[.from(low_degree_nodes) | .to(low_degree_nodes)])

# walktrap community detection on the pruned graph
community_walktrap <- cluster_walktrap(g2_pruned)

# color based on community membership
V(g2_pruned)$color <- factor(community_walktrap$membership)

# plot results
=======
# Walktrap community detection on the pruned graph
community_walktrap <- cluster_walktrap(g2_pruned)

V(g2_pruned)$color <- factor(community_walktrap$membership)

>>>>>>> 37cbb41ff2732be21c8b046bb87e6d79b2510e03
plot(g2_pruned,
     edge.width = 2,
     edge.color = E(g2_pruned)$color,
     edge.curved = 0.2,
     edge.arrow.size = 0.5,
     vertex.color = V(g2_pruned)$color,
     vertex.size = 30,
     vertex.frame.color = "black",
     vertex.label.color = "white",
     layout = layout_with_fr,
     main = "Community Detection (Pruned Graph with Walktrap)")