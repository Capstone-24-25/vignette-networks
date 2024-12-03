library(tidyverse)
library(rjson)
library(igraph)

# Function to get edges going into the ith node from .json file into dataframe
get_edges_in <- function(i, .json) {
  
  # Get list of edges going in
  from <- .json[[1]]$inList[[i]] 
  if (length(from) != 0) {
    from <- from + 1
  }
  
  # Create vector of target node
  to <- rep(i, length(from))
  
  # Get corresponding weights
  weight <- .json[[1]]$inWeight[[i]]
  
  # Bind to existing dataframe
  out <- data.frame(from = from, to = to, weight = weight)
  
  return(out)
}

# Function to convert Congress data from .json to dataframe
cnet_json_to_df <- function(.json) {
  out <- lapply(1:475, get_edges_in, .json = .json) %>%
    bind_rows()
  return(out)
}

# Function to create Congress igraph with node attributes from dataframe
create_cnet_igraph <- function(json_to_df.out) {
  cnet_node_attributes <- read_csv("data/congress_node_attributes.csv",
                                   show_col_types = FALSE)
  out <- graph_from_data_frame(json_to_df.out, 
                               directed = TRUE, vertices = cnet_node_attributes)
  
  return(out)
}
