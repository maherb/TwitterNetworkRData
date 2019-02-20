#' Fetches tweet from database.
#'
#' @param Rdata_file Path to an Rdata file.
#' @param name Name of the dataframe the Rdata file contains
#' @return Dataframe of tweet data.
fetchData <- function(Rdata_file) {
  df_name <- load(Rdata_file)
  data <- eval(parse(text = df_name))
  # Convert hashtag column to better format
  data$hashtags <- lapply(data$hashtags, function(x) {
    unlist(x)
  })
  remove(df_name)
  return(data)
}

#' Gets subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_query String to base subset from.
#' @param subset_type String type of query. Can be hashtag, screen_name, mentions_screen_name, word. 
#' @return Subset of main data based on query and type.
getSubset <- function(data, subset_query, subset_type) {
  # subset_query <- toupper(subset_query)
  assert_that(nrow(data) > 0)
  subset_type <- toString(subset_type)
  switch(subset_type,
    "hashtag" = data_subset <- filter(data, toupper(data$hashtags) %in% subset_query),
    "screen_name" = data_subset <- filter(data, toupper(data$user_screen_name) %in% subset_query),
    "mentions_screen_name" = data_subset <- filter(data, toupper(data$mentions_screen_name) %in% subset_query),
    "group" = data_subset <- data[data$group == subset_query, ],
    #TODO: text processing
    data_subset <- NULL
  )
  return(data_subset)
}

# TODO: May need to change function, not required currently.
#' Gets subset of data from multiple queries.
#'
#' @param data Dataframe with tweet data.
#' @param subset_query Vector of strings to base subset on.
#' @param subset_type Vector of string type of query. Can be hashtags, screen_name, mentions_screen_name, word. 
#' @return Subset of main data based on queries and types.
getSubset2 <- function(data, subset_queries, subset_types) {
  subsets <- vector("list", length = length(subset_queries))
  for(i in 1:length(subsets)) {
    subsets[[i]] <- getSubset(data, subset_queries[i], subset_types[i])
  }
  subset <- do.call("rbind", subsets) %>%
    distinct()
  return(subset)
}

#' Gets graph node from data.
#' 
#' @param data Dataframe with tweet data.
#' @param node_query String to base node off of.
#' @param node_type Type of the node. Can be hashtags, screen_name, mentions_screen_name, text.
#'                    Also may be NA. 
#' @param node_name Name of the node.
#' @return Dataframe of node data for graph. Node is located at (0,0).
getNode <- function(data, node_query, node_type, node_name) {
  node_subset <- getSubset(data, node_query, node_type)
  if(is.null(node_subset)) {
    node_value <- NA
  } else {
    node_value <- nrow(node_subset)
  }
  node <- data.frame(id = node_query,
                     label = node_name,
                     color = color.blue,
                     font = "10px arial #fd7e14",
                     value = node_value,
                     x = 0,
                     y = 0,
                     type = node_type,
                     hidden = is.na(node_value),
                     stringsAsFactors = FALSE)
  return(node)
}

#' Updates the x and y values of each node.
#' 
#' @param nodes Single dataframe of node data.
#' @param nodes_list list of node dataframes.
#' @returns Dataframe of node data with correct positions.
updatePositions <- function(nodes, nodes_list) {
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  for(i in 1:min(length(nodes_list), 12)) {
    if(!is.null(nodes_list[[i]])) {
      nodes$x[i] <- scale * radius * cos(angles[[i]])
      nodes$y[i] <- -scale * radius * sin(angles[[i]])
    }
  }
  return(nodes)
}

#' Joins together list of nodes into one dataframe.
#' 
#' @param nodes_list list of node dataframes to join in their list order.
#' @return Node dataframes joined into one dataframe.
joinNodes <- function(nodes_list) {
  nodes <- do.call("rbind", nodes_list)
  nodes <- updatePositions(nodes, nodes_list)
  return(nodes)
}

#' Gets nodes to graph in the network.
#' 
#'  @param data Dataframe with tweet data.
#'  @param node_queries Vector of queries to base nodes from.
#'  @param node_types Vector of node types to base nodes from.
#'  @param node_names Vector of node names to name nodes.
#'  @return Node data frame.
getNodes <- function(data, node_queries, node_types, node_names) {
  assert_that(length(node_names) == length(node_queries))
  assert_that(length(node_queries) == length(node_types))
  nodes_list <- vector(mode = "list", length = length(node_queries))
  for(i in 1:length(node_queries)) {
    nodes_list[[i]] = getNode(data, node_queries[i], node_types[i], node_names[i]) 
  }
  return(joinNodes(nodes_list))
}

#' Gets graph node edge between two specific nodes
#' 
#' @param data Dataframe with tweet data.
#' @param to_node Node information where the edge terminates. Row of nodes dataframe.
#' @param from_node Node information where the edge originates. Row of nodes dataframe.
#' @param edge_type Type of the edge. Can be hashtags, tweet, screen_name, mentions_screen_name, text. 
#' @returns Dataframe of one edge.
getEdge <- function(data, to_node, from_node, edge_type) {
  to_node_subset <- getSubset(data, to_node$id, to_node$type)
  from_node_subset <- getSubset(data, from_node$id, from_node$type)
  switch(edge_type,
        "hashtag" = {
          to_node_hashtags = unique(unlist(to_node_subset$hashtags))
          from_node_hashtags = unique(unlist(from_node_subset$hashtags))
          size = length(intersect(to_node_hashtags, from_node_hashtags))
        },
        "tweet" = {
          to_node_tweets = unique(to_node_subset$id_str)  
          from_node_tweets = unique(from_node_subset$id_str)
          size = length(intersect(to_node_tweets, from_node_tweets))
        },
        size = 0
        #TODO: MORE TYPES 
  )
  edge <- data.frame(to = to_node$id,
                     from = from_node$id,
                     value = size)
  return(edge)
}

#' Gets graph node edge data from data.
#' 
#' @param data Dataframe with tweet data.
#' @param nodes Dataframe of node data.
#' @param edge_type Type of the edge. Can be hashtags, screen_name, mentions_screen_name, text.
#' @returns Dataframe of edge data from ONE specific node.
getEdges <- function(data, nodes, edge_type) {
  edges <- data.frame()
  # Take NA nodes out, they interfere with creating edges in the below code.
  nonNA_nodes <- nodes[!is.na(nodes$id), ]
  node_combinations = combn(nonNA_nodes$id, 2)
  for(i in 1:ncol(node_combinations)) {
    edges <- rbind(edges, getEdge(data,
                                  nonNA_nodes[nonNA_nodes$id == node_combinations[ ,i][1], ],
                                  nonNA_nodes[nonNA_nodes$id == node_combinations[ ,i][2], ],
                                  edge_type))
  }
  return(edges)
}

#' Get visNetwork object for the floor.
#' 
#' @param nodes Dataframe of node data.
#' @param edges Dataframe of edge data.
getNetwork <- function(nodes, edges) {
  visNetwork(nodes, edges) %>%
    visEdges(scaling = list("min" = 0), smooth = list("enabled" = TRUE)) %>%
    visNodes(scaling = list("min" = 10, "max" = 50)) %>%
      # After drawing the network, center on 0,0 to keep position
      # independant of node number
      visPhysics(stabilization = FALSE, enabled = FALSE) %>%
      visInteraction(dragView = FALSE, zoomView = FALSE)
}