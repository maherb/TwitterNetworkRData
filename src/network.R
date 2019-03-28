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
#' @param subset_query Query object to subset from. 
#' @return Subset of main data based on query and type.
getSubset <- function(data, subset_query)
{
  #print(subset_query)
  if(subset_query$colname %in% colnames(data))
  {
    contains <- vector(length = nrow(data))
    for(i in 1:length(contains))
    {
      if(subset_query$q %in% unlist(data[i, subset_query$colname]))
      {
        contains[i] = TRUE
      }
    }
    data_subset <- filter(data, contains)
  }
  else
  {
    data_subset <- NULL
  }
  data_subset
}

#' Gets subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries Query objects to subset from. MUST all have the same colname. 
#' @param colname Column name to subset from.
#' @return Subset of main data based on queries and type.
getSubsetColname <- function(data, subset_queries, colname)
{
  queries <- sapply(subset_queries, function(x){
    x$q
    })
  if(colname %in% colnames(data))
  {
    contains <- vector(length = nrow(data))
    for(i in 1:length(contains))
    {
      if(any(queries %in% unlist(data[i, colname])))
      {
        contains[i] = TRUE
      }
    }
    data_subset <- filter(data, contains)
  }
  else
  {
    data_subset <- NULL
  }
  data_subset
}

#' Gets intersection subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from. 
#' @return Subset of main data based on query and type.
getSubset2AND <- function(data, subset_queries)
{
  subset <- data
  for(i in 1:length(subset_queries))
  {
    subset <- getSubset(subset, subset_queries[[i]])
  }
  subset
}

#' Gets union subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from. 
#' @return Subset of main data based on query and type.
getSubset2OR <- function(data, subset_queries)
{
  subsets <- vector(mode = "list", length = length(subset_queries))
  for(i in 1:length(subsets))
  {
    subsets[[i]] <- getSubset(data, subset_queries[[i]])
  }
  subset <- do.call("rbind", subsets) %>%
    distinct()
  subset
}

#' Gets graph node from data.
#' 
#' @param data Dataframe with tweet data.
#' @param node_query NodeQuery object.
#' @return Dataframe of node data for graph. Node is located at (0,0).
getNode <- function(data, node_query)
{
  node_subset <- getSubset(data, node_query$query)
  if(is.null(node_subset)) 
  {
    node_value <- NA
  } 
  else 
  {
    node_value <- nrow(node_subset)
  }
  node <- data.frame(id = node_query$query$q,
                     label = node_query$name,
                     color = color.blue,
                     font = paste0("10px arial ", color.white),
                     value = node_value,
                     x = 0,
                     y = 0,
                     colname = node_query$query$colname,
                     colvalue = node_query$query$q,
                     hidden = is.na(node_value),
                     stringsAsFactors = FALSE)
  node
}

#' Updates the x and y values of each node.
#' 
#' @param nodes Dataframe of node data.
#' @returns Dataframe of node data with correct positions.
updatePositions <- function(nodes) {
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  for(i in 1:min(nrow(nodes), 12)) 
  {
    nodes$x[i] <- scale * radius * cos(angles[[i]])
    nodes$y[i] <- -scale * radius * sin(angles[[i]])
  }
  nodes
}

#' Gets nodes to graph in the network.
#' 
#'  @param data Dataframe with tweet data.
#'  @param node_queries List of NodeQuery objects.
#'  @return Node data frame.
getNodes <- function(data, node_queries)
{
  node_dfs <- vector(mode = "list", length = length(node_queries))
  for(i in 1:length(node_dfs))
  {
    node_dfs[[i]] = getNode(data, node_queries[[i]]) 
  }
  nodes <- do.call("rbind", node_dfs)
  nodes <- updatePositions(nodes)
  nodes
}

#' Gets edge subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param to_query Query representing the end node.
#' @param from_query Query representing the start node.
#' @param colname Column name to search in to create edges.
#' @return Subset of data containing rows from to_query, from_query that share common colname content.
getEdgeSubset <- function(data, to_query, from_query, colname)
{
  to_node_subset <- getSubset(data, to_query)
  from_node_subset <- getSubset(data, from_query)
  subset <- rbind(to_node_subset, from_node_subset)
  to_content <- unique(unlist(to_node_subset[ , colname]))
  from_content <- unique(unlist(from_node_subset[ , colname]))
  shared_content <- intersect(to_content, from_content)
  tmps <- vector(mode = "list", length = length(shared_content))
  if(length(shared_content > 0))
  {
    for(i in 1:length(tmps))
    {
      tmps[[i]] <- structure(list(q = shared_content[i], colname = colname),
                       class = "Query")
    }
  }
  edge_subset <- getSubsetColname(subset, tmps, colname)
  edge_subset
}

#' Gets single edge dataframe row.
#'
#' @param data Dataframe with tweet data.
#' @param to_query Query representing the start node.
#' @param from_query Query representing the end node.
#' @param colname Column name to search in to create edges.
#' @return Edge data frame to to_query from from_query.
getEdge <- function(data, to_query, from_query, colname)
{
  size <- 0
  edge_subset <- getEdgeSubset(data, to_query, from_query, colname)
  if(!is.null(nrow(edge_subset)))
  {
    size <- nrow(edge_subset)
  }
  edge <- data.frame(to = to_query$q,
                     from = from_query$q,
                     value = size)
  edge
}

#' Gets all graph node edge data from data.
#' 
#' @param data Dataframe with tweet data.
#' @param subset_queries List of query objects to subset from.  
#' @param edge_colname Column name to search in to create edges.
#' @returns Dataframe of edge data.
getEdges <- function(data, node_queries, edge_colname)
{
  subset_queries <- lapply(node_queries, function(x) {
    x$query
  })
  edges <- data.frame()
  node_combinations <- combn(1:length(subset_queries), 2)
  for(i in 1:ncol(node_combinations))
  {
    if(!is.na(subset_queries[[node_combinations[ ,i][1]]]) && !is.na(subset_queries[[node_combinations[ ,i][2]]]))
    {
      edges <- rbind(edges, getEdge(data,
                                    subset_queries[[node_combinations[ ,i][1]]],
                                    subset_queries[[node_combinations[ ,i][2]]],
                                    edge_colname))
    }
  }
  edges$id <- 1:nrow(edges)
  edges
}

#' Get visNetwork object for the floor.
#' 
#' @param nodes Node dataframe.
#' @param edges Edge dataframe.
getNetwork <- function(nodes, edges)
{
  nodes <- nodes[!is.na(nodes$id), ]
  visNetwork(nodes, edges) %>%
    visEdges(scaling = list("min" = 0), smooth = list("enabled" = TRUE)) %>%
    visNodes(scaling = list("min" = 10, "max" = 50)) %>%
    # After drawing the network, center on 0,0 to keep position
    # independant of node number
    visPhysics(stabilization = FALSE, enabled = FALSE) %>%
    visInteraction(dragView = FALSE, zoomView = FALSE) %>%
    visOptions(nodesIdSelection = TRUE) %>%
    visEvents(deselectEdge = "function(e) {Shiny.onInputChange('network_selected_e', '');}") %>%
    visEvents(selectEdge = "function(e) {if(e.nodes.length == 0){Shiny.onInputChange('network_selected_e', e.edges);}}")
}