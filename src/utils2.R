#' Parse the text from the controller textbox, and return a list of node lists.
#'   Node lists are in the form: list(query=..., type=.., name=..)
#'   
#' @param raw_query String of the form:
#'                    'query=..., type=..., name=...' '...' ...
#' @return List of node lists.
parseTextQuery <- function(raw_query) {
  split_queries <- scan(text = raw_query, what = "character", quiet=TRUE)
  node_lists <- vector("list", length = length(split_queries))
  for(i in 1:length(split_queries)) {
    node_list <- as.list(strsplit(split_queries[i], ', ')[[1]])
    names(node_list) <- c("query", "type", "name")
    node_lists[[i]] <- node_list
  }
  return(node_lists)
}