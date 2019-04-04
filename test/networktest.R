library(tidyverse)
library(visNetwork)

source("src/network.R")

createNodeQuery <- function(q, colname, name)
{
  query <- structure(list(q = q, colname = colname), class = "Query")
  node_query <- structure(list(query = query, name = name), class = "NodeQuery")
  node_query
}

color.blue <- "#1D8DEE"

parsed_json <- fromJSON("test/test_ALLGROUPS.json", nullValue = NA, simplify = FALSE)
data <- fetchData(parsed_json$data_file)
data$test <- sample(1:10, nrow(data), replace = T)
edge_colname <- parsed_json$edge_colname
nodes <- getNodes(data, parsed_json$nodes)
edges <- getEdges(data, parsed_json$nodes, edge_colname)
edges$color <- "black"
edges2 <- getEdges(data, parsed_json$nodes, "test")
edges2$color <- "red"
edges <- rbind(edges, edges2)
edges$id <- 1:nrow(edges)
edges$smooth <- rep(list(list("type" = "continuous")), nrow(edges))
for(i in 1:nrow(edges))
{
  rounds = c(0, .5)
  index = (1:2)[edges$color[[i]] == unique(edges$color)]
  edges$smooth[[i]][["roundness"]] = rounds[index]
}
View(edges)
getNetwork(nodes, edges)