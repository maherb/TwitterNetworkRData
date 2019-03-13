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

load("data/period_5.df.Rdata")
data <- fetchData("data/period_5.df.Rdata")

query1 <- createNodeQuery("ferguson", "hashtags", "ferguson")
query2 <- createNodeQuery("Ferguson", "hashtags", "Ferguson")
query3 <- createNodeQuery("LAPD", "hashtags", "LAPD")
query4 <- createNodeQuery("1", "group", "group1")

subsets <- getSubset2OR(data, list(query1$query, query2$query))

node1 <- getNode(data, query1)

nodes <- getNodes(data, list(query1, query2))

edge <- getEdge(data, query1$q, query2$q, "group")

edges <- getEdges(data, list(query1$q, query2$q, query3$q), "group")

getNetwork(data, list(query1, query2, query3, query4), "group")