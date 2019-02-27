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

query1 <- createNodeQuery("Ferguson", "hashtags", "Ferguson")
query2 <- createNodeQuery("ferguson", "hashtags", "ferguson")
subset <- getSubset(data, query1$query)

subsets <- getSubset2(data, list(query1$query, query2$query))

node1 <- getNode(data, query1)