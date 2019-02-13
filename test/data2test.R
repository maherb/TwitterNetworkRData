source("src/data2.R")

color.blue <- "#1D8DEE"

load("data/period_5.df.Rdata")
data <- period_5.df

data$hashtags <- lapply(data$hashtags, function(x) {
  unlist(x)
})

hashtags <- unique(unlist(data$hashtags))
screen_names <- unique(unlist(data$user_screen_name))
#mentions <- unique(unlist(data$mentions_screen_name))
#text <-

# subset1 <- getSubset(data, hashtags[1], "hashtag")
# subset2 <- getSubset(data, screen_names[1], "screen_name")
# subset3 <- getSubset2(data, c(hashtags[1], screen_names[1]), c("hashtag", "screen_name"))

node1 <- getNode(data, 1, "group", "name")
node2 <- getNode(data, 2, "group", "name2")
node3 <- getNode(data, 3, "group", "name3")
nodes_list <- list(node1, node2, node3)
nodes <- joinNodes(nodes_list)
edges <- getEdges(data, nodes, "hashtag")
print(edges)
network <- getNetwork(nodes, edges)