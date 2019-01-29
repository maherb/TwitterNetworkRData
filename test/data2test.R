source("src/data2.R")

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

# node1 <- getNode(data, hashtags[1], "hashtag", "name")
# node2 <- getNode(data, screen_names[1], "screen_name", "name2")
# nodes_list <- list(node1, node2)
# nodes <- joinNodes(nodes_list)