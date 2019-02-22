# Only call when update on controller is pressed, update every column on the
# wall sequentially. There will only be gaps at the end
updateWall <- function(data, nodes) {
  # Creates the output displayed on the campfire wall of tweet columns.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   nodes: Data frame containing node data formatted for visnetwork.
  #
  # Returns:
  #   List of Shiny HTML columns containing tweet data.
  col_list <- vector("list", 12)
  col_list <- lapply(1:12, function(col_num) {
    if(!(col_num %in% nodes$position_)) {
      column(width = 1,
             textInput(paste0("text.column.", col_num), label = ""),
             actionButton(paste0("button.column.", col_num), "Submit"))
    } else {
      current_node_data <- nodes[nodes$position_ == col_num, ]
      data_subset <- getDataSubset(data, current_node_data$id)
      UpdateColumn(data_subset, current_node_data, nodes$id)
    }
  })
}

UpdateColumn <- function(data_subset, current_node_data, queries) {
  # Creates a single Shiny HTMl column containing tweet data for specific single query.
  # 
  # Args:
  #   data_subset: Tibble of tweet data subset into a single search query.
  #   current_node_data: Row of nodes data frame with the matching current search query.
  #   queries: Vector of strings to be used to search for tweets. Queries are NOT allowed to be NA.
  #
  # Returns:
  #   List of Shiny html columns containing tweet data.
  header_text <- getGroupName(5, as.integer(current_node_data$label))
  column(width = 1,
         tags$div(includeCSS("wall.css"),
                  fluidRow(
                    tags$h2(tags$span(class = "clickable", header_text))
                  ),
                  fluidRow(style = 'height: 600px;
                  overflow-y: auto;
                  overflow-x: hidden;',
                           if(nrow(data_subset) > 0) {
                             lapply(1:nrow(data_subset), function(tweet_num) {
                              tweetText <- data_subset$full_text[tweet_num]
                              tweetHashtags <- unlist(data_subset$hashtags[tweet_num])
                              tweetUrls <- unlist(data_subset$urls[tweet_num])
                              #tweetMentions <- ????
                              coloredText <- highlightHashtags(tweetText, tweetHashtags)
                              coloredText <- highlightUrls(coloredText, tweetUrls)
                              #coloredText <- highlightMentions(coloredText, tweetMentions)
                              tags$div(style = 'padding: 0px;',
                                        tags$h3(tags$span(class = "clickable", paste0("@", data_subset$user_screen_name[tweet_num]))),
                                       tags$p(HTML(coloredText)),
                                       tags$p(HTML(paste("&#x1F499", data_subset$favorite_count[tweet_num], "&#x1F504", data_subset$retweet_count[tweet_num])))
                               )
                             })
                           }
                  )
         )
  )
}

highlightMentions <- function(string, mentions) {
  mentions <- mentions[order(nchar(mentions), mentions, decreasing = TRUE)]
  for(mention in mentions) {
    link <- a(mention, href = paste0("https://twitter.com/", mention))
    replacement <- paste0('<span class="clickable mentionincluded">', paste0("@&", link), '</span>')
    string <- str_replace_all(string, paste0("@", mention), replacement)
  }
  string <- str_replace_all(string, "@&", "@")
  return(string)
}

highlightHashtags <- function(string, hashtags) {
  if (!is.null(hashtags)) {
    hashtags <- hashtags[order(nchar(hashtags), hashtags, decreasing = TRUE)]
    for(hashtag in hashtags) {
      link <- a(hashtag, href = paste0("https://twitter.com/hashtag/", hashtag), style="color:inherit")
      replacement <- paste0('<span class="clickable included">', paste0("#&", link), '</span>')
      string <- str_replace_all(string, paste0("#", hashtag), replacement)
    }
    string <- str_replace_all(string, "#&", "#")
  }
  return(string)
}

highlightUrls <- function(string, urls) {
  if (!is.null(urls)) {
    for(url in urls) {
      if(!is.na(url)) {
        link <- a(url, href = url)
        replacement <- paste0('<span class="clickable url">', link, '</span>')
        string <- str_replace_all(string, url, replacement)
      }
    }
  }
  return(string)
}