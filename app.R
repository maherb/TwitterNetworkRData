library(shiny)
library(visNetwork)
library(tidyverse)
library(ggplot2)
library(useful)
library(assertthat)
library(RJSONIO)

source("app-only-auth-twitter.R")
source("src/floor.R")
source("src/wall.R")
source("src/network.R")
source("src/external-monitor.R")
source("src/utilities.R")
source("src/campfire_lib.R")

campfireApp(
  
  controller = div(
    h1("Controller"),
    fileInput("json_file", "JSON Input", accept = c("application/json")),
    textAreaInput("text_input", "JSON Text", height = '200px'),
    # selectInput(inputId = "edge_type",
    #             label = "Edge Type:",
    #             choices = list("hashtag", "tweet"),
    #             selected = "hashtag"
    #             ),
    actionButton(inputId = "update",
                  label = "Update"),
    style = "position: absolute; 
    top: 50%; left: 50%; 
    margin-right: -50%; 
    transform: translate(-50%, -50%)"
  ),
  
  wall = div(
    uiOutput("wall_ui"),
    style = paste0("background: ", color.back, "; overflow: hidden;",
                   "height: 665px")
  ),
  
  floor = div(
    visNetworkOutput("network", width = "1000px", height = "1000px"),
    style = paste0("position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: ", color.back,
           "; height: 1000px; overflow: hidden")
  ),
  
  datamonitor = div(fluidPage(
    fluidRow(
      column(12,
            uiOutput("tweets_info")
      )
    )),
    fluidRow(
      column(6,
             plotOutput("top.users.bar.extern", height = "920px")
             ),
      column(6,
             plotOutput("top.hashtags.bar.extern", height = "920px")
             )
    ),
    style = paste0("background: ", color.back, ";
                   overflow: hidden;
                   height: 1080px")
  ),
  
  urlmonitor = div(fluidPage(
    htmlOutput("frame")
  )),
  
  serverFunct = function(ServerValues, output, session) {
    
    # Update text box when JSON file changes.
    observeEvent(ServerValues$json_file, {
      text <- read_file(ServerValues$json_file$datapath)
      updateTextInput(session, "text_input", value = text)
    })


    # render the floor
    output$network <- renderVisNetwork({
      if (!is.null(ServerValues$network)) {
        ServerValues$network
      }
    })
    
    output$tweets_info <- renderUI(
    {
      if(ServerValues$network_selected != "")
      {
        node_name <- ServerValues$nodes$label[ServerValues$nodes$id == ServerValues$network_selected]
        node_name <- node_name[!is.na(node_name)]
        node_size <- nrow(ServerValues$data_subset)
        tags$div(
          tags$h1(style = paste0("color:", color.blue), node_name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", node_size)))
      }
      else if(ServerValues$network_selected_e != "")
      {
        edge_data <- ServerValues$edges[ServerValues$network_selected_e, ]
        to_node_name <- ServerValues$nodes$label[ServerValues$nodes$id == edge_data$to]
        to_node_name <- to_node_name[!is.na(to_node_name)]
        from_node_name <- ServerValues$nodes$label[ServerValues$nodes$id == edge_data$from]
        from_node_name <- from_node_name[!is.na(from_node_name)]
        edge_name <- paste(to_node_name, "AND", from_node_name)
        edge_size <- nrow(ServerValues$data_subset)
        tags$div(
          tags$h1(style = paste0("color:", color.blue), edge_name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", edge_size)))
      }
      else
      {
        tags$div(
          tags$h1(style = paste0("color:", color.blue), "Twitter Time Machine"),
          tags$h2(style = paste0("color:", color.blue), paste("Total number of tweets:", nrow(ServerValues$data))))
      }
    })
    
    output$wall_ui <- renderUI({
       fluidPage(
         tags$script(HTML(
          "$(document).on('click', '.clickable', function () {
              var text =  $(this).text();
              Shiny.onInputChange('clicked_text', text);
            });"
        )),
        fluidRow(
          lapply(1:12, function(col.num) {
            ServerValues$col_list[[col.num]]
          })
        )
      )
    })
    # 
    # output$top.users.bar.extern <- renderPlot({
    #   serverValues$monitor.domain <- getDefaultReactiveDomain()
    #   if(!is.null(serverValues$data_subset)) {
    #     serverValues$data_subset %>% 
    #       count(screen_name) %>% 
    #       arrange(desc(n)) %>%
    #       slice(1:10) %>%
    #       ggplot(aes(reorder(screen_name, n), n)) + 
    #         geom_col(fill = color.blue, color = color.blue) + 
    #         coord_flip() + 
    #         labs(x = "Screen Name", y = "Tweets", title = "Top 10 Users") + 
    #         theme_dark() +
    #         theme(plot.background = element_rect(fill = color.back, color = NA),
    #               axis.text = element_text(size = 20, colour = color.white),
    #               text = element_text(size = 20, colour = color.blue))
    #   } else {
    #     serverValues$data %>%
    #       count(query) %>%
    #       ggplot(aes(reorder(query, n), n)) +
    #         geom_col(fill = color.blue, color = color.blue) +
    #         coord_flip() +
    #         labs(x = "Query", y = "Number of Tweets", title = "Tweet Composition") +
    #         theme_dark() +
    #         theme(panel.border = element_blank(),
    #             plot.background = element_rect(fill = "#151E29", color = NA),
    #             axis.text = element_text(size = 20, colour = "#f0f0f0"),
    #             text = element_text(size = 20, colour = "#1D8DEE"))
    #   }
    # })
    # 
    # output$top.hashtags.bar.extern <- renderPlot({
    #   if(!is.null(serverValues$data_subset)) {
    #     serverValues$data_subset %>%
    #       filter(!is.na(hashtags)) %>%
    #       unnest(hashtags) %>%
    #       mutate(hashtags = toupper(hashtags)) %>%
    #       filter(!(paste("#", hashtags, sep = "") %in% toupper(unique(serverValues$data$query)))) %>%
    #       count(hashtags) %>%
    #       arrange(desc(n)) %>%
    #       slice(1:10) %>%
    #       ggplot(aes(reorder(hashtags, n), n)) +
    #         geom_col(fill = color.blue, color = color.blue) +
    #         coord_flip() +
    #         labs(x = "Hashtag", y = "Frequency", title = "Top 10 Hashtags") +
    #         theme_dark() +
    #         theme(panel.border = element_blank(),
    #               plot.background = element_rect(fill = color.back, color = NA),
    #               axis.text = element_text(size = 20, colour = color.white),
    #               text = element_text(size = 20, colour = color.blue))
    #   } else {
    #     serverValues$data %>% 
    #       distinct(screen_name, source) %>%
    #       count(source) %>% 
    #       filter(n >= 5) %>%
    #       ggplot(aes(reorder(source, n), n)) + 
    #         geom_col(fill = color.blue, color = color.blue) +
    #         coord_flip() + 
    #         labs(x = "Source", y = "Tweets", title = "Tweets by source", subtitle = "sources with >=5 tweets") +
    #         theme_dark() +
    #         theme(panel.border = element_blank(),
    #             plot.background = element_rect(fill = color.back, color = NA),
    #             axis.text = element_text(size = 20, colour = color.white),
    #             text = element_text(size = 20, colour = color.blue))
    #   }
    #   
    # })
    # 
    # output$frame <- renderUI({
    #   if(!is.null(serverValues$url)) {
    #     redirectScript <- paste0("window = window.open('", serverValues$url, "');")
    #     tags$script(HTML(redirectScript))
    #   } else {
    #     redirectScript <- paste0("window = window.open('", "https://docs.google.com/presentation/d/1g_q5qQTJAt4jVekozFlEsnEo4XdveubVzLC2t9aeWlo/present", "');")
    #     tags$script(HTML(redirectScript))
    #   }
    # })
    # 
    # observeEvent(serverValues$current_node_id, {
    #   visNetworkProxy("network") %>%
    #     visSelectNodes(serverValues$current_node_id)
    # })
    
  }
)
