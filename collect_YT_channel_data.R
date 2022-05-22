# Script to collect, clean, and save channel data

# Load required libraries
require(httr)
require(jsonlite)
require(dplyr)

keys <- c("[YOUR_API_KEYS_HERE]")

key_index <- 1

#Base Query variables
#key <- key3
base <- "https://www.googleapis.com/youtube/v3/"

# Get channel names

# Pull Channel ID for top 500 channels in YouTube search
#search_query <- "sail%7Csailing%7Csv"
search_type <- "channel"
max_results <- 1
time_after_base <- "-01-01T00%3A00%3A00Z"
time_before_base <- "-12-31T23%3A59%3A59Z"
channelIDs <- c()

webScrape <- read.csv("data/Sailing_Channel_Accounts_Cleaned.csv")

webScrape$Name <- gsub(" ", "%20", webScrape$Name)

for (channel in webScrape$Name) {
  
  #Perform a test query to see if a key maxed out its quota
  test_query <- paste0(base, "channels", "?", paste(paste0("key=", keys[key_index]),
                                                   paste0("id=", "UCaDdlHvrBqEi8LVWIRpo6sA"),
                                                   paste0("part=", "snippet"),
                                                   sep = "&"))
  api_test <- GET(test_query)
  
  #Clean and parse the output
  json_test <- content(api_test, "text", encoding = "UTF-8")
  test_result <- fromJSON(json_test, flatten = TRUE)
  
  if(!is.null(test_result$error)) {
    key_index <- key_index + 1
    
    message("key index incremented to: ", key_index)
  }
  
  #Query channelID for given channel name
  message("Querying ID for channel: ", channel)
  
  search_query <- channel
  
  #channelId_vector <- c()
  
  q_search_param <- paste(paste0("type=", search_type),
                          paste0("maxResults=", max_results),
                          paste0("order=", "relevance"),
                          paste0("q=", search_query),
                          #paste0("publishedAfter=", publishedAfter),
                          #paste0("publishedBefore=", publishedBefore),
                          paste0("part=", "snippet"),
                          sep = "&")
  
  #Perform the search query
  api_call_search <- paste0(base, "search", "?", paste0("key=", keys[key_index]), "&", q_search_param)
  api_result_search <- GET(api_call_search)
  
  #Clean and parse the output
  json_result_search <- content(api_result_search, "text", encoding = "UTF-8")
  result_search <- fromJSON(json_result_search, flatten = TRUE)
  
  #Extract the desired data
  result_search_items <- result_search$items
  
  #channelId_vector <- c(channelId_vector, result_search_items$snippet.channelId)
  
  
  channelIDs <- c(channelIDs, result_search_items$snippet.channelId)
}

#Dataframe to store channel data in
sailing_YT_channels <- data.frame()

#Query variables
parts <- paste("snippet", "statistics", "brandingSettings",
               sep = ",")

index <- 1
for (id in channelIDs) {
  message("Now querying channel index: ", index, "\r", appendLF = FALSE)
  index <- index + 1
  #Build the parameter list
  q_channels_param <- paste(paste0("key=", keys[18]),
                            paste0("id=", id),
                            paste0("part=", parts),
                            sep = "&")
  
  #Build query
  api_call_channels <- paste0(base, "channels", "?", q_channels_param)
  
  #Perform data query
  api_result_channels <- GET(api_call_channels)
  json_result_channels <- content(api_result_channels, "text", encoding = "UTF-8")
  
  #Read and clean raw input
  result_channels <- fromJSON(json_result_channels, flatten = TRUE)
  
  #Parse data
  channel_data <- result_channels$items
  
  if(is.null(channel_data)) {
    message("channel index: ", index, " is null\r", appendLF = FALSE)
    next
  }
  
  if(channel_data$statistics.hiddenSubscriberCount == TRUE) {
    channel_data$statistics.subscriberCount = NA
  }
  
  channel_data_selected <- data.frame(channel_data$id, 
                                      channel_data$snippet.title, 
                                      channel_data$snippet.description, 
                                      channel_data$snippet.publishedAt, 
                                      channel_data$statistics.viewCount, 
                                      channel_data$statistics.videoCount, 
                                      channel_data$statistics.subscriberCount, 
                                      channel_data$statistics.hiddenSubscriberCount)
  
  #Save to object
  sailing_YT_channels <- rbind(sailing_YT_channels, channel_data_selected)
}

#Clean the new data object
colnames(sailing_YT_channels) <- c("ID", "Title", "Description", "Join_Date", 
                                   "View_Count", "Video_Count", "Subscriber_Count",
                                   "Is_Subcriber_Count_Hidden")

sailing_YT_channels$View_Count <- as.numeric(sailing_YT_channels$View_Count)
sailing_YT_channels$Video_Count <- as.numeric(sailing_YT_channels$Video_Count)
sailing_YT_channels$Subscriber_Count <- as.numeric(sailing_YT_channels$Subscriber_Count)

sailing_YT_channels$Join_Date <- gsub("T.*", "", sailing_YT_channels$Join_Date)
sailing_YT_channels$Join_Date <- as.Date(sailing_YT_channels$Join_Date,
                                         format = "%Y-%m-%d")

#Removing records that are obvious not sailing channels
sailing_YT_channels <- sailing_YT_channels[c(-243, -105, -132, -47, -196, -269),]

#Adding in ommitted channels
ommitted_names <- c("Harbors%20Unknown", "Adventures%20of%20Two%20Afloat", "Onboard%20Lifestyle")

omitted_channelIDs <- c()

for (channel in ommitted_names) {
  
  #Perform a test query to see if a key maxed out its quota
  test_query <- paste0(base, "channels", "?", paste(paste0("key=", keys[key_index]),
                                                    paste0("id=", "UCaDdlHvrBqEi8LVWIRpo6sA"),
                                                    paste0("part=", "snippet"),
                                                    sep = "&"))
  api_test <- GET(test_query)
  
  #Clean and parse the output
  json_test <- content(api_test, "text", encoding = "UTF-8")
  test_result <- fromJSON(json_test, flatten = TRUE)
  
  if(!is.null(test_result$error)) {
    key_index <- key_index + 1
    
    message("key index incremented to: ", key_index)
  }
  
  #Query channelID for given channel name
  message("Querying ID for channel: ", channel)
  
  search_query <- channel
  
  #channelId_vector <- c()
  
  q_search_param <- paste(paste0("type=", search_type),
                          paste0("maxResults=", max_results),
                          paste0("order=", "relevance"),
                          paste0("q=", search_query),
                          #paste0("publishedAfter=", publishedAfter),
                          #paste0("publishedBefore=", publishedBefore),
                          paste0("part=", "snippet"),
                          sep = "&")
  
  #Perform the search query
  api_call_search <- paste0(base, "search", "?", paste0("key=", keys[key_index]), "&", q_search_param)
  api_result_search <- GET(api_call_search)
  
  #Clean and parse the output
  json_result_search <- content(api_result_search, "text", encoding = "UTF-8")
  result_search <- fromJSON(json_result_search, flatten = TRUE)
  
  #Extract the desired data
  result_search_items <- result_search$items

  omitted_channelIDs <- c(omitted_channelIDs, result_search_items$snippet.channelId)
}

omitted_channels <- data.frame()

index <- 1
for (id in omitted_channelIDs) {
  message("Now querying channel index: ", index, "\r", appendLF = FALSE)
  index <- index + 1
  #Build the parameter list
  q_channels_param <- paste(paste0("key=", keys[18]),
                            paste0("id=", id),
                            paste0("part=", parts),
                            sep = "&")
  
  #Build query
  api_call_channels <- paste0(base, "channels", "?", q_channels_param)
  
  #Perform data query
  api_result_channels <- GET(api_call_channels)
  json_result_channels <- content(api_result_channels, "text", encoding = "UTF-8")
  
  #Read and clean raw input
  result_channels <- fromJSON(json_result_channels, flatten = TRUE)
  
  #Parse data
  channel_data <- result_channels$items
  
  if(is.null(channel_data)) {
    message("channel index: ", index, " is null\r", appendLF = FALSE)
    next
  }
  
  if(channel_data$statistics.hiddenSubscriberCount == TRUE) {
    channel_data$statistics.subscriberCount = NA
  }
  
  channel_data_selected <- data.frame(channel_data$id, 
                                      channel_data$snippet.title, 
                                      channel_data$snippet.description, 
                                      channel_data$snippet.publishedAt, 
                                      channel_data$statistics.viewCount, 
                                      channel_data$statistics.videoCount, 
                                      channel_data$statistics.subscriberCount, 
                                      channel_data$statistics.hiddenSubscriberCount)
  
  #Save to object
  omitted_channels <- rbind(omitted_channels, channel_data_selected)
}

#Clean the new data object
colnames(omitted_channels) <- c("ID", "Title", "Description", "Join_Date", 
                                   "View_Count", "Video_Count", "Subscriber_Count",
                                   "Is_Subcriber_Count_Hidden")

omitted_channels$View_Count <- as.numeric(omitted_channels$View_Count)
omitted_channels$Video_Count <- as.numeric(omitted_channels$Video_Count)
omitted_channels$Subscriber_Count <- as.numeric(omitted_channels$Subscriber_Count)

#omitted_channels$Join_Date[1] <- "2020-05-19T00:49:11.1234Z"

omitted_channels$Join_Date <- gsub("T.*", "", omitted_channels$Join_Date)
omitted_channels$Join_Date <- as.Date(omitted_channels$Join_Date,
                                         format = "%Y-%m-%d")

sailing_YT_channels <- rbind(sailing_YT_channels, omitted_channels)

sailing_YT_channels$Omitted <- FALSE

sailing_YT_channels$Omitted[c(458, 459, 460)] <- TRUE

write.csv(sailing_YT_channels, "data/sailing_channel_YT_data.csv",
          row.names = FALSE)
