# This script takes the raw excel sheet and cleans the data

#Load the data from the excel sheet
library(readxl)
sailing_channels_raw <- read_excel("data/SAILING CHANNELS.xlsx", 
                               col_names = FALSE)

head(sailing_channels_raw, n = 15)

#Remove Empty records
sailing_channels <- sailing_channels_raw[!is.na(sailing_channels_raw),]

head(sailing_channels, n = 15)

#Replace records that have "SUBSRIBED" or "SUBSCRIBE" with "BREAK"
sailing_channels[sailing_channels == "SUBSCRIBED"] <- "BREAK"
sailing_channels[sailing_channels == "SUBSCRIBE"] <- "BREAK"

head(sailing_channels, n = 15)

#Rearrange to make an actual table
library(tidyverse)
library(dplyr)

sailing_channels <- sailing_channels_copy

channels_clean <- data.frame(c("Name"), c("Stats"), c("Description"), c("BREAK"))
colnames(channels_clean) <- c("Name", "Stats", "Description", "BREAK")
channels_clean <- channels_clean[-1,]

while(nrow(sailing_channels) >= 0) {
  #Get first row where that contains "break" 
  break_index <- which(sailing_channels == "BREAK")
  break_index <- break_index[1]
  
  if(break_index > 4) {
    break_index <- 3
  }
  
  #Retrieve top rows into a mini data frame
  channel <- slice(sailing_channels, 1:break_index)
  
  #Remove those rows from the source data frame
  sailing_channels <- sailing_channels[-1:-break_index,]
  
  #Pivot table into columns instead of rows
  channel <- as.matrix(channel)
  channel <- t(channel)
  
  if(ncol(channel) < 4 && ncol(channel) >= 3) {
    channel <- cbind(channel, "BREAK")
    if(channel[1,3] == "BREAK") {
      channel[1,3] <- ""
    }
  }
  
  channel <- as.data.frame(channel)
  colnames(channel) <- c("Name", "Stats", "Description", "BREAK")
  
  #Add rows to to clean data set
  channels_clean <- rbind(channels_clean, channel)
}

channels_clean <- channels_clean[,-4]

row.names(channels_clean) <- c(1:nrow(channels_clean))

#Clean the stats columns
library(stringr)
head(channels_clean)

#Get Subscriber count
Subscribers <- sub("subscribe.*", "", channels_clean$Stats)
Subscribers[str_which(Subscribers, "video")] <- NA
Subscribers[str_which(Subscribers, "K")] <- as.numeric(sub("K", "", Subscribers[str_which(Subscribers, "K")])) * 1000
Subscribers[str_which(Subscribers, "M")] <- as.numeric(sub("M", "", Subscribers[str_which(Subscribers, "M")])) * 1000000
Subscribers <- as.numeric(Subscribers)
channels_clean <- cbind(channels_clean, Subscribers)

#Get video count
Videos <- str_extract(channels_clean$Stats, regex("\\d+ video"))
Videos <- as.numeric(sub(" video", "", Videos))
channels_clean <- cbind(channels_clean, Videos)

#Final clean up of the data frame
channels_clean <- select(channels_clean, Name, Subscribers, Videos, Description)

write_csv(channels_clean, "data/Sailing_Channel_Accounts_Cleaned.csv")

