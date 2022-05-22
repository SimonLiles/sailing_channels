#Exploring Sailing Channel Account Data from YouTUbe Data API

#Read in data from collection and cleaning script
channels <- read.csv("data/sailing_channel_YT_data.csv")

channels <- unique(channels)

#Little bit of cleaning
channels$Join_Date <- as.Date(channels$Join_Date)

#Adding derived variables
channels$View_Per_Video <- channels$View_Count / channels$Video_Count
channels$View_Per_Subscriber <- channels$View_Count / channels$Subscriber_Count
channels$Subscriber_Per_Video <- channels$Subscriber_Count / channels$Video_Count

today <- Sys.Date()
channels$Videos_Per_Year <- channels$Video_Count / (as.numeric(difftime(today, channels$Join_Date, 
                                                                       units = "weeks")) / 52)
#channels$View_Per_Video_Per_Week <- (channels$View_Per_Video) / as.numeric(difftime(today, channels$Join_Date, 
#                                                                                units = "weeks"))

summary(channels)

#How many above the median subscriber count?
nrow(channels[channels$Subscriber_Count >= median(channels$Subscriber_Count, na.rm = TRUE),])

#How many above the mean subscriber count?
nrow(channels[channels$Subscriber_Count >= mean(channels$Subscriber_Count, na.rm = TRUE),])

#Play with plots
library(ggplot2)
library(ggthemes)
library(ggExtra)
library(viridis)

#Subscribers plotted against video count with axes adjusted by log10 
video_x_sub_plot <- ggplot(channels, aes(Video_Count, Subscriber_Count, color = Join_Date)) + 
  geom_point(alpha = 0.75) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = median(channels$Subscriber_Count, na.rm = TRUE)) + 
  geom_vline(xintercept = median(channels$Video_Count)) + 
  scale_color_viridis(trans = "date") +
  ggtitle("Sailing Channel Subscribers per given Video Count") + 
  theme(legend.position = "left")
video_x_sub_plot <- ggMarginal(video_x_sub_plot, type = "density")
video_x_sub_plot

#channels[which(channels$Video_Count <= 10 & channels$Subscriber_Count >= 500),c(2,4:7)]

#channels[which(channels$Subscriber_Count >= 500000),c(2,4:7)]

#channels[which(channels$Videos >= 500 & channels$Subscribers <= 500000),c(2,4:7)]

#Number of Channels created by Time
channels_over_time_plot <- ggplot(channels, aes(Join_Date)) + 
  geom_histogram(fill = "blue", color = "black") + 
  ggtitle("Number of New Channels per Time")
channels_over_time_plot

#Distribution of upload frequency
upload_freq_plot <- ggplot(channels, aes(Videos_Per_Year)) + 
  geom_histogram(fill = "darkblue", color = "grey") + 
  ggtitle("Distribution of Average Videos Uploaded per Year")
upload_freq_plot

#Distribution of Views per Video
View_Per_Video_hist <- ggplot(channels, aes(View_Per_Video)) +
  geom_histogram(fill = "lightblue", color = "darkgrey") + 
  ggtitle("Distribution of Average Views per Video")
View_Per_Video_hist

Sub_Per_Video_hist <- ggplot(channels, aes(Subscriber_Per_Video)) +
  geom_histogram(fill = "lightblue", color = "darkgrey") + 
  ggtitle("Distribution of Average New Subscribers per Video")
Sub_Per_Video_hist

#Plot of Subscribers per View Per Video
ViewPerVideo_x_Subscribers_plot1 <- ggplot(channels, aes(Subscriber_Count, View_Per_Video,
                                                         color = Join_Date)) +
  geom_point(alpha = 0.9) + 
  scale_x_log10() + 
  scale_y_log10() + 
  scale_color_viridis(trans = "date") +
  ggtitle("Channel Performance, View per Video x Subscriber Count, log 10 scales") + 
  theme(legend.position = "left")
ViewPerVideo_x_Subscribers_plot1 <- ggMarginal(ViewPerVideo_x_Subscribers_plot1, type = "density")
ViewPerVideo_x_Subscribers_plot1

ViewPerVideo_x_Subscribers_plot2 <- ggplot(channels, aes(Subscriber_Count, View_Per_Video)) +
  geom_point(alpha = 0.5) +  
  ggtitle("Channel Performance, View per Video x Subscriber Count, normal scales")
ggMarginal(ViewPerVideo_x_Subscribers_plot2, type = "density")

ViewPerVideo_x_Subscribers_plot3 <- ggplot(channels, aes(Subscriber_Count, View_Per_Video,
                                                         color = Join_Date)) +
  geom_point(alpha = 0.9) + 
  scale_x_log10() + 
  scale_y_log10() + 
  scale_color_viridis(trans = "date") +
  geom_smooth(method = "lm") +
  #geom_segment(aes(x = 1, xend = 1000000, y = 3, yend = 300000), color = "blue") + 
  ggtitle("Channel Performance, View per Video x Subscriber Count, log 10 scales") + 
  theme(legend.position = "left")
ViewPerVideo_x_Subscribers_plot3 <- ggMarginal(ViewPerVideo_x_Subscribers_plot3, type = "density")
ViewPerVideo_x_Subscribers_plot3

#Get the Linear model from above plot
ViewPerVideo_x_Subs_lm <- lm(formula = View_Per_Video ~ Subscriber_Count, data = channels)
channels$Relative_Performance <- NA
channels$Relative_Performance[which(channels$Is_Subcriber_Count_Hidden == FALSE &
                                    !is.nan(channels$View_Per_Video))] <- ViewPerVideo_x_Subs_lm$residuals
#channels$Relative_Performance <- channels$Relative_Performance / channels$Subscriber_Count

#Distribution of relative performance
ggplot(channels, aes(Relative_Performance)) + 
  geom_histogram() + 
  scale_y_log10()

ViewPerVideo_x_Subscribers_plot4 <- ggplot(channels, aes(Subscriber_Count, View_Per_Video,
                                                         color = Relative_Performance)) +
  geom_point(alpha = 0.9) + 
  geom_smooth(method = "lm") +
  scale_x_log10() + 
  scale_y_log10() + 
  scale_color_viridis(option = "D") +
  ggtitle("Channel Performance, View per Video x Subscriber Count, log 10 scales") + 
  theme(legend.position = "left")
ViewPerVideo_x_Subscribers_plot4 <- ggMarginal(ViewPerVideo_x_Subscribers_plot4, type = "density")
ViewPerVideo_x_Subscribers_plot4

#View per Video X Video Count
ViewPerVid_x_Videos <- ggplot(channels, aes(Video_Count, View_Per_Video)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")
ViewPerVid_x_Videos

#Plots of Views per Video X Views per Subscriber
ViewPerVid_x_ViewPerSub_plot1 <- ggplot(channels, aes(View_Per_Video, View_Per_Subscriber,
                                                      color = Join_Date)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() +
  scale_color_viridis(trans = "date") +
  ggtitle("View per Video X View per Subscriber, log 10 scales") + 
  theme(legend.position = "left")
ViewPerVid_x_ViewPerSub_plot1 <- ggMarginal(ViewPerVid_x_ViewPerSub_plot1, type = "density")
ViewPerVid_x_ViewPerSub_plot1

ViewPerVid_x_ViewPerSub_plot2 <- ggplot(channels, aes(View_Per_Video, View_Per_Subscriber)) +
  geom_point() + 
  ggtitle("View per Video X View per Subscriber, normal scales")
ggMarginal(ViewPerVid_x_ViewPerSub_plot2, type = "density")

#View per Video per Week X Video per Year
ViewPerVidPerWk_x_VidPerYear_plot1 <- ggplot(channels, aes(Videos_Per_Year, 
                                                           View_Per_Video_Per_Week)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  ggtitle("View per Video per Week X Videos Published per Year")
ggMarginal(ViewPerVidPerWk_x_VidPerYear_plot1, type = "density")

#View per Video X View per Subscriber, log 10 scales
ViewPerVid_x_SubPerVid_plot1 <- ggplot(channels, aes(View_Per_Video, Subscriber_Per_Video,
                                                      color = Join_Date)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() +
  scale_color_viridis(trans = "date") +
  ggtitle("View per Video X Subscriber per Video, log 10 scales") + 
  theme(legend.position = "left")
ViewPerVid_x_SubPerVid_plot1 <- ggMarginal(ViewPerVid_x_SubPerVid_plot1, type = "density")
ViewPerVid_x_SubPerVid_plot1




kchannels_top25 <- channels[(which(sort(channels$Subscriber_Count))),]

#Subscribers plotted against video count with axes adjusted by log10 
plotTop25 <- ggplot(top25S_top25V, aes(Videos, Subscribers)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = 13900) +  #Median Subscribers
  geom_vline(xintercept = 100.0) #Median Videos
ggMarginal(plotTop25, type = "density")



