#Exploring Sailing Channel Account Data

#Read in data from cleaning script
channels <- read.csv("data/Sailing_Channel_Accounts_Cleaned.csv")

summary(channels)

#How many above the median subscriber count?
nrow(channels[channels$Subscribers >= 293,])

#How many above the mean subscriber count?
nrow(channels[channels$Subscribers >= 25319,])

#Play with plots
library(ggplot2)
library(ggthemes)
library(ggExtra)

#Subscribers plotted against video count with axes adjusted by log10 
plot <- ggplot(channels, aes(Videos, Subscribers)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = 293.5) + 
  geom_vline(xintercept = 25.0)
ggMarginal(plot, type = "density")

channels[which(channels$Videos <= 10 & channels$Subscribers >= 500),1:3]

channels[which(channels$Subscribers >= 500000),1:3]

channels[which(channels$Videos >= 500 & channels$Subscribers <= 500000),1:3]

top25S_top25V <- channels[which(channels$Videos >= 25 & channels$Subscribers > 293.5),1:3]

summary(top25S_top25V)

#Subscribers plotted against video count with axes adjusted by log10 
plotTop25 <- ggplot(top25S_top25V, aes(Videos, Subscribers)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = 13900) +  #Median Subscribers
  geom_vline(xintercept = 100.0) #Median Videos
ggMarginal(plotTop25, type = "density")

top12P <- channels[which(channels$Videos >= 100.0 & channels$Subscribers > 13900),1:3]

summary(top12P)

#Subscribers plotted against video count with axes adjusted by log10 
plotTop12P <- ggplot(top12P, aes(Videos, Subscribers)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = 56850) +  #Median Subscribers
  geom_vline(xintercept = 207.5) #Median Videos
ggMarginal(plotTop12P, type = "density")

top6P <- channels[which(channels$Videos >= 207.5 & channels$Subscribers > 56850),1:3]

summary(top6P)

#Subscribers plotted against video count with axes adjusted by log10 
plotTop6P <- ggplot(top6P, aes(Videos, Subscribers)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_hline(yintercept = 150000) +  #Median Subscribers
  geom_vline(xintercept = 317.0) #Median Videos
ggMarginal(plotTop6P, type = "density")

