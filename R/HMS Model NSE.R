# PMF Model Validation Results
# Calculating Nash-Sutcliffe efficiency
# Clear workspace
rm(list = ls(all.names = TRUE))
library(tidyverse)
theme_USACE <-  function(base_size = 8){theme(
  text = element_text(family = 'serif', color = 'black'),
  line = element_line(colour = 'black', linewidth = 0.2), 
  rect = element_rect(colour = 'black', linewidth = 0.2),
  plot.title = element_text(vjust = 3, size = 9),
  plot.margin = unit(c(1,1,1,1), 'lines'),
  panel.border = element_rect(fill = F),
  panel.grid.major = element_line(colour = 'grey50', linewidth = 0.2),
  panel.grid.minor = element_line(colour = 'grey75', linewidth = 0.1),
  panel.background = element_rect(fill = 'white'),
  #defaults legend to upper left, can/should be overridden based on graph
  #legend.background = element_blank(),
  legend.background = element_rect(fill = "lightgrey", colour = "black"),
  legend.justification = c("left", "top"),
  legend.position = c(0.8, 0.5),
  # this value should be adjusted dependent on 
  # graph with the addition of another 
  # theme(legend.position = c(X, Y)) argument after theme_USACE()
  # or... for no legend 
  # legend.position = element_blank(),
  legend.key = element_blank(),
  legend.title = element_text(size = 9),
  #legend.title = element_blank(), 
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(angle = 90, size = 9),
  axis.text.x = element_text(margin = margin(8, 0, 0, 0)),
  axis.text.y = element_text(margin = margin(0, 8, 0, 0)),
  axis.ticks.length = unit(0.25 , 'cm')
)}

# Read in 3 storms
apr1964 <- read.csv("E:/1.Thurmond/Chapter 4/PMP_PMF/Thurmond_1964_Results.csv")
mar1998 <- read.csv("E:/1.Thurmond/Chapter 4/PMP_PMF/Thurmond_1998_Results.csv")
jul2013 <-read.csv("E:/1.Thurmond/Chapter 4/PMP_PMF/Thurmond_2013_Results.csv")

# plot modeled vs observed flow
flow.colors <- c("Modeled" = "#333BFF", "Observed" = "orangered2")

ggplot(data = jul2013)+
  geom_line(aes(x=Time_hrs,y=Inflow_cfs,color = "Modeled"))+
  geom_line(aes(x=Time_hrs,y=OBS_Flow_cfs,color = "Observed"))+
  theme_USACE()+
  ggtitle("July 2013 Event")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,550,48),minor_breaks = seq(0,550,24))+
  scale_y_continuous(breaks = seq(0,125000,10000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

ggplot(data = mar1998)+
  geom_line(aes(x=Time_hrs,y=Inflow_cfs,color = "Modeled"))+
  geom_line(aes(x=Time_hrs,y=OBS_Flow_cfs,color = "Observed"))+
  theme_USACE()+
  ggtitle("March 1998 Event")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,550,48),minor_breaks = seq(0,550,24))+
  scale_y_continuous(breaks = seq(0,125000,10000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

ggplot(data = apr1964)+
  geom_line(aes(x=Time_hrs,y=Inflow_cfs,color = "Modeled"))+
  geom_line(aes(x=Time_hrs,y=OBS_Flow_cfs,color = "Observed"))+
  theme_USACE()+
  ggtitle("April 1964 Event")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0,550,48),minor_breaks = seq(0,550,24))+
  scale_y_continuous(breaks = seq(0,125000,10000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

# Compute Nash-Sutcliffe efficiency
jul2013 <- jul2013[,colSums(is.na(jul2013))<nrow(jul2013)]
jul2013 <- jul2013[rowSums(is.na(jul2013)) < ncol(jul2013),]
mar1998 <- mar1998[,colSums(is.na(mar1998))<nrow(mar1998)]
apr1964 <- apr1964[,colSums(is.na(apr1964))<nrow(apr1964)]

m_apr1964_obs <- mean(apr1964$OBS_Flow_cfs)
m_jul2013_obs <- mean(jul2013$OBS_Flow_cfs)
m_mar1998_obs <-mean(mar1998$OBS_Flow_cfs)

jul2013 <- jul2013 %>% mutate(numer_i = (OBS_Flow_cfs - Inflow_cfs)^2,denom_i = (OBS_Flow_cfs - m_jul2013_obs)^2)
mar1998 <- mar1998 %>% mutate(numer_i = (OBS_Flow_cfs - Inflow_cfs)^2,denom_i = (OBS_Flow_cfs - m_mar1998_obs)^2)
apr1964 <- apr1964 %>% mutate(numer_i = (OBS_Flow_cfs - Inflow_cfs)^2,denom_i = (OBS_Flow_cfs - m_apr1964_obs)^2)

nse_jul2013 <- 1 - ((sum(jul2013$numer_i)) / (sum(jul2013$denom_i)))
nse_mar1998 <- 1 - ((sum(mar1998$numer_i)) / (sum(mar1998$denom_i)))
nse_apr1964 <- 1 - ((sum(apr1964$numer_i)) / (sum(apr1964$denom_i)))

