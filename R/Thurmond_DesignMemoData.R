# Thurmond Design Memo Data ####################################################
#################################################################################
# Clear workspace
rm(list = ls(all.names = TRUE))

# From Appendix A, Dec 1945
library(tidyverse)
library(scales)
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
# Data input####################################################################
thur_sdf_storm <- data.frame(Duration.hrs = c(0,6,12,18,24,48,72,96),Rainfall.In = c(0,7.7,11.1,13.7,15,18.3,20.5,21.8))

# Design Storm Dist
thur_designstorm <- data.frame(dur_6hr_periods = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                                rainfall_6hr_in = c(0,0.4,0.5,0.6,0.6,0.8,0.9,1.3,3.4,7.7,2.6,0.9,0.7,0.6,0.6,0.3,0.3))

thur_designstorm <- thur_designstorm %>% mutate(Duration.hrs = dur_6hr_periods*6)
thur_designstorm <- thur_designstorm %>% mutate(Precip_Total = cumsum(rainfall_6hr_in))

ggplot(data = thur_designstorm,aes(x=Duration.hrs,y=rainfall_6hr_in))+geom_line()+
  geom_point()+
  theme_USACE()+
  ggtitle("Thurmond Design Storm Most Critical Sequence")+
  labs(x="Hours",y="Rainfall inches")+
  scale_x_continuous(breaks = seq(0,102,12),minor_breaks = seq(0,102,6))+
  scale_y_continuous(breaks = seq(0,8,1),minor_breaks = seq(0,8,0.25))

ggplot(data = thur_designstorm,aes(x=Duration.hrs,y=Precip_Total))+geom_line()+
  geom_point()+
  theme_USACE()+
  ggtitle("Thurmond Design Storm Accumulated Precip")+
  labs(x="Hours",y="Rainfall inches")+
  scale_x_continuous(breaks = seq(0,102,12),minor_breaks = seq(0,102,6))+
  scale_y_continuous(breaks = seq(0,25,2),minor_breaks = seq(0,25,1))

ggplot(data = thur_sdf_storm,aes(x=Duration.hrs,y=Rainfall.In))+geom_line()+
  geom_point()+theme_bw()+ggtitle("Thurmond SDF Storm")+
  labs(x="Hours",y="Rainfall inches")+
  scale_x_continuous(breaks = seq(0,102,12),minor_breaks = seq(0,102,6))+
  scale_y_continuous(breaks = seq(0,25,2),minor_breaks = seq(0,25,1))

# Checking SDF vs Accumulated Precip
ggplot()+
  geom_line(data = thur_sdf_storm,aes(x=Duration.hrs,y=Rainfall.In,color = "SDF Depth-Duration"))+
  geom_point(data = thur_sdf_storm,aes(x=Duration.hrs,y=Rainfall.In,color = "SDF Depth-Duration"))+
  geom_line(data = thur_designstorm,aes(x=Duration.hrs,y=Precip_Total,color = "SDF Temporal Distrib"))+
  geom_point(data = thur_designstorm,aes(x=Duration.hrs,y=Precip_Total,color = "SDF Temporal Distrib"))+
  theme_USACE()+
  ggtitle("Thurmond SDF Storm")+
  labs(x="Hours",y="Rainfall inches",color="Legend")+
  scale_x_continuous(breaks = seq(0,102,12),minor_breaks = seq(0,102,6))+
  scale_y_continuous(breaks = seq(0,25,2),minor_breaks = seq(0,25,1))

# Plot Inflow SDF and Unit Hydrograph
sdf <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/RFA Hydrographs/Hourly/SDF.csv")
uh <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/RFA Hydrographs/Hourly/Design UH.csv")

ggplot(data = sdf,aes(x=Hour,y=Inflow))+
  geom_line()+
  geom_point()+
  theme_USACE()+
  #ggtitle("Thurmond Spillway Design Flood (SDF)")+
  labs(x="Hours",y="Inflow (cfs)")+
  scale_x_continuous(breaks = seq(0,96,12),minor_breaks = seq(0,96,6))+
  scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                     breaks = seq(0,1500000,100000),minor_breaks = seq(0,2000000,50000))

ggsave("E:/1.Thurmond/Chapter 4/Figures/DesignMemo_SDF.png",width = 11, height = 8.5,dpi = 300)

ggplot(data = uh,aes(x=Hour,y=Inflow))+
  geom_line()+
  geom_point()+
  theme_USACE()+
  #ggtitle("Thurmond Spillway Design Flood (SDF)")+
  labs(x="Hours",y="Inflow (cfs)")+
  scale_x_continuous(breaks = seq(0,96,6),minor_breaks = seq(0,96,3))+
  scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                     breaks = seq(0,120000,10000),minor_breaks = seq(0,120000,5000))

ggsave("E:/1.Thurmond/Chapter 4/Figures/DesignMemo_UH.png",width = 11, height = 8.5,dpi = 300)
