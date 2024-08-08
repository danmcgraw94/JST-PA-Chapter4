# Creating Thurmond PA Figures using POR data ##################################
# Using R to create figures for chapter 4

# Dan McGraw
# 20-March-2024
################################################################################
# Clear workspace
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("patchwork")) install.packages("patchwork")
if(!require("scales")) install.packages("scales")
library(RColorBrewer)

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

# Import Data
pordir <- "E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/"
thur_elev88_file <- paste0(pordir,"/csv/Thurmond3-6-2024_ELEV_88.csv")
thur_flows_file <- paste0(pordir,"/csv/Thurmond_3-6-2024_NetInflow_Discharge.csv")
thur_stor_file <- paste0(pordir,"/csv/Thurmond_3-6-2024_STOR.csv")

thur_elev88 <- read.csv(thur_elev88_file)
thur_flows <- read.csv(thur_flows_file)
thur_stor <- read.csv(thur_stor_file)

# Drop X col
thur_elev88 <- thur_elev88 %>% select(-c(X))
thur_flows <- thur_flows %>% select(-c(X))
thur_stor <- thur_stor %>% select(-c(X))

# Change DT
thur_elev88 <- thur_elev88 %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"))
thur_flows <- thur_flows %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"))
thur_stor <- thur_stor %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"))

#Add water year column first
thur_flows <- thur_flows %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_flows <- thur_flows %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

thur_elev88 <- thur_elev88 %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_elev88 <- thur_elev88 %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

thur_stor <- thur_stor %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_stor <- thur_stor %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

# Historic Storms ##############################################################
# Plot inflow, outflow, and stage
april1964 <- as.Date("04/11/1964",format = "%m/%d/%Y")
april1964_start <- april1964 - 7
april1964_end <- april1964 + 7
aprildt1 <- which(thur_flows$DT == april1964_start)
aprildt2 <- which(thur_flows$DT == april1964_end)

april1964_data <- thur_flows[aprildt1:aprildt2,]
april1964_data <- april1964_data %>% mutate(Elev88 = thur_elev88$Elev_88[aprildt1:aprildt2],
                                            Storage = thur_stor$Storage[aprildt1:aprildt2])

jan2016 <- as.Date("1/1/2016",format = "%m/%d/%Y")
jan2016_start <- jan2016 - 14
jan2016_end <- jan2016 + 14
jandt1 <- which(thur_flows$DT == jan2016_start)
jandt2 <- which(thur_flows$DT == jan2016_end)

jan2016_data <- thur_flows[jandt1:jandt2,]
jan2016_data <- jan2016_data %>% mutate(Elev88 = thur_elev88$Elev_88[jandt1:jandt2],
                                        Storage = thur_stor$Storage[jandt1:jandt2])

# April 1964 plot ###################################################################
plot_date <- as.character(format(april1964, "%d-%b-%Y"))
flow.colors <- c("Inflow (cfs)" = "#333BFF", "Outflow (cfs)" = "orangered2","Elevation ft" = "green3")

april1964_flowplot <- ggplot(data = april1964_data)+
  geom_line(aes(x=DT,y=NetInflow,color="Inflow (cfs)"))+geom_point(aes(x=DT,y=NetInflow,color="Inflow (cfs)"))+
  geom_line(aes(x=DT,y=Discharge,color="Outflow (cfs)"))+geom_point(aes(x=DT,y=Discharge,color="Outflow (cfs)"))+
  theme_USACE()+ 
  ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  scale_y_continuous(breaks= pretty_breaks(),labels = comma)+
  labs(y="Discharge(cfs)",color = "Legend")+
  scale_color_manual(values = flow.colors)+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),legend.position = "bottom")

april1964_elevationplot <- ggplot(data = april1964_data)+
  geom_line(aes(x=DT,y=Elev88,color="Elevation ft"))+geom_point(aes(x=DT,y=Elev88,color="Elevation ft"))+
  theme_USACE()+
  #ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  scale_y_continuous(breaks = pretty_breaks())+
  coord_cartesian(ylim=c(328,338))+
  labs(x="Date",y="Elevation ft-NAVD88",color = "Legend")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")

april1964_plots <- april1964_flowplot / april1964_elevationplot
april1964_plots

ggsave("E:/1.Thurmond/Chapter 4/Figures/HistoricStorms/April_11_1964.png",
       april1964_plots,width = 11, height = 8.5,dpi = 300)

april1964_storageplot <- ggplot(data = april1964_data)+
  geom_line(aes(x=DT,y=Storage))+geom_point(aes(x=DT,y=Storage))+
  theme_USACE()+
  ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  scale_y_continuous(breaks = pretty_breaks())+
  #coord_cartesian(ylim=c(328,338))+
  labs(x="Date",y="Storage ac-ft",color = "Legend")+
  #scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")

# ggsave("E:/1.Thurmond/Chapter 4/Figures/HistoricStorms/April_11_1964_7x5.png",
#        april1964_plots,width = 7, height = 5,dpi = 300)

# January 2016 plot ###################################################################
plot_date <- as.character(format(jan2016, "%d-%b-%Y"))
flow.colors <- c("Inflow (cfs)" = "#333BFF", "Outflow (cfs)" = "orangered2","Elevation ft" = "green3")

jan2016_flowplot <- ggplot(data = jan2016_data)+
  geom_line(aes(x=DT,y=NetInflow,color="Inflow (cfs)"))+geom_point(aes(x=DT,y=NetInflow,color="Inflow (cfs)"))+
  geom_line(aes(x=DT,y=Discharge,color="Outflow (cfs)"))+geom_point(aes(x=DT,y=Discharge,color="Outflow (cfs)"))+
  theme_USACE()+
  ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  scale_y_continuous(breaks= pretty_breaks(),labels = comma)+
  labs(y="Discharge(cfs)",color = "Legend")+
  scale_color_manual(values = flow.colors)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),legend.position = "bottom")

jan2016_elevationplot <- ggplot(data = jan2016_data)+
  geom_line(aes(x=DT,y=Elev88,color="Elevation ft"))+geom_point(aes(x=DT,y=Elev88,color="Elevation ft"))+
  theme_USACE()+
  #ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  labs(x="Date",y="Elevation ft-NAVD88",color = "Legend")+
  scale_y_continuous(breaks = pretty_breaks())+
  coord_cartesian(ylim=c(325,336))+
  scale_color_manual(values = flow.colors)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")

jan2016_plots <- jan2016_flowplot / jan2016_elevationplot
jan2016_plots

ggsave("E:/1.Thurmond/Chapter 4/Figures/HistoricStorms/Jan_01_2016.png",
       jan2016_plots,width = 11, height = 8.5,dpi = 300)

jan2016_storageplot <- ggplot(data = jan2016_data)+
  geom_line(aes(x=DT,y=Storage))+geom_point(aes(x=DT,y=Storage))+
  theme_USACE()+ 
  ggtitle(paste0("Record Pool Event on ",plot_date))+
  scale_x_date(date_breaks = "1 day", date_labels = "%d-%b")+
  scale_y_continuous(breaks = pretty_breaks())+
  #coord_cartesian(ylim=c(328,338))+
  labs(x="Date",y="Storage ac-ft",color = "Legend")+
  #scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "none")

# ggsave("E:/1.Thurmond/Chapter 4/Figures/HistoricStorms/Jan_01_2016_7x5.png",
#        jan2016_plots,width = 7, height = 5,dpi = 300)


## Tailwater rating curve from RAS
TW_RC <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Water Control Manual/RAS_TW_Ratingcurve.csv")

ggplot(data = TW_RC, aes(x=Flow_cfs, y=Elev_88)) + geom_line()+
  theme_USACE()+
  scale_x_continuous(breaks = seq(0,4000000,500000),labels = comma,minor_breaks = seq(0,4000000,100000))+
  scale_y_continuous(breaks = seq(150,280,10),minor_breaks = seq(150,280,1))+
  labs(x="Discharge (cfs)",y="Elevation (ft-NAVD88)")

ggsave("E:/1.Thurmond/Chapter 4/Figures/PMF/Tailwater_RC_RAS.png",width = 11, height = 8.5,dpi = 300)
