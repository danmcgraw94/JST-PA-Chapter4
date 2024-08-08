# Converting Thurmond POR data to CSV/xlsx #####################################
# Inputs are raw text files copied from WM Site: http://155.82.195.152:8080/SAS/cf/DataQuery/DataQuery.cfm


# Plotting rainfall, pool, and discharges against Nathan's flow meter data
# He claims to have plotted the data against pool elevation, but I want to compare with discharges.

# Dan McGraw
# 4-23-2024
################################################################################
# Clear workspace
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
library(RColorBrewer)
library(lubridate)
library(patchwork)
library(zoo)

# install.packages("ggpubr")
# library(ggpubr)

# Read files ####################################################################
pordir <- "E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/"
thur_elev_file <- paste0(pordir,"ThurmondDL_3-6-2024_ELEV.txt")
thur_flows_file <- paste0(pordir,"ThurmondDL_3-6-2024_NetInflow_Discharge.txt")
thur_stor_file <- paste0(pordir,"ThurmondDL_3-6-2024_STOR.txt")

thur_elev <- read.table(thur_elev_file)
thur_flows <- read.table(thur_flows_file)
thur_stor <- read.table(thur_stor_file)

# Rename columns
thur_elev <- thur_elev %>% rename(Date = V1,DOW = V2, Elev_29 = V3) 
thur_flows <- thur_flows %>% rename(Date = V1,DOW = V2, NetInflow = V3, Discharge = V4) 
thur_stor <- thur_stor %>% rename(Date = V1,DOW = V2, Storage = V3) 

# Drop DOW column
# can change Date column formatting if desired for more R work
class(thur_elev$Date)

thur_elev <- thur_elev %>% select (-c(DOW))
thur_flows <- thur_flows %>% select (-c(DOW))
thur_stor <- thur_stor %>% select (-c(DOW))

# Convert to Date to arrange by increasing date
thur_flows <- thur_flows %>% mutate(DT = as.POSIXlt(Date,format = "%m/%d/%Y"),.before = NetInflow )
thur_elev <- thur_elev %>% mutate(DT = as.POSIXlt(Date,format = "%m/%d/%Y"),.before = Elev_29)
thur_stor <- thur_stor %>% mutate(DT = as.POSIXlt(Date,format = "%m/%d/%Y"),.before = Storage)

thur_elev <- thur_elev %>% arrange(DT)
thur_flows <- thur_flows %>% arrange(DT)
thur_stor <- thur_stor %>% arrange(DT)

# thur_elev88 <- thur_elev %>% mutate(Elev_88 = Elev_29 - 0.7)
# thur_elev88 <- thur_elev88 %>% select(-c(Elev_29))
thur_elev <- thur_elev %>% mutate(Elev_88 = Elev_29 - 0.7)
class(thur_elev$DT)

# Import flow meter data
flowdata <- read.csv("E:/1.Thurmond/FlowMeter/flowdata.csv",header=T)
flowdata <-  flowdata %>% mutate(DT =  as.POSIXlt(Date,format = "%m/%d/%Y %H:%M"),.before = Flow)
flowdata <- flowdata %>% arrange(DT)
class(flowdata$DT)

#flowdata$Incremental_rainfall <- c(flowdata$Rainfall_cume[1], diff(flowdata$Rainfall_cume))
flowdata$Rainfall_inc <- 0

for (i in 2:length(flowdata$Rainfall_cume)){
  
  P1 <- flowdata$Rainfall_cume[i-1]
  P2 <- flowdata$Rainfall_cume[i]
  P_inc <- P2-P1
  
  flowdata$Rainfall_inc[i] = P_inc
}

# Flag the end of accumulations
flowdata$End_flag <- ifelse(flowdata$Rainfall_inc <0,1,0)

# Set end of accumulations to 0
flowdata$Rainfall_inc[flowdata$Rainfall_inc <0] <- 0
flowdata$Rainfall_inc[1]<-flowdata$Rainfall_cume[1]

start_date <- flowdata$DT[1]
end_date <- flowdata$DT[length(flowdata$DT)]

# Import COOP HPD Data
hpd_raw <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Precip Data/Coop/USC00381726.csv",header=T)
hpd_raw <- hpd_raw %>% mutate(DT = as.POSIXct(DATE,format = "%Y-%m-%d"),.after = DATE)
hpd <- hpd_raw %>% select(c(DT,STATION,LATITUDE,LONGITUDE,DlySum,DlySumMF,DlySumQF,DlySumS1,DlySumS2))

hpd_poi <- hpd %>% filter(DT >= start_date & DT <= end_date)
hpd_poi <- hpd_poi %>% mutate(Precip_in = DlySum/100)

hpd_poi$Cumulative3day <- rollmean(hpd_poi$Precip_in,k=3,fill=NA,align="left")

# Obtain Pool and flow data from the same time
# poi = period of interest
thur_elev_poi <- thur_elev %>% filter(DT >= start_date & DT <= end_date)
thur_flows_poi <- thur_flows %>% filter(DT >= start_date & DT <= end_date)

colnames(thur_elev_poi)
colnames(thur_flows_poi)

# Combine into nice DF
thur_res_poi <- data.frame(DT = thur_flows_poi$DT, 
                           Elev_29 =thur_elev_poi$Elev_29,
                           Elev88 = thur_elev_poi$Elev_88,
                           Inflow = as.numeric(thur_flows_poi$NetInflow),
                           Discharge = as.numeric(thur_flows_poi$Discharge))

sapply(thur_res_poi,class)

# ggplot needs posixct to plot
thur_res_poi <- thur_res_poi %>% mutate(DT2 = DT)
thur_res_poi <- thur_res_poi %>% mutate(DT = as.POSIXct(DT2))
sapply(thur_res_poi,class)
thur_res_poi <- thur_res_poi %>% select(-c(DT2))

flowdata <- flowdata %>% mutate(DT2 = DT)
flowdata <- flowdata %>% mutate(DT = as.POSIXct(DT2))
sapply(flowdata,class)
flowdata <- flowdata %>% select(-c(DT2))

# plot to compare ##############################################################
elevplot <- ggplot() + 
  geom_point(data = thur_res_poi,aes(x=DT,y=Elev_29),size=1)+
  geom_line(data = thur_res_poi,aes(x=DT,y=Elev_29))+
  theme_bw()+
  scale_y_continuous(breaks = seq(321,326,1),minor_breaks = seq(321,326,0.25))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Reservoir Pool Elev (ft-NGVD29)",x="Date")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7.5))

dischargeplot <- ggplot() + 
  geom_point(data = thur_res_poi,aes(x=DT,y=Discharge),size=1)+
  geom_line(data = thur_res_poi,aes(x=DT,y=Discharge))+
  geom_hline(yintercept = 3100,color="red3")+
  theme_bw()+
  scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Reservoir Discharge (cfs)",x="Date")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7.5))

inflowplot <- ggplot() + 
  geom_hline(yintercept = 0,color = "green3")+
  geom_point(data = thur_res_poi,aes(x=DT,y=Inflow),size=1)+
  geom_line(data = thur_res_poi,aes(x=DT,y=Inflow))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Reservoir Inflow (cfs)",x="Date")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7.5))

drydt1 <- as.POSIXct("2023-10-21 00:00:00")
drydt2 <- as.POSIXct("2023-11-09 00:00:00")
drydt3 <- as.POSIXct("2023-11-29 00:00:00")
drydt4 <- as.POSIXct("2023-12-09 00:00:00")

flowmeterplot <- ggplot() + 
  geom_point(data = flowdata[flowdata$Flow >=0,],aes(x=DT,y=Flow),size=1)+
  #geom_line(data = flowdata,aes(x=DT,y=Flow))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Pipe Flow (gpm)",x="Date")+
  geom_vline(xintercept = drydt1,color = "orange2")+
  geom_vline(xintercept = drydt2,color = "orange2")+
  geom_vline(xintercept = drydt3,color = "red3")+
  geom_vline(xintercept = drydt4,color = "red3")+
  theme(axis.title.y = element_text(size = 7.5))

flowmeterplot_full <- ggplot() + 
  geom_point(data = flowdata,aes(x=DT,y=Flow),size=1)+
  #geom_line(data = flowdata,aes(x=DT,y=Flow))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Pipe Flow (gpm)",x="Date")+
  theme(axis.title.y = element_text(size = 7.5))

precipplot_bad <- ggplot() + 
  geom_point(data = flowdata,aes(x=DT,y=Rainfall_cume),size=1)+
  #geom_line(data = flowdata[flowdata$Rainfall_inc <0.1,],aes(x=DT,y=Rainfall_inc))+
  #geom_line(data = flowdata,aes(x=DT,y=Flow))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Cumulative Precip (in.)",x="Date")+
  theme(axis.title.y = element_text(size = 7.5))

hpdplot <- ggplot() + 
  geom_point(data = hpd_poi,aes(x=DT,y=Precip_in),size=1)+
  geom_line(data = hpd_poi,aes(x=DT,y=Precip_in))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="Cumulative Precip (in.)",x="Date")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7.5))

hyeto <- ggplot(flowdata) +
  theme_bw()+
  geom_point(aes(x = DT,y = Rainfall_inc),color="blue") +
  labs(x = "Time", y = "Incremental Rainfall(in.)") +
  scale_y_continuous(breaks = seq(0,0.05,0.01))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  coord_cartesian(ylim=c(0,0.05))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7.5))

av3dayrain <- ggplot() + 
  geom_point(data = hpd_poi,aes(x=DT,y=Cumulative3day),color="blue2")+
  #geom_line(data = hpd_poi,aes(x=DT,y=Precip_in))+
  #geom_line(data = flowdata,aes(x=DT,y=Flow))+
  theme_bw()+
  #scale_y_continuous(breaks = seq(2500,5000,500),minor_breaks = seq(2500,5000,100))+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %d",date_minor_breaks = "1 day")+
  labs(y="3-day Avg Precip (in.)",x="Date")+
  theme(axis.title.y = element_text(size = 7.5))

elevplot/dischargeplot/inflowplot/flowmeterplot
ggsave("E:/1.Thurmond/FlowMeter/Figures/Elev-Dis-In-Meter.png",width = 11, height = 8.5,dpi = 400)

hpdplot/precipplot_bad
dischargeplot/flowmeterplot
inflowplot/flowmeterplot

precipplot_bad/flowmeterplot
ggsave("E:/1.Thurmond/FlowMeter/Figures/BadPrecip-Meter.png",width = 11, height = 8.5,dpi = 400)

hpdplot/flowmeterplot
ggsave("E:/1.Thurmond/FlowMeter/Figures/HPDPrecip-Meter.png",width = 11, height = 8.5,dpi = 400)

hyeto/flowmeterplot
ggsave("E:/1.Thurmond/FlowMeter/Figures/IncPrecip-Meter.png",width = 11, height = 8.5,dpi = 400)

hpdplot/hyeto/flowmeterplot
ggsave("E:/1.Thurmond/FlowMeter/Figures/Precips-Meter.png",width = 11, height = 8.5,dpi = 400)

# flowmeterplot/flowmeterplot_full
# ggsave("E:/1.Thurmond/FlowMeter/Figures/Flow meter errors.png.png",width = 11, height = 8.5,dpi = 400)

flowmeterplot_full
ggsave("E:/1.Thurmond/FlowMeter/Figures/Flow meter errors.png.png",width = 11, height = 8.5,dpi = 400)
