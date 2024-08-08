# Flow Volume - Critical Duration
# 
# This code will create average flow volume based on given duration(s)
# I have not created unregulated flow dataset yet but it will be input here
# Using regulated for now
# 
# Starting with 1-,2-,3-,4-,5-,6-,7-,12-,14-day averages
# 12-day because that was 2016 duration used.

# Dan McGraw
# 25-Mar-2024
################################################################################
# Clear workspace
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("patchwork")) install.packages("patchwork")
if(!require("scales")) install.packages("scales")
if(!require("zoo")) install.packages("zoo")
library(lubridate)
library(RColorBrewer)

# Read files ####################################################################
thur_flows_reg <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/csv/Thurmond_3-6-2024_NetInflow_Discharge.csv")
thur_flow_unreg <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Est_Unregulated_Daily_1925-2023.csv")

# Set Dates to DT format, remove excess
thur_flows_reg <- thur_flows_reg %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"))
thur_flow_unreg <- thur_flow_unreg %>% mutate(DT2 = DT)
thur_flow_unreg <- thur_flow_unreg %>% mutate(DT = as.Date(DT2,format = "%m/%d/%Y"),.before = Yr)
thur_flow_unreg <- thur_flow_unreg %>% select(-c(DT2))

colnames(thur_flows_reg)
colnames(thur_flow_unreg)

thur_flows_reg <- thur_flows_reg %>% select(c(DT,NetInflow))
#thur_flow_unreg <- thur_flow_unreg %>% select(c(DT,Est_Unreg_Inflow))
thur_flows_unimp <- thur_flows_unimp %>% select(c(DT,Flow))

# Rename columns nicely
thur_flows_reg <- thur_flows_reg %>% rename(Regulated_Inflow = NetInflow)
#thur_flow_unreg
thur_flows_unimp <- thur_flows_unimp %>% rename(Unimpaired_Inflow = Flow)

# Regulated needs yr,mon,day and water year
thur_flows_reg <- thur_flows_reg %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT),.before = Regulated_Inflow)
thur_flows_reg <- thur_flows_reg %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr),.before = Regulated_Inflow)

# Unimpaired needs yr,mon,day and water year
thur_flows_unimp <- thur_flows_unimp %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT),.before = Unimpaired_Inflow)
thur_flows_unimp <- thur_flows_unimp %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr),.before = Unimpaired_Inflow)

# Create Average Fields
# I think if you want to automate this: 
# Set "xday_vol", see if it exists, write to column

# Average daily inflow by critical duration
critdur <- 1
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_1day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_1day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_1day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 2
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_2day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_2day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_2day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 3
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_3day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_3day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_3day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 4
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_4day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_4day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_4day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 5
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_5day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_5day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_5day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 6
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_6day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_6day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_6day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 7
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_7day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_7day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_7day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 12
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_12day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_12day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_12day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))

critdur <- 14
thur_flows_reg <- thur_flows_reg %>% mutate(VolDur_14day = rollmeanr(thur_flows_reg$Regulated_Inflow,critdur,fill=NA))
thur_flow_unreg <- thur_flow_unreg %>% mutate(VolDur_14day = rollmeanr(thur_flow_unreg$Unreg_DailyQ,critdur,fill=NA))
thur_flows_unimp <- thur_flows_unimp %>% mutate(VolDur_14day = rollmeanr(thur_flows_unimp$Unimpaired_Inflow,critdur,fill=NA))


# test plot
flow.colors <- c("Daily" = "#333BFF", "VolDuration" = "orangered2")
thur_flow_unreg[0:730,] %>% ggplot() + geom_line(aes(x=DT,y=Unreg_DailyQ,color = "Daily"))+geom_line(aes(x=DT,y = VolDur_4day,color = "VolDuration"))+
  scale_x_date(date_breaks = "1 month") + labs(x="Date",y="Discharge(cfs)",color = "Legend") +
  scale_color_manual(values = flow.colors)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Export to csv
write.csv(thur_flows_reg,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/Thurmond_Regulated.csv")
write.csv(thur_flow_unreg,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/Thurmond_Unregulated.csv")
write.csv(thur_flows_unimp,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/Thurmond_Unimpaired.csv")

# Get AMS for every duration
# library(lubridate)
# 
# thur_flows_reg <- thur_flows_reg %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
# thur_flow_unreg <- thur_flow_unreg %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
# #thur_flows_unimp <- thur_flows_unimp %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
# 
# thur_flows_reg <- thur_flows_reg %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))
# thur_flow_unreg <- thur_flow_unreg %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))
# #thur_flows_unimp <- thur_flows_unimp %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))


annual_max_reg <- thur_flows_reg %>%
  group_by(WaterYear) %>%
  summarize(Max1Day = round(max(VolDur_1day,na.rm = T),0),
            Max2Day = round(max(VolDur_2day,na.rm = T),0),
            Max3Day = round(max(VolDur_3day,na.rm = T),0),
            Max4Day = round(max(VolDur_4day,na.rm = T),0),
            Max5Day = round(max(VolDur_5day,na.rm = T),0),
            Max6Day = round(max(VolDur_6day,na.rm = T),0),
            Max7Day = round(max(VolDur_7day,na.rm = T),0),
            Max12Day = round(max(VolDur_12day,na.rm = T),0),
            Max14Day = round(max(VolDur_14day,na.rm = T),0))

annual_max_unreg <- thur_flow_unreg %>%
  group_by(WaterYear) %>%
  summarize(Max1Day = round(max(VolDur_1day,na.rm = T),0),
            Max2Day = round(max(VolDur_2day,na.rm = T),0),
            Max3Day = round(max(VolDur_3day,na.rm = T),0),
            Max4Day = round(max(VolDur_4day,na.rm = T),0),
            Max5Day = round(max(VolDur_5day,na.rm = T),0),
            Max6Day = round(max(VolDur_6day,na.rm = T),0),
            Max7Day = round(max(VolDur_7day,na.rm = T),0),
            Max12Day = round(max(VolDur_12day,na.rm = T),0),
            Max14Day = round(max(VolDur_14day,na.rm = T),0))

annual_max_unimp <- thur_flows_unimp %>%
  group_by(WaterYear) %>%
  summarize(Max1Day = round(max(VolDur_1day,na.rm = T),0),
            Max2Day = round(max(VolDur_2day,na.rm = T),0),
            Max3Day = round(max(VolDur_3day,na.rm = T),0),
            Max4Day = round(max(VolDur_4day,na.rm = T),0),
            Max5Day = round(max(VolDur_5day,na.rm = T),0),
            Max6Day = round(max(VolDur_6day,na.rm = T),0),
            Max7Day = round(max(VolDur_7day,na.rm = T),0),
            Max12Day = round(max(VolDur_12day,na.rm = T),0),
            Max14Day = round(max(VolDur_14day,na.rm = T),0))

fourdaydiff <- ggplot()+
  geom_line(data = annual_max_reg, aes(x=WaterYear,y=Max5Day,color = "Regulated 4-day AMS")) +
  geom_line(data = annual_max_unreg, aes(x=WaterYear,y=Max5Day,color = "Unregulated 4-day AMS")) +
  geom_line(data = annual_max_unimp, aes(x=WaterYear,y=Max5Day,color = "Unimpaired 4-day AMS")) +
  theme_bw()+ 
  ggtitle("Comparison of 4-Day Inflow Volumes")+
  scale_x_continuous(breaks = seq(1930,2025,10),minor_breaks = seq(1930,2025,5))+
  scale_y_continuous(breaks =seq(0,200000,25000),minor_breaks = seq(0,200000,5000))+
  labs(x="Year",y="Discharge(cfs)",color = "Legend")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")

ggsave("E:/1.Thurmond/Chapter 4/Figures/Four Day AMS Inflow Vol Differences.png",
       fourdaydiff,width = 11, height = 8.5,dpi = 300)

# write.csv(annual_max_reg,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/AMS/Thurmond_Regulated_AMS.csv")
# write.csv(annual_max_unreg,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/AMS/Thurmond_Unregulated_AMS.csv")
# write.csv(annual_max_unimp,"E:/1.Thurmond/Chapter 4/HH/Data/Daily Volume Duration/AMS/Thurmond_Unimpaired_AMS.csv")

# Peak over Volume ratios ######################################################
# For 4-day
# Read files ####################################################################
pordir <- "E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/"
thur_elev_file <- paste0(pordir,"ThurmondDL_3-6-2024_ELEV.txt")
thur_elev <- read.table(thur_elev_file)

# Rename columns
thur_elev <- thur_elev %>% rename(Date = V1,DOW = V2, Elev_29 = V3) 
thur_elev <- thur_elev %>% select (-c(DOW))

# Convert to Date to arrange by increasing date
thur_elev <- thur_elev %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = Elev_29)
thur_elev <- thur_elev %>% arrange(DT)

thur_elev88 <- thur_elev %>% mutate(Elev_88 = Elev_29 - 0.7)
thur_elev88 <- thur_elev88 %>% select(-c(Elev_29))

# AMS dates
thur_elev88 <- thur_elev88 %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_elev88 <- thur_elev88 %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

annual_max_stage88 <- thur_elev88 %>%
  group_by(WaterYear) %>%
  summarize(Max_Elev = max(Elev_88),
            Date = DT[which.max(Elev_88)],
            Month = month(DT[which.max(Elev_88)]),
            Day = day(DT[which.max(Elev_88)]))

eventDTs <- annual_max_stage88$Date

# Grab flow hydrographs 14 days before and after max pool
eventDTs_start <- eventDTs - 7
eventDTs_end <- eventDTs + 7

thur_peak_to_vol <- list()
flow.colors <- c("Inflow" = "#333BFF", "4-day Vol" = "orangered2")

for (i in 1:length(eventDTs)){
  #hist_pool_date <- eventDTs[i]
  
  dt1 <- which(thur_flow_unreg$DT == eventDTs_start[i])
  dt2 <- which(thur_flow_unreg$DT == eventDTs_end[i])
  
  if (eventDTs_start[i] < thur_flow_unreg$DT[1]){
    dt1 <- 1
  }
  
  hydrograph <- thur_flow_unreg[dt1:dt2,]
  peakflow <- max(hydrograph$Unreg_DailyQ)
  peak4dayvol <- max(hydrograph$VolDur_4day)
  peaktovol_ratio <- peakflow/peak4dayvol
  
  thur_peak_to_vol[[i]] <- data.frame(Event = eventDTs[i],PeaktoVol = peaktovol_ratio)
  
}

thur_peaktovol <- do.call(rbind.data.frame,thur_peak_to_vol)

m_pov <- mean(thur_peaktovol$PeaktoVol)
sd_pov <- sd(thur_peaktovol$PeaktoVol)
med_pov <- median(thur_peaktovol$PeaktoVol)
total_count <- sum(!is.na(thur_peaktovol$PeaktoVol))

# fourdaypovhist <- ggplot(data = thur_peaktovol, aes(x =PeaktoVol))+
#   geom_histogram(binwidth = 0.1,fill="lightblue",color="black",alpha=0.9)+
#   #stat_density(aes(y = ..density..), geom = "line", color = "lightgreen", size = 1.0) +
#   theme_bw()+
#   ggtitle("4-Day Peak Over Volume Ratios")+
#   theme(plot.title = element_text(hjust = 0.5))+
#   labs(x="Peak Discharge/4-Day Vol", y="Count")+
#   scale_x_continuous(breaks = seq(0,3,0.1),minor_breaks = seq(0,3,0.05))+
#   scale_y_continuous(breaks = seq(0,20,5),minor_breaks = seq(0,20,1))
#   #geom_vline(xintercept = med_pov, color="orange2")+
#   #geom_text(aes(x=med_pov, label=paste("\nMed = ",round(med_pov,2),sep=""), y=7.5), colour="orange2", angle=90)
# 
# ggsave("E:/1.Thurmond/Chapter 4/Figures/Four Day Peak over Volume Hist.png",
#        fourdaypovhist,width = 11, height = 8.5,dpi = 300)

