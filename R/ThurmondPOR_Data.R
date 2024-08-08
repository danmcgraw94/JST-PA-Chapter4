# Thurmond POR data ############################################################
# Inputs are raw text files copied from WM Site
# Dan McGraw
# 6-March-2024
################################################################################

# Clear workspace
rm(list = ls(all.names = TRUE))

# Load Libraries
#install.packages("tidyverse")
library("tidyverse")
library("ggplot2")
library(RColorBrewer)
library(lubridate)
source("E:/R/theme_USACE.r")

# Read files ####################################################################
pordir <- "E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/"
thur_elev_file <- paste0(pordir,"ThurmondDL_3-6-2024_ELEV.txt")
thur_flows_file <- paste0(pordir,"ThurmondDL_3-6-2024_NetInflow_Discharge.txt")
thur_stor_file <- paste0(pordir,"ThurmondDL_3-6-2024_STOR.txt")
thur_gen_file <- paste0(pordir,"ThurmondDL_3-6-2024_GEN.txt")

thur_elev <- read.table(thur_elev_file)
thur_flows <- read.table(thur_flows_file)
thur_stor <- read.table(thur_stor_file)
thur_gen <- read.table(thur_gen_file)

# Rename columns
thur_elev <- thur_elev %>% rename(Date = V1,DOW = V2, Elev_29 = V3) 
thur_flows <- thur_flows %>% rename(Date = V1,DOW = V2, NetInflow = V3, Discharge = V4) 
thur_stor <- thur_stor %>% rename(Date = V1,DOW = V2, Storage = V3) 
thur_gen <- thur_gen %>% rename(Date = V1,DOW = V2, Gen = V3)

# Drop DOW column
# can change Date column formatting if desired for more R work
class(thur_elev$Date)

thur_elev <- thur_elev %>% select (-c(DOW))
thur_flows <- thur_flows %>% select (-c(DOW))
thur_stor <- thur_stor %>% select (-c(DOW))

# Convert to Date to arrange by increasing date
thur_flows <- thur_flows %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = NetInflow )
thur_elev <- thur_elev %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = Elev_29)
thur_stor <- thur_stor %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = Storage)

thur_elev <- thur_elev %>% arrange(DT)
thur_flows <- thur_flows %>% arrange(DT)
thur_stor <- thur_stor %>% arrange(DT)

thur_elev88 <- thur_elev %>% mutate(Elev_88 = Elev_29 - 0.7)
thur_elev88 <- thur_elev88 %>% select(-c(Elev_29))

# Export to CSV - this can be manually exported as excel or DSS
# Someone kindly wrote an R package for direct dss import/export in R: https://github.com/eheisman/dssrip
thur_elev_outfile <- paste0(pordir,"csv/Thurmond3-6-2024_ELEV.csv")
thur_elev88_outfile <- paste0(pordir,"csv/Thurmond3-6-2024_ELEV_88.csv")
thur_flows_outfile <- paste0(pordir,"csv/Thurmond_3-6-2024_NetInflow_Discharge.csv")
thur_stor_outfile <- paste0(pordir,"csv/Thurmond_3-6-2024_STOR.csv")

#write.csv(thur_elev,thur_elev_outfile)
#write.csv(thur_elev88,thur_elev88_outfile)
#write.csv(thur_flows,thur_flows_outfile)
#write.csv(thur_stor,thur_stor_outfile)

# Max pool
maxthurmpool <- max(thur_elev$Elev_29)

# Check historic Flows
library(lubridate)

thur_flows <- thur_flows %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = NetInflow )
thur_elev <- thur_elev %>% mutate(DT = as.Date(Date,format = "%m/%d/%Y"),.before = Elev_29)

thryears <- unique(year(thur_flows$DT))

thur_flows %>% filter(NetInflow > 80000)%>%
  ggplot(aes(x=DT,y=NetInflow))+geom_point()

thur_flows %>% filter(NetInflow > 80000)

ggplot(data = thur_flows,aes(x=DT,y=Discharge)) + geom_point()+
  coord_cartesian(ylim=c(0,20000))

# Check values for table 3 and 4 of PA
bigdates <- c("1964-03-26","1964-04-08","1971-03-03","1975-03-14","1998-02-04","2020-02-07")
event1 <- which(thur_flows$DT == "1975-03-01")
event2 <- which(thur_flows$DT == "1975-03-31")
thur_flows[event1:event2,] %>%
  ggplot(aes(x=DT,y=NetInflow))+geom_point()+geom_line()
thur_elev[event1:event2,] %>%
  ggplot(aes(x=DT,y=Elev_29))+geom_point()+geom_line()

thur_elev[event1:event2,] %>% arrange(desc(Elev_29))

# MAX Pool Elevations (top 10)
thur_elev %>% top_n(10,Elev_29) %>%
  ggplot(aes(x=DT,y=Elev_29))+geom_point()

thur_elev %>% top_n(10,Elev_29)%>% arrange(desc(Elev_29))

# Max Inflows (top 10)
thur_flows %>% top_n(10,NetInflow)%>%
  ggplot(aes(x=DT,y=NetInflow))+geom_point()

thur_flows %>% top_n(11,NetInflow) %>% arrange(desc(NetInflow))

# Get annual maxima - Add water year column first
thur_flows <- thur_flows %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_flows <- thur_flows %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

thur_elev88 <- thur_elev88 %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
thur_elev88 <- thur_elev88 %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

# Extract water year and calculate annual max
annual_max_flows <- thur_flows %>%
  group_by(WaterYear) %>%
  summarize(MaxInflow = max(NetInflow),
            Date = DT[which.max(NetInflow)],
            Month = month(DT[which.max(NetInflow)]),
            Day = day(DT[which.max(NetInflow)]))

annual_max_stage88 <- thur_elev88 %>%
  group_by(WaterYear) %>%
  summarize(Max_Elev = max(Elev_88),
            Date = DT[which.max(Elev_88)],
            Month = month(DT[which.max(Elev_88)]),
            Day = day(DT[which.max(Elev_88)]))


# write.csv(annual_max_stage88,paste0(pordir,"Stage_AMS.csv"))
# write.csv(annual_max_flows,paste0(pordir,"Reg_Flow_AMS.csv"))

# sanity check
# thur_flows[thur_flows$Yr == 1989 |thur_flows$Yr == 1990 |thur_flows$Yr == 1991,] %>% ggplot()+geom_point(aes(x=DT,y=NetInflow))+geom_line(aes(x=DT,y=NetInflow))+
#   scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
#                date_labels = "%b") +theme_bw()

thur_flows %>% filter(thur_flows$NetInflow %in% annual_max_flows$MaxInflow) %>%
  ggplot(aes(x=as.factor(Mon))) + geom_histogram(stat="count",binwidth=1)+
  xlab("Month")
  
# Trying to use my algorithm for flood duration ################################
thur_elev88$WSERate <- NA
thur_elev88$WSERate[1]<-0

# Get rate of changer per day
for (i in 2:length(thur_elev88$Elev_88)){
  day2 = thur_elev88$Elev_88[i]
  day1 = thur_elev88$Elev_88[i-1]
  stagedelta = day2 - day1
  thur_elev88$WSERate[i] = stagedelta
}

FastRise <- thur_elev88 %>%
  arrange(desc(WSERate)) %>% slice_max(WSERate,n=150)

FastRise <- thur_elev88 %>%
  arrange(desc(WSERate)) %>% #slice_max(WSERate,n=125)# slice(1:125)
  top_frac(0.01)

ggplot(data=FastRise,aes(x=WSERate,y=Elev_88))+geom_point()+theme_bw()

# Duration
stormdurations = list()

# Find Start Dates
for (i in 1:length(FastRise$WSERate)){
  startDT = FastRise$DT[i]
  found <-FALSE
  
  while (!found){
    if (thur_elev88$WSERate[which(thur_elev88$DT == startDT)] <=0.25){
      found <- TRUE
    } else{
      startDT <- startDT - 1
    }
  }
  
  endDT = FastRise$DT[i]
  found <-FALSE
  while (!found){
    if (thur_elev88$WSERate[which(thur_elev88$DT == endDT)] >= -0.25 & thur_elev88$WSERate[which(thur_elev88$DT == endDT)] < 0){
      found <- TRUE
    } else{
      endDT <- endDT + 1
    }
  }
  stormdurations[[i]] <- c(as.Date(startDT,"%m/%d/%Y"),as.Date(endDT,"%m/%d/%Y"))
}

# convert to DF
stormdates.messy <- as.data.frame((stormdurations))
stormdates <- as.data.frame(t(stormdates.messy))

rm(stormdates.messy)

# Remove row names, change colnames
rownames(stormdates) <- NULL
colnames(stormdates) <- c("StartDate","EndDate")

# Get Duration
stormdates <- stormdates %>% mutate(Duration.Days = as.numeric(as.Date(EndDate) - as.Date(StartDate)))
stormdates <- stormdates %>% mutate(maxWSERate = FastRise$WSERate)

# Exploratory
ggplot(data = stormdates,aes(x=maxWSERate,y=Duration.Days))+geom_point()+
  theme_bw()+
  ggtitle("Maximum Rising WSE Rate of Selected Storms")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Max WSE Rising Rate (ft/day)",y="Flood Duration (days)")

colorpal <- brewer.pal(9,"Set1")
histcolor <- colorpal[2]
linecolor<- "white"#colorpal[9]
meancolor <- colorpal[1]
#medcolor <- colorpal[3]
sdcolor <- colorpal[3]

mWSErate <- mean(stormdates$maxWSERate)
sdWSErate <- sd(stormdates$maxWSERate)

# Hist of Max WSE Rise Rate
ggplot(data = stormdates,aes(x=maxWSERate))+
  geom_histogram(binwidth=0.25,fill=histcolor,color=linecolor,alpha=0.9)+
  theme_bw()+
  ggtitle("Maximum Rising WSE Rate of Selected Events")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Max Rising WSE Rate (ft/day)", y="Frequency")+
  scale_x_continuous(breaks = seq(0,7,0.25))+
  geom_vline(xintercept = mWSErate, color=meancolor)+
  geom_vline(xintercept = (mWSErate+sdWSErate), color=sdcolor, linetype = "dashed")+
  geom_vline(xintercept = (mWSErate-sdWSErate), color=sdcolor, linetype = "dashed")+
  geom_text(aes(x=mWSErate, label=paste("\nMean = ",round(mWSErate,2),sep=""), y=45), colour=meancolor, angle=90)+
  geom_text(aes(x=(mWSErate+sdWSErate), label=paste("\n+1SD = ",round(mWSErate+sdWSErate,2),sep=""), y=45), colour=sdcolor, angle=90)+
  geom_text(aes(x=(mWSErate-sdWSErate), label=paste("\n-1SD = ",round(mWSErate-sdWSErate,2),sep=""), y=45), colour=sdcolor, angle=90)

# Hist of Storm Durations
mdur <- mean(stormdates$Duration.Days)
sddur <- sd(stormdates$Duration.Days)

ggplot(data = stormdates,aes(x=Duration.Days))+
  geom_histogram(binwidth=1,fill=histcolor,color=linecolor,alpha=0.9)+
  stat_density(aes(y = ..count..), geom = "line", color = "lightgreen", size = 1) +
  theme_bw()+
  ggtitle("Duration of Selected Events")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Duration (days)", y="Frequency")+
  scale_x_continuous(breaks = seq(0,30,1),minor_breaks = seq(0,30,0.5))+
  geom_vline(xintercept = mdur, color=meancolor)+
  geom_vline(xintercept = (mdur+sddur), color=sdcolor, linetype = "dashed")+
  geom_vline(xintercept = (mdur-sddur), color=sdcolor, linetype = "dashed")+
  geom_text(aes(x=mdur, label=paste("\nMean = ",round(mdur,2),sep=""), y=7.5), colour=meancolor, angle=90)+
  geom_text(aes(x=(mdur+sddur), label=paste("\n+1SD = ",round(mdur+sddur,2),sep=""), y=7.5), colour=sdcolor, angle=90)+
  geom_text(aes(x=(mdur-sddur), label=paste("\n-1SD = ",round(mdur-sddur,2),sep=""), y=7.5), colour=sdcolor, angle=90)

# Select Storms and get average of dates #######################################
# Event dates are from the Chapter 4
eventdates <- c("04/11/1964",
                "3/30/1964",
                "3/3/1971",
                "3/17/1975",
                "2/7/1998",
                "1/1/2016")

eventDTs <- as.Date(eventdates,format = "%m/%d/%Y")

# Combine with the annual maxima (later)
ams_dates <- annual_max_stage88$Date
ams_dates <- annual_max_flows$Date
analysisdates <- unique(c(eventDTs,ams_dates))

eventDTs <- analysisdates # comment out to not do the all ams

# Grab flow hydrographs 14 days before and after max pool
eventDTs_start <- eventDTs - 7
eventDTs_end <- eventDTs + 7

thur_rep_hydrographs <- list()
thur_flood_durations <- list()
flow.colors <- c("Inflow" = "#333BFF", "Outflow" = "orangered2")

for (i in 1:length(eventDTs)){
  hist_pool_date <- eventDTs[i]
  
  dt1 <- which(thur_flows$DT == eventDTs_start[i])
  dt2 <- which(thur_flows$DT == eventDTs_end[i])
  
  if (eventDTs_start[i] < thur_flows$DT[1]){
    dt1 <- 1
  }
  
  hydrograph <- thur_flows[dt1:dt2,]
  hydrograph$Event <- eventDTs[i]
  thur_rep_hydrographs[[i]] <- hydrograph
  
  plot_date <- as.character(format(hist_pool_date, "%d-%b-%Y"))
  flood_dur_plot<-ggplot(data = hydrograph)+
    geom_line(aes(x=DT,y=NetInflow,color="Inflow"))+geom_point(aes(x=DT,y=NetInflow,color="Inflow"))+
    geom_line(aes(x=DT,y=Discharge,color="Outflow"))+geom_point(aes(x=DT,y=Discharge,color="Outflow"))+
    theme_USACE()+
    ggtitle(paste0("Record Pool Event on ",plot_date))+
    scale_x_date(date_breaks = "1 day")+labs(x="Date",y="Discharge(cfs)",color = "Legend")+
    scale_color_manual(values = flow.colors)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = c(0.8,0.8))
  
  # Find where Inflow > outflow for flood duration
  flood_dates <- hydrograph$DT[hydrograph$NetInflow >= hydrograph$Discharge]
  peakflowdate <- hydrograph$DT[max(hydrograph$NetInflow) == hydrograph$NetInflow]
  event_index <- which(flood_dates == peakflowdate)
  
  if (length(event_index) == 0){
    event_index <- length(flood_dates)+3
  }
  
  # Find consecutive dates around eventDT
  start_index <- event_index
  end_index <- event_index
  
  # Expand the start index to include previous consecutive dates
  while (start_index > 1 && flood_dates[start_index] - flood_dates[start_index - 1] == 1) {
    start_index <- start_index - 1
  }
  
  # Expand the end index to include following consecutive dates
  while (end_index < length(flood_dates) && flood_dates[end_index + 1] - flood_dates[end_index] == 1) {
    end_index <- end_index + 1
  }
  
  # Extract consecutive dates
  consecutive_dates <- flood_dates[start_index:end_index]
  
  # Flood Duration in Days
  flood_dur <- length(consecutive_dates)
  
  # march 21 2006 is tough, duration likely = 4-5 but outflow was always larger than inflow
  # peak outflow matched peak inflow
  
  thur_flood_durations[[i]] <- data.frame(Event = hist_pool_date,Dur_days = flood_dur)

  # Write CSV
  write.csv(hydrograph,paste0("E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/FloodDurations/Flood_",hist_pool_date,".csv"),row.names = F)
  
  # Save plot of flood event
  ggsave(paste0("E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/FloodDurations/Plots/",hist_pool_date,".png"),
         flood_dur_plot,width = 8, height = 6,dpi = 300)
}

thur_dur <- do.call(rbind.data.frame,thur_flood_durations)


mdur <- mean(thur_dur$Dur_days)
sddur <- sd(thur_dur$Dur_days)
meddur <- median(thur_dur$Dur_days)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(thur_dur$Dur_days)

durationhist <- ggplot(data = thur_dur,aes(x=Dur_days))+geom_histogram(binwidth=1,fill=histcolor,color=linecolor,alpha=0.9)+
  #stat_density(aes(y = ..count..), geom = "line", color = "lightgreen", size = 1) +
  scale_x_continuous(breaks = seq(1,14,1))+
  theme_USACE()+
  labs(x = "Flood Duration (Days)",y="Count",subtitle = "Based on AMS Flood Events (1962 - 2024)")+
  ggtitle("Distribution of Flood Durations at Thurmond Dam")+
  geom_vline(xintercept = mdur, color=meancolor)+
  geom_vline(xintercept = (mdur+sddur), color=sdcolor, linetype = "dashed")+
  geom_vline(xintercept = (mdur-sddur), color=sdcolor, linetype = "dashed")+
  #geom_vline(xintercept = meddur, color="orange2")+
  #geom_text(aes(x=meddur, label=paste("\nMed = ",round(meddur,2),sep=""), y=7.5), colour="orange2", angle=90)
  #geom_text(aes(x=mdur, label=paste("\nMean = ",round(mdur,2),sep=""), y=7.5), colour=meancolor, angle=90)+
  #geom_text(aes(x=(mdur+sddur), label=paste("\n+1SD = ",round(mdur+sddur,2),sep=""), y=7.5), colour=sdcolor, angle=90)+
  #geom_text(aes(x=(mdur-sddur), label=paste("\n-1SD = ",round(mdur-sddur,2),sep=""), y=7.5), colour=sdcolor, angle=90)
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggsave("E:/1.Thurmond/Chapter 4/Figures/Distribution of Flood Event Durations.png",
       durationhist,width = 11, height = 8.5,dpi = 300)
ggsave("E:/1.Thurmond/Chapter 4/Figures/Distribution of Flood Event Durations_5x7.png",
       durationhist,width = 7, height = 5,dpi = 300)

# Unimpaired Dataset ###########################################################
# Un-Impaired Flow Dataset from Arcadis - for Augusta
unimp_daily_raw <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Unimpaired Flow Study/Augusta_Final_UNIMP-0ADJ Annual.csv")
sapply(unimp_daily_raw,class)

unimp_daily_raw <- unimp_daily_raw %>% mutate(DT = as.Date(Date,"%d %b %Y"),.before = Date)
unimp_daily_raw <- unimp_daily_raw %>% mutate(Flow_cfs = as.numeric(gsub(",", "",Flow)))

unimp_daily_raw <- unimp_daily_raw %>% mutate(Flow = as.numeric(Flow_cfs))
sapply(unimp_daily_raw,class)

unimp_daily <- unimp_daily_raw %>% select(c(DT,Flow))

# Get annual maxima - Add water year column first
unimp_daily <- unimp_daily %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
unimp_daily <- unimp_daily %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))

# Extract water year and calculate annual max
annual_max_flows_unimp<- unimp_daily %>%
  group_by(WaterYear) %>%
  summarize(MaxInflow = max(Flow),
            Date = DT[which.max(Flow)],
            Month = month(DT[which.max(Flow)]),
            Day = day(DT[which.max(Flow)]))

write.csv(annual_max_flows_unimp,"E:/1.Thurmond/Chapter 4/HH/Data/Unimpaired Flow Study/UnImp_Flow_AMS_Augusta.csv")

# Now for Thurmond
unimp_daily_raw <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Unimpaired Flow Study/Thurmond_Final_UNIMP-0ADJ Annual.csv")
sapply(unimp_daily_raw,class)

unimp_daily_raw <- unimp_daily_raw %>% mutate(DT = as.Date(Date,"%d %b %Y"),.before = Date)
unimp_daily_raw <- unimp_daily_raw %>% mutate(Flow_cfs = as.numeric(gsub(",", "",Flow)))

unimp_daily_raw <- unimp_daily_raw %>% mutate(Flow = as.numeric(Flow_cfs))
sapply(unimp_daily_raw,class)

unimp_daily <- unimp_daily_raw %>% select(c(DT,Flow))

# Get annual maxima - Add water year column first
unimp_daily <- unimp_daily %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
unimp_daily <- unimp_daily %>% mutate(WaterYear = ifelse(Mon >= 10, Yr + 1, Yr))
write.csv(unimp_daily,"E:/1.Thurmond/Chapter 4/HH/Data/Unimpaired Flow Study/UnImp_Flow_Daily_Thurmond.csv")

# Extract water year and calculate annual max
annual_max_flows_unimp<- unimp_daily %>%
  group_by(WaterYear) %>%
  summarize(MaxInflow = max(Flow),
            Date = DT[which.max(Flow)],
            Month = month(DT[which.max(Flow)]),
            Day = day(DT[which.max(Flow)]))

#write.csv(annual_max_flows_unimp,"E:/1.Thurmond/Chapter 4/HH/Data/Unimpaired Flow Study/UnImp_Flow_AMS_Thurmond.csv")

# Combine Dataset of Thurmond POR and Thurmond Unimp daily flows
summary(unimp_daily$DT)
summary(thur_flows$DT)

thur_flows <- thur_flows %>% left_join(unimp_daily,by="DT")
colnames(thur_flows)

thur_flows <- thur_flows %>% select(c(Date,DT,Yr.x,Mon.x,Day.x,WaterYear.x,NetInflow,Flow))

thur_reg_unimp <- thur_flows %>% rename(Yr = Yr.x,
                                        Mon = Mon.x,
                                        Day= Day.x,
                                        WaterYear = WaterYear.x,
                                        Regulated_Inflow = NetInflow,
                                        Unimpaired_Inflow = Flow)

thur_reg_unimp <- thur_reg_unimp %>% mutate(Percent_Red = Unimpaired_Inflow/Regulated_Inflow)

  # plot daily comparison
ggplot(data = thur_reg_unimp,aes(x=Regulated_Inflow,y=Unimpaired_Inflow))+
  geom_point(size=0.5,aes(alpha=0.75))+
  geom_abline(intercept = 0, slope = 1)+
  theme_USACE()+ 
  ggtitle("Savannah River at JST Dam")+
  labs(x="Regulated Discharge (cfs)",y="Unimpaired Discharge (cfs)",subtitle = "Measured Regulated and Unimpaired Flow Daily discharges",
       color = "Legend")+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),legend.position = "none")+
  coord_cartesian(xlim = c(0,120000),ylim = c(0,120000))+
  scale_x_continuous(breaks = seq(0,120000,10000)) + 
  scale_y_continuous(breaks = seq(0,120000,10000))

# Seasonality by Stage #########################################################
amsStageSeason <- ggplot(data = annual_max_stage88,aes(x = Month))+
  geom_histogram(binwidth=1,fill="lightblue",color="black",alpha=0.9)+
  stat_density(aes(y = ..count..), geom = "line", color = "green3", size = 1) +
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,20,2),minor_breaks = seq(0,20,1))+
  theme_USACE()+
  labs(x = "Month",y="Count",subtitle = "Based on AMS of POR Stage Elev.(1962 - 2024)")+
  ggtitle("High Pool AMS Seasonality at Thurmond Dam")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggsave("E:/1.Thurmond/Chapter 4/Figures/AMS Stage Seasonality.png",
       amsStageSeason,width = 11, height = 8.5,dpi = 300)
