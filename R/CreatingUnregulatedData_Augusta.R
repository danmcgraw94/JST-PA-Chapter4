# Creating an unregulated AMS Dataset for the Augusta Gage
# Open Augusta Data and set regulated data to unregulated via conversion fomr USGS/USACE
# Unregulated report

# Joe Hoke is on report

# Dan McGraw
# 15-May-2024
################################################################################
# Clear workspace
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("stats")) install.packages("stats")
if(!require("lubridate")) install.packages("lubridate")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("scales")) install.packages("scales")
if(!require("dataRetrieval")) install.packages("dataRetrieval")
if(!require("zoo")) install.packages("zoo")
if(!require("naniar")) install.packages("naniar")

source("E:/R/theme_USACE.r")

# Read files ###################################################################
# Import Digitized Data for Figure 35 - Exceedance Prob for Unregulated and Regulated
fig35 <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Figure35data.csv")
fig35 <- fig35 %>% mutate(NEP = 100-ExceedanceProb)

# Convert Reg and Unreg at Augusta
augusta_conversion <- fig35 %>% mutate(Aug_RegQ = PeakQRegulated,Aug_UnregQ = PeakQUnregulated)
augusta_conversion <- augusta_conversion %>% select(c(Aug_RegQ,Aug_UnregQ))

# Import Augusta Daily Data ####################################################
##### Grab Augusta Data
siteNo <- "02197000"
pCode <- "00060" #00061
start.date <- "1700-01-01"
end.date <- "2024-12-31"

augusta_daily <- readNWISdv(siteNumbers = siteNo,
                            parameterCd = pCode,
                            startDate = start.date,
                            endDate = end.date)

augusta_daily <- renameNWISColumns(augusta_daily)

augusta_daily <- augusta_daily %>% mutate(DT = as.Date(Date),.before = Flow)

ggplot(data = augusta_daily) + geom_line(aes(x=DT,y=Flow))
#naniar::vis_miss(augusta_daily)

# Add water year column
augusta_daily <- augusta_daily %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
augusta_daily <- augusta_daily %>% mutate(WaterYear = ifelse(Mon > 9, Yr + 1, Yr))

# Augusta Peak Flow AMS ########################################################
augusta_peak_ams <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/csv/Augusta_Peak_AMS.csv",header = T)
augusta_peak_ams <- augusta_peak_ams %>% mutate(DT = as.Date(Date),.before = Flow)
augusta_peak_ams <- augusta_peak_ams %>% mutate(Yr = year(DT),Mon = month(DT),Day = day(DT))
augusta_peak_ams <- augusta_peak_ams %>% mutate(WaterYear = ifelse(Mon > 9, Yr + 1, Yr))
augusta_peak_ams <- augusta_peak_ams %>% rename(Peak_Flow = Flow)

# Convert to Unregulated #######################################################
# Regulation Assumed to start in 1951
regulation_start <- as.Date("1950-01-10")
regulation_wy <- 1951
pre_reg <- augusta_daily %>% filter(WaterYear < regulation_wy)
aug_reg <- augusta_daily %>% filter(WaterYear >= regulation_wy)

pre_reg_peak <- augusta_peak_ams %>% filter(WaterYear < regulation_wy)
aug_reg_peak <- augusta_peak_ams %>% filter(WaterYear >= regulation_wy)

# Estime Unregulated from approx function
Augusta_Unreg <- approx(augusta_conversion$Aug_RegQ, augusta_conversion$Aug_UnregQ, xout = aug_reg$Flow)[2]
Augusta_Unreg_peak <- approx(augusta_conversion$Aug_RegQ, augusta_conversion$Aug_UnregQ, xout = aug_reg_peak$Peak_Flow)[2]

# add to regulated data
aug_reg <- aug_reg %>% mutate(Unregulated_Flow = round(Augusta_Unreg$y,0))
aug_reg_peak <- aug_reg_peak %>% mutate(Unregulated_PeakFlow = round(Augusta_Unreg_peak$y,0))

# create a 1:1 column of unregulated flow to make bind_rows easier
pre_reg <- pre_reg %>% mutate(Unregulated_Flow = round(Flow,0))
pre_reg_peak <- pre_reg_peak %>% mutate(Unregulated_PeakFlow = round(Peak_Flow,0))

# Combine datasets
augusta_daily_unreg <- bind_rows(pre_reg,aug_reg)
AMS_Peak <- bind_rows(pre_reg_peak,aug_reg_peak)

# remove any confusion
rm(Augusta_Unreg)
rm(Augusta_Unreg_peak)

# Check results ################################################################
ggplot(data = augusta_daily_unreg) +
  geom_line(aes(x=DT,y=Unregulated_Flow,color = "Unregulated"))+
  geom_line(aes(x=DT,y=Flow,color="Existing"))+
  theme_USACE()+
  labs(x = "Date",
       y = "Daily Flow (cfs)",
       title = "Daily Flow Values Augusta (02197000)",
       subtitle = "Post-WY 1951 Converted to Unregulated")+
  scale_y_continuous(breaks = seq(0,250000,50000),minor_breaks = seq(0,250000,10000))+
  scale_x_date(breaks = "10 year",minor_breaks = "1 year")+
  scale_color_manual(values = c("#377EB8", "red2"), 
                     name = "Legend", 
                     labels = c("Existing","Unregulated"))+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),legend.position = c(.85,.8))
ggsave("E:/1.Thurmond/Chapter 4/Figures/UnregulatedData/Unreg_TimeSeries.png",width = 11, height = 8.5,dpi = 300)

# Create Thurmond Daily unregulated0.93
thurm_daily_export <- augusta_daily_unreg %>% mutate(Thurm_Unreg = 0.93*Unregulated_Flow) %>%
  select(c(DT,Thurm_Unreg))

# Export Results to csv ########################################################
write.csv(augusta_daily_unreg,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Augusta_Unregulated_Daily_1888-2024.csv",row.names = F)
write.csv(AMS_Peak,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Augusta_PeakAMS_Unreg.csv",row.names = F)

write.csv(thurm_daily_export,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unregulated_Daily_1888-2024.csv",row.names = F)
rm(thurm_daily_export)

# Obtain AMS for Daily Data ####################################################
# 1-day AMS
AMS_1day <- augusta_daily_unreg %>% 
  group_by(WaterYear) %>%
  top_n(1, Unregulated_Flow) %>%
  distinct(WaterYear, .keep_all = TRUE) %>%
  ungroup()

# 4-day volume AMS
# Create Data with gaps to avoid rolling mean over the gaps (i.e. 1891-12-31 & 1896-01-01)
complete_dates <- seq(min(augusta_daily_unreg$DT), max(augusta_daily_unreg$Date), by = "day")
aug_unreg_complete <- merge(augusta_daily_unreg, data.frame(Date = complete_dates), by = "Date", all = TRUE)
# aug_unreg_complete[3010:3020,]  # check
naniar::vis_miss(aug_unreg_complete) # see gaps in data

critdur <- 4
aug_unreg_complete <- aug_unreg_complete %>% mutate(VolDur_4day = round(rollmeanr(Unregulated_Flow,k = critdur, fill=NA),0))
aug_unreg_complete <- aug_unreg_complete %>% mutate(VolDur_5day = round(rollmeanr(Unregulated_Flow,k = 5, fill=NA),0))

AMS_4day <- aug_unreg_complete %>% 
  group_by(WaterYear) %>%
  top_n(1, VolDur_4day) %>%
  distinct(WaterYear, .keep_all = TRUE) %>%
  ungroup()

AMS_5day <- aug_unreg_complete %>% 
  group_by(WaterYear) %>%
  top_n(1, VolDur_5day) %>%
  distinct(WaterYear, .keep_all = TRUE) %>%
  ungroup()


AMS_4day <- AMS_4day %>% mutate(Ratio_1d_4d = Unregulated_Flow/VolDur_4day)

# Combine Peak, 1-day, and 4-day AMS to help stay organized
# 4- Day already has 1-day info in it (unregulated flow)
all_AMS <- left_join(AMS_Peak,AMS_4day,AMS_5day,by = "WaterYear") # %>% select(-c(WaterYear.y)) %>% rename(WaterYear = WaterYear.x)
colnames(all_AMS)
all_AMS <- all_AMS %>% select(c(DT.x,Peak_Flow,Flow_notes,WaterYear,Unregulated_PeakFlow,DT.y,Unregulated_Flow,VolDur_4day,VolDur_5day,Ratio_1d_4d))
all_AMS <- all_AMS %>% rename(DT_Peak = DT.x, DT_Daily = DT.y)

# Export AMS  ##################################################################
write.csv(AMS_1day,"E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/Augusta_Unreg_AMS_1Day.csv",row.names = F)
write.csv(AMS_4day,"E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/Augusta_Unreg_AMS_4Day.csv",row.names = F)
#write.csv(AMS_Peak,"E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/Augusta_PeakAMS_Unreg.csv",row.names = F)

# Calculate Peak to 4-day Vol Ratio
matchingAMS <- all_AMS %>% filter(DT_Peak >= DT_Daily - 4 | DT_Peak <= DT_Daily + 4)
matchingAMS <- matchingAMS %>% mutate(PeaktoVol = Unregulated_PeakFlow/VolDur_4day)
mean(matchingAMS$PeaktoVol)
median(matchingAMS$PeaktoVol)

all_AMS <- all_AMS %>% mutate(Ratio_Peak_4d = ifelse(DT_Peak >= DT_Daily - 4 | DT_Peak <= DT_Daily + 4,Unregulated_PeakFlow/VolDur_4day,NA))
mean(all_AMS$Ratio_Peak_4d,na.rm = T)

# Plot 1-day to 4-day ratio
ggplot(data = AMS_4day)+
  geom_histogram(aes(x = Ratio_1d_4d),fill="lightblue",color="black",alpha=0.9,
                breaks = seq(0.9,1.5,0.1))+
  #stat_density(aes(x =PeaktoVol,y = ..density..), geom = "line", color = "lightgreen", size = 1.0) +
  theme_USACE()+
  ggtitle("Ratio of 1 Day to 4 Day Volume at Augusta (02197000)")+
  labs(x="1 Day Vol/4-Day Vol", y="Count")+
  scale_x_continuous(breaks = seq(0.9,1.5,0.1),minor_breaks = seq(0,1.5,0.05))+
  scale_y_continuous(breaks = seq(0,15,1))+
  geom_vline(xintercept = mean(AMS_4day$Ratio_1d_4d), color="orange2")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave("E:/1.Thurmond/Chapter 4/Figures/One Day to Four Day Volume Hist.png",width = 8, height = 6,dpi = 300)
 
# Plot Peak to 4-day ratio
ggplot(data = matchingAMS)+
  geom_histogram(aes(x =PeaktoVol),fill="lightblue",color="black",alpha=0.9,
                 breaks = seq(0.9,3.0,0.1))+
  #stat_density(aes(x =PeaktoVol,y = ..density..), geom = "line", color = "lightgreen", size = 1.0) +
  theme_USACE()+
  ggtitle("Ratio of Peak Flow to 4-Day Volume at Augusta (02197000)")+
  labs(x="Peak Flow/4-Day Vol", y="Count")+
  scale_x_continuous(breaks = seq(0.9,3.0,0.1),minor_breaks = seq(0,3.0,0.05))+
  scale_y_continuous(breaks = seq(0,25,1))+
  geom_vline(xintercept = mean(matchingAMS$PeaktoVol), color="orange2")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(size=6.5))
ggsave("E:/1.Thurmond/Chapter 4/Figures/Peak Flow to Four Day Volume Hist.png",width = 7, height = 5,dpi = 300)

# Apply Peak to Vol Ratio to Peak values missing ##############################
PtVratio_4day <- mean(matchingAMS$PeaktoVol)
PtVratio_1day <- mean(matchingAMS$Unregulated_PeakFlow/matchingAMS$Unregulated_Flow)
peaktovol5dayvals <- matchingAMS %>% mutate(PeaktoVol = Unregulated_PeakFlow/VolDur_5day) %>% select(PeaktoVol)
PtVratio_5day <- mean(peaktovol5dayvals$PeaktoVol)

all_AMS <- all_AMS %>% mutate(Peak_to_1day = round(Unregulated_PeakFlow/PtVratio_1day,0))
all_AMS <- all_AMS %>% mutate(Peak_to_4day = round(Unregulated_PeakFlow/PtVratio_4day,0))
all_AMS <- all_AMS %>% mutate(Peak_to_5day = round(Unregulated_PeakFlow/PtVratio_5day,0))

all_AMS <- all_AMS %>% mutate(Complete_1Day_AMS = ifelse(is.na(all_AMS$Unregulated_Flow),Peak_to_1day,Unregulated_Flow))
all_AMS <- all_AMS %>% mutate(Complete_4Day_AMS = ifelse(is.na(all_AMS$VolDur_4day),Peak_to_4day,VolDur_4day))
all_AMS <- all_AMS %>% mutate(Complete_5Day_AMS = ifelse(is.na(all_AMS$VolDur_5day),Peak_to_5day,VolDur_5day))

# Apply Updated Drainage Area ratio and use for Thurmond #######################
DAratio <- 0.93

all_AMS <- all_AMS %>% mutate(Thurm_AMS_1day = round(DAratio*Complete_1Day_AMS,0))
all_AMS <- all_AMS %>% mutate(Thurm_AMS_4day = round(DAratio*Complete_4Day_AMS,0))
all_AMS <- all_AMS %>% mutate(Thurm_AMS_5day = round(DAratio*Complete_5Day_AMS,0))

all_AMS <- all_AMS %>% rename(Aug_AMS_1day = Unregulated_Flow, 
                              Aug_AMS_4day = VolDur_4day,
                              Aug_AMS_5day = VolDur_5day,
                              Aug_Complete_1Day_AMS = Complete_1Day_AMS,
                              Aug_Complete_4Day_AMS = Complete_4Day_AMS,
                              Aug_Complete_5Day_AMS = Complete_5Day_AMS)

all_AMS <- all_AMS %>% mutate(BestFitNotes = ifelse(is.na(Ratio_Peak_4d),"Estimated from Peak AMS",NA))

all_AMS <- all_AMS %>% mutate(Interval_Lower = ifelse(WaterYear<1876,round((DAratio*0.8*Unregulated_PeakFlow)/PtVratio_4day,0),NA),
                              Interval_MostLikely = ifelse(WaterYear<1876,round((DAratio*1.0*Unregulated_PeakFlow)/PtVratio_4day,0),NA),
                              Interval_Upper = ifelse(WaterYear<1876,round((DAratio*1.2*Unregulated_PeakFlow)/PtVratio_4day,0),NA))

all_AMS_day5 <- all_AMS %>% mutate(Interval_Lower = ifelse(WaterYear<1876,round((DAratio*0.8*Unregulated_PeakFlow)/PtVratio_5day,0),NA),
                              Interval_MostLikely = ifelse(WaterYear<1876,round((DAratio*1.0*Unregulated_PeakFlow)/PtVratio_5day,0),NA),
                              Interval_Upper = ifelse(WaterYear<1876,round((DAratio*1.2*Unregulated_PeakFlow)/PtVratio_5day,0),NA))

#AMS_1day <- AMS_1day %>% mutate(Thurmond_Unreg = DAratio*Unregulated_Flow)
#AMS_4day <- AMS_4day %>% mutate(Thurmond_Unreg = DAratio*Unregulated_Flow)

#Thurmond_AMS_1day <- AMS_1day %>% select(c(agency_cd,Date,DT,Yr,Mon,Day,WaterYear,Thurmond_Unreg))
#Thurmond_AMS_4day <- AMS_4day %>% select(c(agency_cd,Date,DT,Yr,Mon,Day,WaterYear,Thurmond_Unreg))

#write.csv(Thurmond_AMS_1day,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unreg_AMS_1Day.csv",row.names = F)
#write.csv(Thurmond_AMS_4day,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unreg_AMS_4Day.csv",row.names = F)
write.csv(all_AMS,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unreg_AMS_Peak_1day_4day.csv",row.names = F)

bestfit_input <- all_AMS %>% select(c(WaterYear,Peak_Flow,Flow_notes,Unregulated_PeakFlow,
                                      Thurm_AMS_1day,Thurm_AMS_4day,BestFitNotes,
                                      Interval_Lower,Interval_MostLikely,Interval_Upper))

bestfit_input_5day <- all_AMS_day5 %>% select(c(WaterYear,Peak_Flow,Flow_notes,Unregulated_PeakFlow,
                                      Thurm_AMS_1day,Thurm_AMS_5day,BestFitNotes,
                                      Interval_Lower,Interval_MostLikely,Interval_Upper))

write.csv(bestfit_input,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unreg_BestFitINPUT.csv",row.names = F)
write.csv(bestfit_input_5day,"E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Thurmond_Unreg_BestFit_5day.csv",row.names = F)
