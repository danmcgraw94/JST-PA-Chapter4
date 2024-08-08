# Augusta Daily Data - Data handling
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("stats")) install.packages("stats")

# read text file
augdailyfile <- "E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/txt/Augusta_Daily_Streamflow_formatted.txt"
augusta_raw <- read.table(augdailyfile, sep = '\t',header = T)

# Time series codes
colnames(augusta_raw)
#            TS   parameter     statistic     Description
#        126850       00060     00001     Discharge, cubic feet per second (Maximum)
#        126851       00060     00002     Discharge, cubic feet per second (Minimum)
#        126852       00060     00003     Discharge, cubic feet per second (Mean)

# We want to use daily mean for consistency
augusta_raw <- augusta_raw %>% rename(MaxDailyQ = X126850_00060_00001,
                                      MaxDailyQ_Code = X126850_00060_00001_cd,
                                      MinDailyQ = X126851_00060_00002,
                                      MinDailyQ_Code = X126851_00060_00002_cd,
                                      AvgDailyQ = X126852_00060_00003,
                                      AvgDailyQ_Code = X126852_00060_00003_cd)
sapply(augusta_raw, class)
head(augusta_raw)

augusta_raw <- augusta_raw %>% mutate(DT = as.Date(datetime,format = "%Y-%m-%d"))

ggplot(data = augusta_raw,aes(x=DT,y=AvgDailyQ)) + geom_point()

# Extract just daily average
augusta <- augusta_raw %>% select(c(datetime,AvgDailyQ))

# Export to csv
write.csv(augusta,"E:/1.Thurmond/Chapter 4/HH/Data/Augusta Gage/csv/Augusta_Daily.csv",row.names =FALSE)

# fig35 <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Savannah River - USGS Unregulated Study/Figure35data.csv")
# thurm_conversion <- fig35 %>% mutate(Thurm_RegQ = PeakQRegulated*DA_ratio,Thurm_UnregQ = PeakQUnregulated*DA_ratio)
# thurm_conversion <- thurm_conversion %>% select(c(Thurm_RegQ,Thurm_UnregQ))
