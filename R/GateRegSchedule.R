# Thurmond PMF Results
# Gate Reg Schedule
rm(list = ls(all.names = TRUE))
library(tidyverse)
pmf2014 <- read.csv("E:/1.Thurmond/MMC_Discharge/2014PMF.csv")
pmf2024 <- read.csv("E:/1.Thurmond/MMC_Discharge/2024PA_PMF.csv")

source("E:/R/theme_USACE.r")

# MMC RAS data
mmcpmf_dis <- read.csv("E:/1.Thurmond/MMC_Discharge/MMC_2014PMF.csv",header=T)
mmc_inflow <- read.csv("E:/1.Thurmond/MMC_Discharge/MMC_Inflow.csv",header=T)
mmcpmf <- mmcpmf_dis[mmcpmf_dis$Time_ord %in% mmc_inflow$Time_ord,]
mmcpmf <- mmcpmf %>% mutate(Inflow = mmc_inflow$Inflow_cfs[1:35],.before = Discharge_cfs)
row.names(mmcpmf) <- NULL

# Gate Regs
gatereg <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Water Control Manual/GateRegSchedule.csv")

# Assign Gate Regs #############################################################
# Assign Flow Bin
breaks <- c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, Inf)
inflow_factor <- cut(pmf2014$Inflow_cfs, breaks, labels = c("cfs50", "cfs100", "cfs200", "cfs300", "cfs400", "cfs500", "cfs600", "cfs700", "cfs800", "cfs900", "cfs1000", "full"), right = FALSE)
pmf2014 <- pmf2014 %>% mutate(reg_curve = as.character(inflow_factor))

# do the same for mmc data
mmc_inflowfact <- cut(mmcpmf$Inflow, breaks, labels = c("cfs50", "cfs100", "cfs200", "cfs300", "cfs400", "cfs500", "cfs600", "cfs700", "cfs800", "cfs900", "cfs1000", "full"), right = FALSE)
mmcpmf <- mmcpmf %>% mutate(reg_curve = as.character(mmc_inflowfact))

# use flow bin to get discharge for elevations
for (i in 1:length(pmf2014$Inflow_cfs)){
  reg_curve <- pmf2014$reg_curve[i]
  elev_i <- pmf2014$Elev_NAVD88[i]
  
  # find elev bin
  regelev <-  ifelse(elev_i >= 342.4,342.4,min(gatereg$ResElev_88[gatereg$ResElev_88 >= elev_i]))
  
  # find schedule discharge for 2014 pmf
  reg_col <- which(colnames(gatereg) == reg_curve)
  reg_row <- which(gatereg$ResElev_88 == regelev)
  Q <- gatereg[reg_row,reg_col]
  
  pmf2014$Schedule_Discharge[i] <- Q
}

# repeat for mmc data
for (i in 1:length(mmcpmf$Inflow)){
  reg_curve <- mmcpmf$reg_curve[i]
  elev_i <- mmcpmf$Elev_NAVD88[i]
  
  # find elev bin
  regelev <-  ifelse(elev_i >= 342.4,342.4,min(gatereg$ResElev_88[gatereg$ResElev_88 >= elev_i]))
  
  # find schedule discharge for 2014 pmf
  reg_col <- which(colnames(gatereg) == reg_curve)
  reg_row <- which(gatereg$ResElev_88 == regelev)
  Q_mmc <- gatereg[reg_row,reg_col]
  
  mmcpmf$Schedule_Discharge[i] <- Q_mmc
}

write.csv(pmf2014,"E:/1.Thurmond/MMC_Discharge/2014PMF_Schedule.csv",row.names = FALSE)
write.csv(mmcpmf,"E:/1.Thurmond/MMC_Discharge/MMCPMF_Schedule.csv",row.names = FALSE)

# Plot both inflows
mmcpmf <- mmcpmf %>% mutate(Time_hrs = ((Time_ord*15)/60))

ggplot()+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Inflow_cfs,color="2014 HMS"))+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Inflow,color="MMC RAS"))+
  labs(x = "Duration (hrs)", y = "Inflow (cfs)", title = "Thurmond Inflow",color="Legend")+
  scale_x_continuous(breaks = seq(0,300,24))+
  theme_USACE()+
  scale_color_manual(values = c("#377EB8", "red2"), 
                     name = "Legend", 
                     labels = c("2014 HMS","MMC RAS"))
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/1.Inflow_Comparison.png",width = 11, height = 8.5,dpi = 300)

ggplot()+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Schedule_Discharge,color="2014 HMS"))+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Schedule_Discharge,color="MMC RAS"))+
  labs(x = "Duration (hrs)", y = "Discharge (cfs)", title = "Gate Regulation Schedule Discharge",color="Legend")+
  scale_x_continuous(breaks = seq(0,300,24))+
  theme_USACE()+
  scale_color_manual(values = c("#377EB8", "red2"), 
                     name = "Legend", 
                     labels = c("2014 HMS","MMC RAS"))
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/2.Gate_Reg_Sched_Comparison.png",width = 11, height = 8.5,dpi = 300)

ggplot()+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Schedule_Discharge,color="Sched Discharge"))+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Discharge_cfs,color="Model Discharge"))+
  labs(x = "Duration (hrs)", y = "Discharge (cfs)", title = "Gate Regulation Schedule Discharge - 2014 HMS Model",color="Legend")+
  scale_x_continuous(breaks = seq(0,300,24))+
  theme_USACE()+
  scale_color_manual(values = c("#E08A1C","#4DAF4A"), 
                     name = "Legend", 
                     labels = c("Sched Discharge","Model Discharge"))
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/3.HMS_Discharge.png",width = 11, height = 8.5,dpi = 300)

ggplot()+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Schedule_Discharge,color="Sched Discharge"))+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Discharge_cfs,color="Model Discharge"))+
  labs(x = "Duration (hrs)", y = "Discharge (cfs)", title = "Gate Regulation Schedule Discharge - MMC RAS Model",color="Legend")+
  scale_x_continuous(breaks = seq(0,300,24))+
  theme_USACE()+
  scale_color_manual(values = c("#E08A1C","#4DAF4A"), 
                     name = "Legend", 
                     labels = c("Sched Discharge","Model Discharge"))
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/4.RAS_Discharge.png",width = 11, height = 8.5,dpi = 300)

ggplot()+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Schedule_Discharge,color="MMC Sched Discharge"))+
  geom_line(data=mmcpmf,aes(x=Time_hrs,y=Discharge_cfs,color="MMC Model Discharge"))+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Schedule_Discharge,color="HMS Sched Discharge"))+
  geom_line(data=pmf2014,aes(x=Time_hrs,y=Discharge_cfs,color="HMS Model Discharge"))+
  labs(x = "Duration (hrs)", y = "Discharge (cfs)", title = "Gate Regulation Schedule Discharge - MMC RAS Model",color="Legend")+
  scale_x_continuous(breaks = seq(0,300,24))+
  theme_USACE()

ggplot()+
  geom_point(data=pmf2014[1:113,],aes(x=Elev_NAVD88,y=Schedule_Discharge,color="Sched Discharge"))+
  geom_line(data=pmf2014[1:113,],aes(x=Elev_NAVD88,y=Schedule_Discharge,color="Sched Discharge"))+
  labs(x = "Elev (ft-NAVD88)", y = "Discharge (cfs)", title = "Gate Regulation Schedule Discharge - 2014 HMS Model",color="Legend")+
  scale_x_continuous(breaks = seq(320.3,360.3,1))+
  scale_y_continuous(breaks = seq(0,1000000,25000))+
  theme_USACE()+
  scale_color_manual(values = c("#E08A1C","#4DAF4A"), 
                     name = "Legend", 
                     labels = c("Sched Discharge","Model Discharge"))
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/HMS_Discharge_schedule_PMF.png",width = 11, height = 8.5,dpi = 300)
ggsave("E:/1.Thurmond/MMC_Discharge/Figures/HMS_Discharge_schedule_PMF_11x17.png",width = 17, height = 11,dpi = 300)
