# Work during PA for quick analysis
rm(list = ls(all.names = TRUE))

if(!require("tidyverse")) install.packages("tidyverse")
source("E:/R/theme_USACE.r")

POR_flow <- dir("E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/csv/","*flow*",full.names = T)
POR_pool <- dir("E:/1.Thurmond/Chapter 4/HH/Data/Project POR Data/csv/","*ELEV_88*",full.names = T)

thurm_flow <- read.csv(POR_flow)
thurm_elev <- read.csv(POR_pool)

thurm_flow <- thurm_flow %>% mutate(DT = as.Date(DT))
thurm_elev <- thurm_elev %>% mutate(DT = as.Date(DT))

thurm_POR <- left_join(thurm_elev,thurm_flow,by="DT")
thurm_POR <- thurm_POR %>% select(c(DT,Elev_88,NetInflow,Discharge))

# Compare POR to gate reg schedule
gatereg <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/Water Control Manual/GateRegSchedule.csv")
breaks <- c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, Inf)
inflow_factor <- cut(thurm_POR$NetInflow, breaks, labels = c("cfs50", "cfs100", "cfs200", "cfs300", "cfs400", "cfs500", "cfs600", "cfs700", "cfs800", "cfs900", "cfs1000", "full"), right = FALSE)

thurm_POR <- thurm_POR %>% mutate(reg_curve = as.character(inflow_factor))

# Grab pool of record data
startDT <- as.Date("1964-04-01")
endDT <- as.Date("1964-04-18")
rec_pool <- thurm_POR %>% filter(DT >=startDT & DT <= endDT)

# use flow bin to get discharge for elevations
for (i in 1:length(rec_pool$NetInflow)){
  reg_curve <- rec_pool$reg_curve[i]
  elev_i <- rec_pool$Elev_88[i]
  
  # find elev bin
  regelev <-  ifelse(elev_i >= 342.4,342.4,min(gatereg$ResElev_88[gatereg$ResElev_88 >= elev_i]))
  
  # find schedule discharge for 2014 pmf
  reg_col <- which(colnames(gatereg) == reg_curve)
  reg_row <- which(gatereg$ResElev_88 == regelev)
  Q <- gatereg[reg_row,reg_col]
  
  rec_pool$Schedule_Discharge[i] <- Q
}

ggplot(data = rec_pool) + 
  geom_line(aes(x=DT,y=Discharge,color="Discharge"))+
  geom_line(aes(x=DT,y=Schedule_Discharge,color="Schedule Discharge"))


# use flow bin to get discharge for elevations
for (i in 1:length(thurm_POR$NetInflow)){
  reg_curve <- thurm_POR$reg_curve[i]
  elev_i <- thurm_POR$Elev_88[i]
  
  # find elev bin
  regelev <-  ifelse(elev_i >= 342.4,342.4,min(gatereg$ResElev_88[gatereg$ResElev_88 >= elev_i]))
  
  # find schedule discharge for 2014 pmf
  reg_col <- which(colnames(gatereg) == reg_curve)
  reg_row <- which(gatereg$ResElev_88 == regelev)
  Q <- ifelse(is.na(reg_curve),NA,gatereg[reg_row,reg_col])
  
  thurm_POR$Schedule_Discharge[i] <- Q
}

ggplot(data = thurm_POR) + 
  geom_line(aes(x=DT,y=Discharge,color="Discharge"))+
  geom_line(aes(x=DT,y=Schedule_Discharge,color="Schedule Discharge"))

ggplot(data=thurm_POR,aes(x=DT,y=Elev_88))+
  geom_line()

startDT <- as.Date("1987-07-01")
endDT <- as.Date("1990-01-01")

ggplot(data=thurm_POR,aes(x=DT,y=Elev_88))+
  geom_line()+
  scale_x_date(limits = c(startDT,endDT))+
  scale_y_continuous(breaks = seq(300,335,5),minor_breaks = seq(300,335,1))+
  theme_USACE()


# Elevation Crest Deformation ##################################################
crest_survey <- read.csv(file = "E:/1.Thurmond/Chapter 4/Thurmond2023CrestSurvey.csv",header=T)
crest_survey <- crest_survey %>% mutate(Design = 350.3,Delta = Z_Elev88 - Design)
crest_survey <- crest_survey %>% mutate(LowSpot = ifelse(Delta <0,"Low",""))
#crest_survey <- crest_survey %>% mutate(STA = ,.before= everything())

crest_survey$Dist <- 0
crest_survey$TotalDist <- 0

for(i in 2:length(crest_survey$PT)){
  x1 <- crest_survey$X_EAST[i-1]
  y1 <- crest_survey$Y_NORTH[i-1]
  
  x2 <- crest_survey$X_EAST[i]
  y2 <- crest_survey$Y_NORTH[i]
  
  disti <- sqrt(((x2-x1)^2) + ((y2-y1)^2))
  
  crest_survey$Dist[i] <- disti
}

crest_survey$TotalDist <- cumsum(crest_survey$Dist)

crest_survey <- crest_survey %>% mutate(STA = NA,.before = Z_Elev88)
crest_survey$STA[crest_survey$Z_Elev88 > 350.11 & crest_survey$Z_Elev88 < 350.13] <- 5200

ggplot(data = crest_survey)+
  theme_USACE()+
  geom_line(aes(x=X_EAST,y=Y_NORTH))+
  geom_point(aes(x=X_EAST,y=Y_NORTH),size=0.25)

ggplot(data = crest_survey)+
  theme_USACE()+
  geom_line(aes(x=PT,y=Z_Elev88,color=Embk))+
  geom_line(aes(x=PT,y=Design))+
  labs(y = "Elev. ft-NAVD88")

sc_crest <- crest_survey %>% filter(Embk == "SC")
sc_crest$Dist <- 0
sc_crest$TotalDist <- 0

for(i in 2:length(sc_crest$PT)){
  x1 <- sc_crest$X_EAST[i-1]
  y1 <- sc_crest$Y_NORTH[i-1]
  
  x2 <- sc_crest$X_EAST[i]
  y2 <- sc_crest$Y_NORTH[i]
  
  disti <- sqrt(((x2-x1)^2) + ((y2-y1)^2))
  
  sc_crest$Dist[i] <- disti
}

sc_crest$TotalDist <- cumsum(sc_crest$Dist)
sc_crest_idx <- which(sc_crest$STA==5200)
sta1 <- round(5200 - sc_crest$TotalDist[sc_crest_idx],0)
sc_crest$STA[1] <- sta1

sc_crest <- sc_crest %>% mutate(STA_Final = round(sta1 + TotalDist,0))

sc_crest %>% filter(Embk == "SC")%>%
  ggplot()+
  theme_USACE()+
  geom_line(aes(x=STA_Final,y=Z_Elev88),color="green4")+
  #geom_line(aes(x=STA_Final,y=Design))+
  scale_y_continuous(breaks = seq(350,351,0.1))+
  scale_x_continuous(breaks = seq(4800,6500,100))+
  coord_cartesian(xlim=c(4800,6100))+
  labs(y = "Elev. ft-NAVD88",title = "South Carolina Embankment",x="Station")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 350.3,color = "black")+
  geom_text(aes(5700,350.3, label = "Design Crest Elev = 350.3 ft-NAVD88", vjust = -0.5), size = 2.75)
ggsave("E:/1.Thurmond/Chapter 4/Figures/PFMs/CrestSurvey.png",height=6,width=8,dpi=300)

sc_crest %>% filter(Embk == "SC")%>%
  ggplot()+
  theme_USACE()+
  geom_line(aes(x=STA_Final,y=Z_Elev88),color="green4")+
  geom_line(aes(x=STA_Final,y=Design))+
  scale_y_continuous(breaks = seq(350,356,1.0))+
  scale_x_continuous(breaks = seq(4800,6500,100))+
  coord_cartesian(xlim=c(4800,6100))+
  labs(y = "Elev. ft-NAVD88",title = "South Carolina Embankment",x="Station")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 355.1,color = "blue2")+
  geom_text(aes(5000,355.1, label = "1.5x PMF = 355.1 ft-NAVD88", vjust = -0.5), size = 2.75)

#ggsave("E:/1.Thurmond/Chapter 4/Figures/PFMs/CrestSurvey.png",height=6,width=8,dpi=300)


# Overtopping Hydrograph
pmf150 <- read.csv(file = "E:/1.Thurmond/Chapter 4/PMP_PMF/Thurmond_1.5PMF.csv",header=T)
pmf150 <- pmf150 %>% mutate(Time_hrs = seq(1,length(Inflow_cfs),1),.before = everything())
ot_hrs <- pmf150$Time_hrs[pmf150$Stage_NAVD88 > 350]

ggplot(data=pmf150,aes(x=Time_hrs,y=Stage_NAVD88)) + 
  theme_USACE()+
  geom_line()+
  labs(x="Time (hrs)",y="Stage (ft-NAVD88)")+
  scale_x_continuous(breaks = seq(60,120,5),minor_breaks = seq(60,120,1))+
  scale_y_continuous(breaks = seq(348,356,1),minor_breaks = seq(348,356,0.25))+
  coord_cartesian(xlim = c(70,105),ylim = c(348,356))+
  geom_hline(yintercept = 350.12, color = "black")+ geom_text(aes(85,350.12, label = "Low Point Elev = 350.12-ft", vjust = -0.5), size = 2.75)+
  geom_hline(yintercept = 350.12+1, color = "red3",linetype = "dashed")+geom_text(aes(85,350.12+1, label = "1-ft overtopping", vjust = -0.5), size = 2.75)+
  geom_hline(yintercept = 350.12+2, color = "orange2",linetype = "dashed")+geom_text(aes(85,350.12+2, label = "2-ft overtopping", vjust = -0.5), size = 2.75)+
  geom_hline(yintercept = 350.12+3, color = "green3",linetype = "dashed")+geom_text(aes(85,350.12+3, label = "3-ft overtopping", vjust = -0.5), size = 2.75)+
  geom_hline(yintercept = 350.12+4, color = "blue2",linetype = "dashed")+geom_text(aes(85,350.12+4, label = "4-ft overtopping", vjust = -0.5), size = 2.75)+
  geom_hline(yintercept = 350.12+5, color = "purple1",linetype = "dashed")+geom_text(aes(85,350.3+5, label = "5-ft overtopping", vjust = -0.5), size = 2.75)

ggsave("E:/1.Thurmond/Chapter 4/Figures/PFMs/OvertoppingTimeline.png",height=6,width=8,dpi=300)

OT_hrs <- data.frame(Depth_ft = c(0.05,1,2,3,4))
OT_hrs <- OT_hrs %>% mutate(Hrs = c(sum(pmf150$Stage_NAVD88 > 350.12+0.05),
                                    sum(pmf150$Stage_NAVD88 > 350.12 + 1),
                                    sum(pmf150$Stage_NAVD88 > 350.12 + 2),
                                    sum(pmf150$Stage_NAVD88 > 350.12 + 3),
                                    sum(pmf150$Stage_NAVD88 > 350.12 + 4)))

## Nathan's PFM
loading <- 0.84

node1 <- mean(c(70,50,80,55,50,60)/100)
exp(mean(log(c(70,50,80,55,50,60)/100)))

node2 <- mean(c(1,0.95,0.9,0.75,0.999,0.9))
exp(mean(log(c(1,0.95,0.9,0.75,0.999,0.9))))

node3 <- 1

node4 <- 0.95

node5 <- 0.85

node6 <- exp(mean(log(c(0.1,0.1,0.05,0.2,0.5,0.45))))

node7 <- mean(c(0.01,0.01,0.01,0.001,0.01))

srp <- node1*node2*node3*node4*node5*node6*node7
#srp <- node1*node2*node3*node4*node5*node6*0.5

apf_x <- c(node1,node2,node3,node4,node5,node6,node7)
apf_y <- (c(node1,node2,node3,node4,node5,node6,node7)*loading)

apf <- loading*srp

# 1.0 PMF
library(zoo)
pmf <- read.csv(file = "E:/1.Thurmond/Chapter 4/Thurmond_PMF.csv",header=T)
pmf <- pmf %>% mutate(Time_hrs = seq(1,length(Inflow_cfs),1),.before = everything())

pmf <- pmf %>% mutate(vol4day = rollmean(Inflow_cfs,96,fill = NA))
pmf_4day <- max(pmf$vol4day,na.rm=T)
round(pmf_4day,-3)

max(pmf$Inflow_cfs)
