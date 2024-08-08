# Unit Hydrograph Peaking

# Clear workspace
rm(list = ls(all.names = TRUE))
library(tidyverse)

# Read unpeaked UHG
unpeaked2014 <- read.csv("E:/1.Thurmond/PMP_PMF/2014 Peaking/UHG_Unpeaked_2014.csv")

# Read 2014 peaked UHG
peaked2014 <- read.csv("E:/1.Thurmond/PMP_PMF/2014 Peaking/UHG_Peaked_2014.csv")

# Read 2024 peaked UHG
peaked2024 <- read.csv("E:/1.Thurmond/PMP_PMF/2014 Peaking/UHG_Peaked_2024.csv")

# Combine
uhg <- data.frame(Time_hrs = peaked2014$Time_hrs,
                       Unpeaked_cfs = unpeaked2014$Inflow_cfs,
                       Peaked14_cfs = peaked2014$Inflow_cfs,
                       Peaked24_cfs = peaked2024$Inflow_cfs)
design_uh <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/RFA Hydrographs/Hourly/Design UH.csv")

# plot modeled vs observed flow
flow.colors <- c("2014 Unpeaked" = "#333BFF", "2014 Peaked (25%)" = "orangered2","2024 Peaked (50%)" = "green2","Design UH" = "black")

# plot
uh_1.5 <- ggplot(data = uhg)+
  geom_line(aes(x=Time_hrs,y=Peaked24_cfs))+
  #geom_line(data = design_uh,aes(x=Hour,y=Inflow,color = "Design UH"))+
  theme_bw()+ 
  #ggtitle("Unit Hydrograph Peaking")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,216,24),minor_breaks = seq(0,216,12))+
  scale_y_continuous(breaks = seq(0,60000,10000),minor_breaks = seq(0,60000,1000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

#ggsave("E:/1.Thurmond/Chapter 4/Figures/PMF/UH Peaking.png",
#       uhpeaking_fig,width = 11, height = 8.5,dpi = 300)

uhpeaking_fig <- ggplot(data = uhg)+
  geom_line(aes(x=Time_hrs,y=Unpeaked_cfs,color = "2014 Unpeaked"))+
  geom_line(aes(x=Time_hrs,y=Peaked14_cfs,color = "2014 Peaked (25%)"))+
  geom_line(aes(x=Time_hrs,y=Peaked24_cfs,color = "2024 Peaked (50%)"))+
  #geom_line(data = design_uh,aes(x=Hour,y=Inflow,color = "Design UH"))+
  theme_bw()+ 
  #ggtitle("Unit Hydrograph Peaking")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,216,24),minor_breaks = seq(0,216,12))+
  scale_y_continuous(breaks = seq(0,60000,10000),minor_breaks = seq(0,60000,1000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

# ggsave("E:/1.Thurmond/Chapter 4/Figures/PMF/UH Peaking.png",
#        uhpeaking_fig,width = 11, height = 8.5,dpi = 300)

# peak UH flow
max_unpeak <- max(uhg$Unpeaked_cfs)
max_peak2014 <- max(uhg$Peaked14_cfs)
max_peak2024 <- max(uhg$Peaked24_cfs)

max_peak2014/max_unpeak
max_peak2024/max_unpeak

# pmf plot
pmfs <- read.csv("E:/1.Thurmond/PMP_PMF/2014 Peaking/PMF_Results.csv")
sdf <- read.csv("E:/1.Thurmond/Chapter 4/HH/Data/RFA Hydrographs/Hourly/SDF.csv")
sdf <- sdf %>% mutate(Time_hrs = Hour + 24)

# plot
flow.colors <- c("2014 Unpeaked PMF" = "#333BFF", "2014 Peaked PMF (25%)" = "orangered2","2024 Peaked PMF (50%)" = "green2", "Design SDF" = "black")

pmfs_inflow <- ggplot(data = pmfs)+
  geom_line(aes(x=Time_hrs,y=Unpeaked_inflow,color = "2014 Unpeaked PMF"))+
  geom_line(aes(x=Time_hrs,y=Peaked25_inflow,color = "2014 Peaked PMF (25%)"))+
  geom_line(aes(x=Time_hrs,y=Peaked50_inflow,color = "2024 Peaked PMF (50%)"))+
  geom_line(data = sdf, aes(x=Time_hrs,y=Inflow,color = "Design SDF"))+
  theme_bw()+ 
  #ggtitle("PMF Peaking")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,216,24),minor_breaks = seq(0,216,12))+
  scale_y_continuous(breaks = seq(0,1300000,250000),minor_breaks = seq(0,1300000,50000))+
  labs(x="Time (hours)",y="Discharge(cfs)",color = "Legend")

ggsave("E:/1.Thurmond/Chapter 4/Figures/PMF/PMF Peaking - Inflows.png",
       pmfs_inflow,width = 11, height = 8.5,dpi = 300)

# plot
pmfs <- pmfs %>% mutate(Unpeaked_navd88 = Unpeaked_Elev-0.7, 
                        Peaked25_navd88 = Peaked25_Elev-0.7, 
                        Peaked50_navd88 = Peaked50_Elev-0.7,
                        P50_unpK_Elev88 = P50_unpK_Elev - 0.7)

pmfs_elev <- ggplot(data = pmfs)+
  geom_line(aes(x=Time_hrs,y=Unpeaked_Elev,color = "2014 Unpeaked PMF"))+
  geom_line(aes(x=Time_hrs,y=Peaked25_Elev,color = "2014 Peaked PMF (25%)"))+
  geom_line(aes(x=Time_hrs,y=Peaked50_Elev,color = "2024 Peaked PMF (50%)"))+
  theme_bw()+ 
  ggtitle("PMF Peaking")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,216,24),minor_breaks = seq(0,216,12))+
  scale_y_continuous(breaks = seq(330,351,1),minor_breaks = seq(330,351,0.5))+
  geom_hline(yintercept = 350.3, color="black")+
  geom_text(aes(x=36, label="\nTop of Dam = 350.3 ft-NAVD88"), y=350.3, colour="black", angle=0,size=3)+
  geom_hline(yintercept = 345.6, color="black")+
  geom_text(aes(x=36, label="\nDesign SDF Elev = 345.6 ft-NAVD88"), y=345.6, colour="black", angle=0,size=3)+
  labs(x="Time (hours)",y="Pool Elevation (ft-NAVD88)",color = "Legend")

ggsave("E:/1.Thurmond/Chapter 4/Figures/PMF/PMF Peaking - Elevs.png",
       pmfs_elev,width = 11, height = 8.5,dpi = 300)

max(pmfs$Unpeaked_Elev)
max(pmfs$Peaked25_Elev)
max(pmfs$Peaked50_Elev)

max(pmfs$Peaked50_Elev) - max(pmfs$Peaked25_Elev)


ggplot(data = pmfs)+
  geom_line(aes(x=Time_hrs,y=Unpeaked_Elev,color = "2014 Unpeaked PMF"))+
  geom_line(aes(x=Time_hrs,y=Peaked25_Elev,color = "2014 Peaked PMF (25%)"))+
  geom_line(aes(x=Time_hrs,y=Peaked50_Elev,color = "2024 Peaked PMF (50%)"))+
  geom_line(aes(x=Time_hrs,y=P50_unpK_Elev88,color = "Not Peaked K"))+
  theme_bw()+ 
  ggtitle("PMF Peaking")+
  scale_color_manual(values = flow.colors)+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  scale_x_continuous(breaks = seq(0,216,24),minor_breaks = seq(0,216,12))+
  scale_y_continuous(breaks = seq(330,351,1),minor_breaks = seq(330,351,0.5))+
  geom_hline(yintercept = 350.3, color="black")+
  geom_text(aes(x=36, label="\nTop of Dam = 350.3 ft-NAVD88"), y=350.3, colour="black", angle=0,size=3)+
  geom_hline(yintercept = 345.6, color="black")+
  geom_text(aes(x=36, label="\nDesign SDF Elev = 345.6 ft-NAVD88"), y=345.6, colour="black", angle=0,size=3)+
  labs(x="Time (hours)",y="Pool Elevation (ft-NAVD88)",color = "Legend")
