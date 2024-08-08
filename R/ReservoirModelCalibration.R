### Reservoir Model Calibration for RFA and HMS update
library(tidyverse)
# Above 344, use 2014 HMS discharges
# Below 337, use RFA calibrated discharge
rm(list = ls(all.names = TRUE))

source("E:/R/theme_USACE.r")

library(tidyverse)

# Read CSV
resmodels <- read.csv("E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/ResModelCalibration.csv",header=T)

hms2014 <- resmodels %>% filter(Curve == "HMS2014")
atr2024 <- resmodels %>% filter(Curve == "ATR2024_1")
atr2_2024 <- resmodels %>% filter(Curve == "ATR2024_2")
pa2024 <- resmodels %>% filter(Curve == "PA2024Draft")
#fullyopen <- resmodels %>% filter(Curve == "Gates Fully Open")
open_1ft <- resmodels %>% filter(Curve == "open_1ft")
open_10ft <- resmodels %>% filter(Curve == "open_10ft")
fully_open <- resmodels %>% filter(Curve == "fully_open")

# Plot first
ggplot(data = resmodels, aes(x=Elevation_ftNAVD88, y=Discharge_CFS,color=Curve))+
  geom_line()+
  geom_point()+
  theme_USACE()+
  scale_x_continuous(breaks = seq(330.3,360.3,5),minor_breaks = seq(330.3,360.3,1),limits = c(330,360))+
  labs(x="Elevation (ft-NAVD88)",y="Discharge (cfs)")

resmodels %>% filter(Curve == "HMS2014" | Curve == "open_1ft"|Curve == "open_10ft"|Curve == "fully_open") %>%
  ggplot(aes(x=Elevation_ftNAVD88, y=Discharge_CFS,color=Curve))+
  geom_line()+
  geom_point()+
  theme_USACE()+
  scale_x_continuous(breaks = seq(330.3,360.3,5),minor_breaks = seq(330.3,360.3,1),limits = c(330,360))+
  labs(x="Elevation (ft-NAVD88)",y="Discharge (cfs)")

# Now take HMS data above 344 and rfa below 336.3
discharge <- bind_rows(atr2_2024 %>% filter(Elevation_ftNAVD88 < 337),
                       hms2014 %>% filter(Elevation_ftNAVD88 >=344),
                       atr2024 %>% filter(Elevation_ftNAVD88 >359))

# Overtopping
damlength <- 5658
spillway_gross_L <- 1096
tod <- 350.3
coef <- 2.65

effectiveL <- damlength - spillway_gross_L

elev29 <- c(348,349,350,351,352,353,354,355,356,360)
elev88 <- elev29 - 0.7

overtopping <- data.frame(Elev88 = elev88,OT_depth = NA,Discharge = NA)

overtopping <- overtopping %>% mutate(OT_depth = ifelse((Elev88 - tod) < 0,0,(elev88 - tod)))
overtopping <- overtopping %>% mutate(Discharge = coef*effectiveL*(OT_depth^1.5))
overtopping <- overtopping %>% filter(Elev88 > 350)
maxOT <- max(overtopping$Discharge)
maxSpill <- max(discharge$Discharge_CFS)

# Match Curve to Reservoir Storage Values
storage_elev <- read.csv("E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Storage_navd88.csv",header=T)
elevations <-as.numeric(storage_elev$Elev_NAVD88)

# approx points from discharge
est_Q <- approx(x=discharge$Elevation_ftNAVD88,y=discharge$Discharge_CFS,xout=elevations,method="linear")
est_Q$y

# Create Res Model DF
res_model_navd88 <- storage_elev %>% mutate(Discharge_cfs = est_Q$y)
res_model_navd88$Discharge_cfs[res_model_navd88$Elev_NAVD88==350.3] = 1060000

# ggplot(data = resmodels, aes(x=Elevation_ftNAVD88, y=Discharge_CFS,color=Curve))+
#   geom_line()+
#   geom_point()+
#   geom_line(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
#   geom_point(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
#   theme_USACE()+
#   scale_x_continuous(breaks = seq(330.3,360.3,5),minor_breaks = seq(330.3,360.3,1),limits = c(330,360))+
#   labs(x="Elevation (ft-NAVD88)",y="Discharge (cfs)")

resmodels %>% filter(Curve == "HMS2014" | Curve == "open_2ft"|Curve == "open_10ft"|Curve == "fully_open") %>%
  ggplot(aes(x=Elevation_ftNAVD88, y=Discharge_CFS,color=Curve))+
  geom_line()+
  #geom_point()+
  geom_line(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
  geom_point(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
  theme_USACE()+
  scale_x_continuous(breaks = seq(295.3,360.3,5),minor_breaks = seq(295.3,360.3,1))+
  coord_cartesian(xlim = c(330,360))+
  labs(x="Elevation (ft-NAVD88)",y="Discharge (cfs)")+
  scale_color_manual(values = c("red","blue","orange2","#2ca02c"),labels = c("Fully Open (34-ft)","2014 HMS Model","Gates Open 10-ft","Gates Open 1-ft"),name = "Legend")
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/Elev-Discharge-Sensit.png",dpi=300,height = 6,width=8)

resmodels %>% filter(Curve == "HMS2014" | Curve == "open_2ft"|Curve == "open_10ft"|Curve == "fully_open") %>%
  ggplot(aes(x=Elevation_ftNAVD88, y=Discharge_CFS,color=Curve))+
  geom_line()+
  #geom_point()+
  #geom_line(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
  #geom_point(data=res_model_navd88,aes(x=Elev_NAVD88 , y=Discharge_cfs),color="black",size=1)+
  theme_USACE()+
  scale_x_continuous(breaks = seq(295.3,360.3,5),minor_breaks = seq(295.3,360.3,1))+
  coord_cartesian(xlim = c(330,360))+
  labs(x="Elevation (ft-NAVD88)",y="Discharge (cfs)")+
  scale_color_manual(values = c("red","blue","orange2","#2ca02c"),labels = c("Fully Open (34-ft)","2014 HMS Model","Gates Open 10-ft","Gates Open 1-ft"),name = "Legend")
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/Elev-Discharge-Sensit_NO Expected.png",dpi=300,height = 6,width=8)

res_model_navd88 <- res_model_navd88 %>% mutate(Discharge_cfs = round(Discharge_cfs,0))
res_model_navd88 <- res_model_navd88 %>% mutate(Storage_acft = round(Storage_acft,0))

# Export Curve
write.csv(discharge,"E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Final_Elev-Discharge.csv",row.names = F)
write.csv(res_model_navd88,"E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Final_ResModel.csv",row.names = F)

# Export Sensitivity (gate opening) Curves
sensit_elev <- fully_open$Elevation_ftNAVD88

est_V <- approx(x=storage_elev$Elev_NAVD88,y=storage_elev$Storage_acft,xout=sensit_elev,method="linear")

open_1ft <- open_1ft %>% mutate(Storage = est_V$y,.before=Discharge_CFS)
open_10ft <- open_10ft %>% mutate(Storage = est_V$y,.before=Discharge_CFS)
fully_open <- fully_open %>% mutate(Storage = est_V$y,.before=Discharge_CFS)

write.csv(open_1ft,"E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Sensitivity/1-ft_open.csv",row.names = F)
write.csv(open_10ft,"E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Sensitivity/10-ft_open.csv",row.names = F)
write.csv(fully_open,"E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Sensitivity/34-ft_open.csv",row.names = F)

# RFA Sensitivity
rfa_results <- read.csv("E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Sensitivity/RFA_Sensitivity.csv",header=T)
stage_obs <- read.csv("E:/1.Thurmond/Chapter 4/HH/RFA/InputData/ResModel/Sensitivity/RFA_Stage_points.csv",header=T)

rfa_results <- rfa_results %>% mutate(ANEP = 1-AEP,
                                      Z = qnorm(ANEP, mean = 0, sd = 1, lower.tail = TRUE),
                                      Gumble = -log(-log(ANEP)),
                                      .before = Elev_NAVD88)

stage_obs <- stage_obs %>% mutate(ANEP = 1-AEP,
                                  Z = qnorm(ANEP, mean = 0, sd = 1, lower.tail = TRUE),
                                  Gumble = -log(-log(ANEP)),
                                  .before = Elev_NAVD88,)
simulation_colors <- c(
  "Expected" = "black",    # Blue
  "2014 HMS" = "blue",        # Orange
  "Gates Open 1-ft" = "#2ca02c", # Green
  "Gates Open 10-ft" = "orange2",# Red
  #  "Gates Open 30-ft" = "purple",
  "Gates Fully Open (34-ft)" = "#d62728",
  "5-day" = "cyan")

minor_breaks <- function(brks) {
  # Generate minor breaks between the major breaks
  minor <- unlist(lapply(seq_along(brks[-length(brks)]), function(i) {
    seq(brks[i], brks[i+1], length.out = 10)[-c(1, 10)]
  }))
  return(minor)
}

brks <- c(0.99,0.9,0.5,0.1,0.01,0.001,1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10)
zbrks <-  qnorm(1-brks, mean = 0, sd = 1, lower.tail = TRUE)

ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation != "Gates Open 30-ft"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/RFA-Sensit.png",dpi=300,height = 6,width=8)

ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation != "Expected" & Simulation != "Gates Open 30-ft"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/RFA-Sensit_NO Expected.png",dpi=300,height = 6,width=8)

# HMS
ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation == "2014 HMS"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.8,.35))

# 1-ft open
ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Open 1-ft"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.8,.35))

# 10-ft open
ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Open 10-ft"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.8,.35))


# Fully Open
ggplot()+
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Fully Open (34-ft)"),aes(x=Z,y=Elev_NAVD88,color = Simulation))+
  geom_point(data = stage_obs,aes(x=Z,y=Elev_NAVD88))+
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))

# Filter data for Gates Fully Open (34-ft) and Gates Open 1-ft
gates_fully_open <- rfa_results %>% filter(Simulation == "Gates Fully Open (34-ft)")
gates_open_1ft <- rfa_results %>% filter(Simulation == "Gates Open 1-ft")

ggplot() +
  geom_ribbon(data = gates_fully_open, aes(x = Z, ymin = Elev_NAVD88, ymax = gates_open_1ft$Elev_NAVD88), fill = "grey80", alpha = 0.5) +
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Fully Open (34-ft)" | Simulation == "Gates Open 1-ft"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_line(data = rfa_results %>% filter(Simulation == "Expected"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_point(data = stage_obs, aes(x = Z, y = Elev_NAVD88)) +
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/Full_1ft.png",dpi=300,height = 6,width=8)

# Filter data for Gates Fully Open (34-ft) and Gates Open 10-ft
gates_fully_open <- rfa_results %>% filter(Simulation == "Gates Fully Open (34-ft)")
gates_open_10ft <- rfa_results %>% filter(Simulation == "Gates Open 10-ft")

ggplot() +
  geom_ribbon(data = gates_fully_open, aes(x = Z, ymin = Elev_NAVD88, ymax = gates_open_10ft$Elev_NAVD88), fill = "grey80", alpha = 0.5) +
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Fully Open (34-ft)" | Simulation == "Gates Open 10-ft"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_line(data = rfa_results %>% filter(Simulation == "Expected"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_point(data = stage_obs, aes(x = Z, y = Elev_NAVD88)) +
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/Full_10ft.png",dpi=300,height = 6,width=8)

# Filter data for Gates Open 30-ft and Gates Open 1-ft
gates_fully_open <- rfa_results %>% filter(Simulation == "Gates Open 30-ft")
gates_open_1ft <- rfa_results %>% filter(Simulation == "Gates Open 1-ft")

ggplot() +
  geom_ribbon(data = gates_fully_open, aes(x = Z, ymin = Elev_NAVD88, ymax = gates_open_1ft$Elev_NAVD88), fill = "grey80", alpha = 0.5) +
  geom_line(data = rfa_results %>% filter(Simulation == "Gates Open 30-ft" | Simulation == "Gates Open 1-ft"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_line(data = rfa_results %>% filter(Simulation == "Expected"),
            aes(x = Z, y = Elev_NAVD88, color = Simulation)) +
  geom_point(data = stage_obs, aes(x = Z, y = Elev_NAVD88)) +
  theme_USACE()+
  scale_y_continuous(breaks = seq(299.3,360.3,5),minor_breaks = seq(299.3,360.3,1))+
  coord_cartesian(ylim=c(299.3,355.3))+
  scale_x_continuous(breaks = zbrks,labels = brks,minor_breaks = minor_breaks(zbrks))+
  scale_color_manual(values = simulation_colors)+
  labs(y = "Elevation (ft-NAVD88)",x="AEP")+
  geom_hline(yintercept = 350.3,size=1)+
  theme(legend.position = c(.7,.35))
ggsave("E:/1.Thurmond/Chapter 4/Figures/RFA/30ft_1ft.png",dpi=300,height = 6,width=8)
