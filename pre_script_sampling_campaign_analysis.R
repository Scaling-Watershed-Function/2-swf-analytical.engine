###############################################################################
# Scaling Analysis for In Situ Respiration Rates across the Yakima River Basin
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md
gc()

rm()

librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable,# To manipulate ggplot objects
                 car,#partial residual plots
                 caret,#variable import
                 MuMIn, #ensemble models for partial residuals
                 ggeffects,
                 fedmatch)# customizable merge

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())


# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

set.seed(2703)

#Data:

# External data import

heading_dat_path <- "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/processed/230429_dd_basin_char_hydr_geom_yrb_wrb.csv"
bgc_dat_path <- "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/processed/230507_bgc_dat_c10.csv"
spat_study_path <- "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/combined_results_updated_04262023.csv"

heading_dat <- read_csv(heading_dat_path,show_col_types = FALSE)
bgc_dat_c10 <- read_csv(bgc_dat_path, show_col_types = FALSE)
spat_study <- read_csv(spat_study_path, show_col_types = FALSE)


# Local import and export paths (when clonning the entire project)

# raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
# processed_data <- "../1-swf-knowledge.base/assets/data/processed"
# assets_figs <- "../1-swf-knowledge.base/assets/plots"
# 
# #header info (data dictionary)
# 
# heading_dat <- read_csv(paste(processed_data,"guerrero_etal_swf_dd.csv", sep = '/'),
#                         show_col_types = FALSE)
# 
# #values
# bgc_dat_c10 <- read_csv(paste(processed_data,"230507_bgc_dat_c10.csv", sep = "/"),
#                         show_col_types = FALSE)
# 
# spat_stdy <- read_csv(paste(raw_data,"combined_results_updated_04262023.csv", sep = "/"),
#                       show_col_types = FALSE)

# Merging bgc data with the spatial study dataset

ykm_spc <- spat_study %>% 
  mutate(tot_o2_consump_gm2day = -ERsed_gm2day) %>% 
  rename(comid = COMID.y) %>% 
  merge(.,
        bgc_dat_c10,
        by = "comid",
        all.x = TRUE) %>% 
  select(-COMID.x) %>% 
  filter(ERsed_gm2day < 0)

# Quick exploratory plots

# checking relationship between o2 consumption and streambed area

o2_area_mod <- lm(tot_o2_consump_gm2day~stream_area_m2,
                   data = ykm_spc)
summary(o2_area_mod)

# R squared is 0.27 and p value < 0.0001

plot(o2_area_mod) #regression diagnostic plots
# Observations 3, 6, and 37 seem to be highly influential on the regression plot

inf_obs <- ykm_spc %>% 
  slice(1,3,6,37)
inf_obs

# Running model without influential observations
o2_area_mod_ni <- lm(tot_o2_consump_gm2day~wshd_area_km2,
                     data = ykm_spc[-1,])
summary(o2_area_mod_ni)
plot(o2_area_mod_ni)

# R squared drops to 0.05 and p value is 0.12

# Thus the trend in the plot for watershed area should exclude these influential observations: 

# Let's start with the multiple regression plot

o2_mr_mod <- lm(tot_o2_consump_gm2day ~ log(mean_depth_m) + log(velocity_ms) + log(wshd_area_km2) + slope,
               data = ykm_spc[-1,])


summary(o2_mr_mod)
crPlots(o2_mr_mod)
vif(o2_mr_mod)

o2_mr_mod2 <- lm(tot_o2_consump_gm2day ~ log(mean_depth_m) + log(velocity_ms) + log(wshd_area_km2) + slope,
                data = ykm_spc)


summary(o2_mr_mod2)
crPlots(o2_mr_mod2)
vif(o2_mr_mod2)

# Bringing back in the influential variables into a multiple regression analysis does not change the results

# Let's plot respiration rates vs. watershed area, but excluding the influential observation from the trend

# Running model without influential observations (watershed area)
o2_warea_mod_ni <- lm(tot_o2_consump_gm2day~wshd_area_km2,
                     data = ykm_spc[-1,])
summary(o2_warea_mod_ni)
plot(o2_warea_mod_ni)

o2_stream_area_ni <- ggplot(data =  ykm_spc[-c(1,3,6),],
                            aes(x = wshd_area_km2,
                                y = tot_o2_consump_gm2day))+
  geom_smooth(method = "lm")+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
o2_stream_area_ni
  