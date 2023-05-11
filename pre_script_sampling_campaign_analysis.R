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

heading_dat_path <- 

heading_dat <- read_csv(url = "https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/230429_dd_basin_char_hydr_geom_yrb_wrb.csv",
                        show_col)



# Local import and export paths

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"
assets_figs <- "../1-swf-knowledge.base/assets/plots"

#header info (data dictionary)

heading_dat <- read_csv(paste(processed_data,"guerrero_etal_swf_dd.csv", sep = '/'),
                        show_col_types = FALSE)

#values
bgc_dat_c10 <- read_csv(paste(processed_data,"230507_bgc_dat_c10.csv", sep = "/"),
                        show_col_types = FALSE)

spat_stdy <- read_csv(paste(raw_data,"combined_results_updated_04262023.csv", sep = "/"),
                      show_col_types = FALSE)
