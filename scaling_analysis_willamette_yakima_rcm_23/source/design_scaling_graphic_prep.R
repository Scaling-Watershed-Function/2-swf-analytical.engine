###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River Basins
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: RIVER CORRIDOR MODEL (By Kyongho Son and Yilin Fang). Slope and D50 
# data were constrained by Downstream Hydraulic Geometry (DHG) and then used in a 
# Random Forest Model to fill gaps in hyporheic hydraulics (lateral and vertical 
# exchange fluxes and residence times). This new data were coupled with corresponding
# substrate concentrations and fed into the RCM for prediction of both aerobic and
# anaerobic respiration

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md

#################################################################################
# Loading packages

librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 ggExtra,#adding marginal distributions (does not work with facet_wrap)
                 gridExtra,#allows to combine plots on a customizable grid
                 gridtext,#allows you to add rich text to gridExtra plots
                 grid,#allows you to add complex axis titles to gridExtra plots
                 ggdist,# potentially working with facet_wrap
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable,# To manipulate ggplot objects
                 car,#partial residual plots
                 caret,#variable import
                 MuMIn, #ensemble models for partial residuals
                 ggeffects,
                 sp,#reading shape files
                 sf,# reading shape files
                 leaflet,#creating html widget maps
                 fedmatch,# customizable merge
                 svglite,#save plots as svg files (lighter?)
                 ggthemes, #uploading different themes for plots
                 knitr,#compiling r-makrdown/quarto docs
                 nhdplusTools)#across the network calculation

#################################################################################
# file paths

local_data <- "./data"

results <- "./results"

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"

##################################################################################
# Loading data

scaling_analysis_dat <- read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/231008_scaling_analysis_dat.csv",
                                 show_col_types = FALSE)

################################################################################
# plot themeing

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

breaks <- 10^(-10:20)
breaks_c <- 10^seq(-10,20,by=4)
minor_breaks <- rep(1:9, 31)*(10^rep(-10:20, each=9))

set.seed(2703)


# Creating a quasi-sequential color palette for discrete categories
# Source: https://www.ibm.com/design/language/color/

my_dcolors <- c("#dae6ff",
                "#c0e2ff",
                "#82cfff",
                "#33b1ff",
                "#1192e8",
                "#0072c3",
                "#00539a",
                "#003a6d",
                "#012749",
                "#061727")

my_mcolors <- c("#fff0f7",
                "#ffd6e8",
                "#ffafd2",
                "#ff7eb6",
                "#ee5396",
                "#d62670",
                "#9f1853",
                "#740937",
                "#510224",
                "#2a0a18")

#NLCD color palette
set.seed(2703)

# Landscape colors from nlcd color scale
nlcd_colors_c <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")
nlcd_colors_w <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")

# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv

# NLDC Categories
nlcd_cat_c <- c("c_water_scp","c_human_scp","c_barren_scp","c_forest_scp","c_shrub_scp","c_grass_scp")
nlcd_cat_w <- c("w_water_scp","w_human_scp","w_barren_scp","w_forest_scp","w_shrub_scp","w_grass_scp")

# Assigning names to a color scale
names(nlcd_colors_c) <- nlcd_cat_c
names(nlcd_colors_w) <- nlcd_cat_w

################################################################################

#Kernel density estimation function: bandwidth for density plots
bwf <- function(x){bw.SJ(x)}

# Calculating Quantiles

#Calculating the quantiles with Hmisc::cut2, which allows for the inclusion of zeroes

# https://stackoverflow.com/questions/46750635/cut-and-quantile-in-r-in-not-including-zero

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80","Q90","Q100")

# assigning names to color scale
names(my_dcolors) <- qlabel
names(my_mcolors) <- qlabel




