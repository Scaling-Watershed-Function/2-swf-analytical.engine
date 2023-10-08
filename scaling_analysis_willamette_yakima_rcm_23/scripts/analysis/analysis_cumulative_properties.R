###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# CUMULATIVE CALCULATIONS ACROSS THE STREAM NETWORKS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis, GGally)

set.seed(2703)

# Data

# Local data saving
local_data <- "./data" 

#  Local figure export
results <- "./results" #For svg files that can be explored online

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"

# GitHub import
wshd_lnd_dat <- filter(read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/231008_landscape_heterogeneity_pnw.csv",
                                show_col_types = FALSE), level == "watershed")
