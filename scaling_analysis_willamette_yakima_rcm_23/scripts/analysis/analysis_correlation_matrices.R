###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# CORRELATION MATRICES AMONG HIGHER SCALE VARIABLES ON HYPORHEIC RESP. 
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis, GGally, nhdplusTools, gridExtra, ggExtra, data.table, viridis, ggtern)

set.seed(2703)

# Local data saving
local_data <- "./data" 

#  Local figure export
results <- "./results" #For svg files that can be explored online

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"

# GitHub import
scaling_analysis_dat <-read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/231008_scaling_analysis_dat.csv",
                        show_col_types = FALSE)


correlation_data <- scaling_analysis_dat %>% 
  select(basin,
         accm_totco2g_day,
         accm_water_exchng_kg_d,
         d50_m,
         mean_ann_runf_mm,
         mean_ann_flow_m3s,
         mean_ann_pcpt_mm) %>% 
  mutate(log_accm_totco2g_day = log(accm_totco2g_day),
         log_accm_water_exchng_kg_d = log(accm_water_exchng_kg_d),
         log_d50_m = log(d50_m),
         log_mean_ann_runf_mm = log(mean_ann_runf_mm),
         log_mean_ann_flow_m3s = log(mean_ann_flow_m3s),
         log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm)) %>% 
  select(basin,
         log_accm_totco2g_day,
         log_accm_water_exchng_kg_d,
         log_d50_m,
         log_mean_ann_runf_mm,
         log_mean_ann_flow_m3s,
         log_mean_ann_pcpt_mm)
  

corr_mat_pearson <- ggpairs(data = correlation_data,
                            aes(color = basin,
                                alpha = 0.5),
                                c(2:7),
                            upper = list(continuous = wrap("cor",
                                                           method = "pearson")),
                            title = "Correlation matrix (Pearson)")
corr_mat_pearson
ggsave(paste(results_png,"correlation_matrix_pearson.png",sep = '/'),
       width = 20,
       height = 12,
       units = "in",
       dpi = 300)


corr_mat_spearman <- ggpairs(data = correlation_data,
                            aes(color = basin,
                                alpha = 0.5),
                            c(2:7),
                            upper = list(continuous = wrap("cor",
                                                           method = "spearman",
                                                           exact = FALSE)),
                            title = "Correlation matrix (Spearman)")
corr_mat_spearman
ggsave(paste(results_png,"correlation_matrix_spearman.png",sep = '/'),
       width = 20,
       height = 12,
       units = "in",
       dpi = 300)


