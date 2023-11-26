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
                 entropy, 
                 usethis, 
                 GGally, 
                 nhdplusTools, 
                 gridExtra, 
                 ggExtra, 
                 data.table, 
                 viridis)

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
         wshd_area_km2,
         mean_ann_pcpt_mm,
         wshd_max_elevation_m,
         wshd_min_elevation_m,
         mean_ann_runf_mm,
         mean_ann_flow_m3s,
         forest_3scp,
         shrub_3scp,
         human_3scp,
         ht_3,
         simpson_d3,
         accm_totco2_o2g_day
) %>% 
  mutate(log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm),
         log_wshd_max_elev_m = log(wshd_max_elevation_m),
         log_wshd_min_elev_m = if_else(wshd_min_elevation_m == 0,
                                       log(0.0001),
                                       if_else(wshd_min_elevation_m<0,
                                               log(0.0001),
                                               log(wshd_min_elevation_m))),
         log_mean_ann_runf_mm = log(mean_ann_runf_mm),
         log_mean_ann_flow_m3s = log(mean_ann_flow_m3s),
         log_forest_3scp = if_else(forest_3scp == 0,
                                   log(0.0001),
                                   log(forest_3scp)),
         log_shrub_3scp = if_else(shrub_3scp == 0,
                                   log(0.0001),
                                   log(shrub_3scp)),
         log_human_3scp = if_else(human_3scp == 0,
                                   log(0.0001),
                                   log(human_3scp)),
         log_ht_3 =  if_else(ht_3 == 0,
                             log(0.0001),
                             log(ht_3)),
         log_simpson_d3 =  if_else(simpson_d3 == 0,
                             log(0.0001),
                             if_else(simpson_d3 < 0,
                                     log(0.0001),
                                     log(simpson_d3))),
         log_accm_totco2_o2g_day_km2 = log(accm_totco2_o2g_day/wshd_area_km2)) %>% 
  select(basin,
         log_mean_ann_pcpt_mm,
         log_wshd_max_elev_m,
         log_wshd_min_elev_m,
         log_mean_ann_runf_mm,
         log_mean_ann_flow_m3s, 
         log_forest_3scp,
         log_shrub_3scp,
         log_human_3scp,
         log_ht_3,
         log_simpson_d3,
         log_accm_totco2_o2g_day_km2)


summary(correlation_data)
  

corr_mat_pearson <- ggpairs(data = correlation_data,
                            aes(color = basin,
                                alpha = 0.5),
                                c(2:12),
                            upper = list(continuous = wrap("cor",
                                                           method = "pearson")),
                            title = "Correlation matrix (Pearson)")
corr_mat_pearson
ggsave(paste(results_png,"guerrero_etal_23_correlation_matrix_pearson_areal.png",sep = '/'),
       width = 20,
       height = 12,
       units = "in",
       dpi = 300)

# Rank correlations (Spearman)

# Transform the numeric variables to ranks
ranked_data <- correlation_data %>%
  mutate(across(where(is.numeric), rank))

corr_mat_spearman <- ggpairs(data = ranked_data,
                            aes(color = basin,
                                alpha = 0.5),
                            c(2:12),
                            upper = list(continuous = wrap("cor",
                                                           method = "spearman",
                                                           exact = FALSE)),
                            title = "Correlation matrix (Spearman)")
corr_mat_spearman
ggsave(paste(results_png,"guerrero_etal_23_correlation_matrix_spearman_areal.png",sep = '/'),
       width = 20,
       height = 12,
       units = "in",
       dpi = 300)

write.csv(
  correlation_data,
  paste(local_data,
        "guerrero_etal_23_correlation_analysis_dat.csv",
        sep = '/'),
  row.names = FALSE
)
