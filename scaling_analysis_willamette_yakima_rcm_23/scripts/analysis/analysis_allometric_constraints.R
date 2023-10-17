################################################################################
# SCALING WATERSHED FUNCTION: Allometric Constraint Analysis - Three approaches
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table)

kb_datasets <- "../../1-swf-knowledge.base/datasets/raw_data/cross_walk_nhdplus_hu12/data"
local_data <- "./data"
source("./source/function_allometric_analysis_area_based.R")
source("./source/function_allometric_analysis_huc_12.R")
source("./source/function_allometric_constraint_huc_12.R")

scaling_analysis_dat <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                                    show_col_types = FALSE)

randomized_paths <- read_csv(paste(local_data,"231014_randomized_paths_allometric_constraint.csv", sep = "/"),
                             show_col_types = FALSE)

huc_12_dat <- read_csv(paste(kb_datasets,"cross_walk_nhdplus_hu12.csv", sep = "/"),
                       show_col_types = FALSE) %>% 
  select(comid,
         HUC_12) %>% 
  rename(huc_12 = HUC_12)

# Merging huc_12 and scaling_analysis_dat

scaling_huc12_dat <- scaling_analysis_dat %>% 
  merge(.,
        huc_12_dat,
        by = "comid",
        all.x = TRUE) %>% 
  mutate(huc_12 = as.factor(huc_12))

# Merging randomized paths and scaling analysis dat

rand_paths_huc12_dat <- randomized_paths %>% 
  select(comid,
         path_number) %>% 
  merge(.,
        scaling_huc12_dat,
        by = "comid",
        all.x = TRUE)

# Method 1 Resampling of scaling_analysis_dat and randomized paths and estimation of slopes and bootstrapped CI

resampling_dat <- scaling_analysis_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 1000))

rand_paths_dat <- rand_paths_huc12_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 1000))

# Method 2: Resampling of randomized huc 12 and estimation of slopes and CI based on highest R-squared values

results_df <- allometric_constraint_huc_12(scaling_huc12_dat,
                                           n = 500, 
                                           sample_size = 100,
                                           x_col = "wshd_area_km2",
                                           y_col = "accm_totco2_o2g_day")

# Method 3: Resampling of randomized huc 12 and estimation of slopes and CI based 
# on cdf area distribution and max, med, min values

allometric_huc12_data <- scaling_huc12_dat %>% 
  group_by(basin) %>% 
  do(allometric_analysis_huc_12(.,
                             n = 1000, 
                             x_col = "wshd_area_km2",
                             y_col = "accm_totco2_o2g_day"))
