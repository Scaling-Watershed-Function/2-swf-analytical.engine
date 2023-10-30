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
results <- "./results"
source("./source/function_allometric_analysis_area_based.R")
source("./source/function_allometric_analysis_huc_12.R")
source("./source/function_allometric_constraint_huc_12.R")


all_paths_dat <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                                    show_col_types = FALSE)

rand_paths_dat <- read_csv(paste(local_data,"231014_randomized_paths_allometric_constraint.csv", sep = "/"),
                             show_col_types = FALSE)

huc_12_dat <- read_csv(paste(kb_datasets,"cross_walk_nhdplus_hu12.csv", sep = "/"),
                       show_col_types = FALSE) %>% 
  select(comid,
         HUC_12) %>% 
  rename(huc_12 = HUC_12)

# Merging huc_12 and scaling_analysis_dat

all_paths_huc_12_dat <- all_paths_dat %>% 
  merge(.,
        huc_12_dat,
        by = "comid",
        all.x = TRUE) %>% 
  mutate(huc_12 = as.factor(huc_12)) %>% 
  distinct(comid,tocomid,huc_12,.keep_all = TRUE)

# Merging randomized paths and scaling analysis dat

rand_paths_huc_12_dat <- rand_paths_dat %>% 
  select(comid,
         path_number) %>% 
  merge(.,
        all_paths_huc_12_dat,
        by = "comid",
        all.x = TRUE) %>% 
  distinct(comid, tocomid, .keep_all = TRUE) 

# Subsetting all_paths_huc_12_dat by unique values of huc_12
all_paths_unique_huc_12_dat <- all_paths_huc_12_dat %>% 
  distinct(huc_12,.keep_all = TRUE)

# Subsetting rand_paths_huc_12_dat by unique values of huc_12
rand_paths_unique_huc_12_dat <- rand_paths_huc_12_dat %>% 
  distinct(huc_12,.keep_all = TRUE)


# Method 1 Resampling of scaling_analysis_dat and randomized paths and estimation of slopes and bootstrapped CI

results <- all_paths_huc_12_dat %>%
  group_by(basin) %>%
  summarise(allometric_results = list(allometric_analysis(., 
                                                          x_col = "wshd_area_km2",
                                                          y_col = "accm_totco2_o2g_day",
                                                          n = 500,
                                                          best_iterations = TRUE)), 
            .groups = 'drop')


results$allometric_results[[2]]

constraint_all_paths_huc_12 <- all_paths_huc_12_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 10,
                         best_iterations = TRUE))
constraint_all_paths_huc_12$method = "Bootstrap all paths"

constraint_rand_paths_huc_12 <- rand_paths_huc_12_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 1000))
constraint_rand_paths_huc_12$method = "Bootstrap random paths"


constraint_all_paths_unique_huc_12_dat <- all_paths_unique_huc_12_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 1000))
constraint_all_paths_unique_huc_12_dat$method =  "Bootstrap all paths unique HUC-12"

# This dataset is too narrow and the analysis results in multiple warnings about
# not enough data to make some calculations (i.e., not enough area range). It also
# return errors due to multiple NAs in RMSE

constraint_rand_paths_unique_huc_12_dat <- rand_paths_unique_huc_12_dat %>%
  group_by(basin) %>%
  do(allometric_analysis(., x_col = "wshd_area_km2",
                         y_col = "accm_totco2_o2g_day",
                         n = 1000))
constraint_rand_paths_unique_huc_12_dat$method =  "Bootstrap random paths unique HUC-12"

# This dataset is too narrow and the analysis results in multiple warnings about
# not enough data to make some calculations (i.e., not enough area range) It also
# return errors due to multiple NAs in RMSE 

constraints_results <- rbind(constraint_all_paths_huc_12,
                             constraint_rand_paths_huc_12) 

colnames(constraints_results)=c("Basin",
                                 "Constraint",
                                 "Average scaling exponent",
                                 "Scaling exponent lower c.i.",
                                 "Scaling exponent upper c.i.",
                                 "Average intercept",
                                 "Intercept lower c.i.",
                                 "Intercept upper c.i.",
                                 "Average R-Squared",
                                 "R-Squared lower c.i.",
                                 "R-Squared upper c.i.",
                                 "Average R-Squared",
                                 "R-Squared lower c.i.",
                                 "R-Squared upper c.i.",
                                 "Bootstraping method")


# Method 2: Resampling of randomized huc 12 and estimation of slopes and CI based 
# on highest R-squared values

results_df <- allometric_constraint_huc_12(all_paths_huc_12_dat,
                                           n = 500, 
                                           sample_size = 100,
                                           x_col = "wshd_area_km2",
                                           y_col = "accm_totco2_o2g_day")

# Saving results

write.csv(constraints_results,paste(results,"guerrero_etal_23_constraint_analysis.csv",sep = '/'),
          row.names = FALSE)

write.csv(results_df,paste(results,"guerrero_etal_23_raw_scaling.csv", sep = '/'),
          row.names = FALSE)
