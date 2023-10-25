###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LOG-LOG REGRESSIONS LOCAL RESPIRATION RATES ~ WATERSHED AREAS WITHIN HYP. 
# EXCHANGE QUANTILES. 
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis, GGally, nhdplusTools, gridExtra, ggExtra, data.table, viridis)

set.seed(2703)

results <- "./results"

source("./source/design_scaling_graphic_prep.R")
source("./source/function_regressions_local_rates_hzt_cat.R")

local_scaling_dat <- scaling_analysis_dat %>% 
  filter(stream_order<6) %>% 
  select(basin,
         wshd_area_km2,
         totco2_o2g_m2_day,
         hzt_cat)

local_rates_results <- local_rates_regressions(local_scaling_dat)

local_rates_results 
       
# Sort the dataframe
sorted_results <- local_rates_results  %>%
  arrange(basin, desc(factor(hzt_cat, levels = c("Q100", "Q90", "Q80", "Q70", "Q60", "Q50", "Q40", "Q30", "Q20", "Q10"))))
sorted_results

write.csv(sorted_results,
          paste(results,"guerrero_etal_23_local_scaling_regressions.csv", sep = '/'),
          row.names = FALSE)

