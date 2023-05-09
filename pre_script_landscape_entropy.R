################################################################################
# PRE-SCRIPT: LANDSCAPE HETEROGENEITY
################################################################################

#Author: Francisco J. Guerrero

# Input output file paths:

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"

# Importing data

bgc_dat_c6 <- read_csv(paste(processed_data,"230505_bgc_dat_c6_inf.csv",sep = '/'),
                       show_col_types = FALSE)
