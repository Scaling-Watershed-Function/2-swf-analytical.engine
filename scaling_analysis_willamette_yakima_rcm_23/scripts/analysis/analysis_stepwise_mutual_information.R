###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# MUTUAL INFORMATION MATRICES AMONG HIGHER SCALE VARIABLES ON HYPORHEIC RESP. 
###############################################################################

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

# Loading dataset

correlation_data <- read_csv(
  paste(
    local_data,
    "guerrero_etal_23_correlation_analysis_dat.csv",
    sep = '/'
  ),
  show_col_types = FALSE
)

# Number of bins
num_bins <- 10

for (basin in basins) {
  basin_data <- subset(correlation_data, basin == basin)
  
  # Discretizing each variable
  for (i in 2:ncol(basin_data)) {
    basin_data[[i]] <- cut(basin_data[[i]], breaks = num_bins, labels = FALSE, include.lowest = TRUE)
  }
  
  # List to store mutual information values for this basin
  mi_values <- list()
  
  for (i in 2:(ncol(basin_data)-1)) {
    var1 <- basin_data[[i]]
    var2 <- basin_data[[i + 1]]
    
    # Entropy of var1 and var2
    h_var1 <- entropy(var1, method = "ML")
    h_var2 <- entropy(var2, method = "ML")
    
    # Joint entropy of var1 and var2
    joint_entropy <- entropy(cbind(var1, var2), method = "ML")
    
    # Mutual information between var1 and var2
    mi_var1_var2 <- h_var1 + h_var2 - joint_entropy
    
    # Conditional entropy of var2 given var1
    ce_var2_given_var1 <- joint_entropy - h_var1
    
    # If not the first iteration, calculate mutual information between var2 and conditional entropy of previous variables
    if (i > 2) {
      prev_var <- basin_data[[i - 1]]
      joint_entropy_prev <- entropy(cbind(prev_var, var2), method = "ML")
      ce_prevVar_given_var2 <- joint_entropy_prev - entropy(prev_var, method = "ML")
      mi_var2_ce_prevVar <- h_var2 - ce_prevVar_given_var2
      mi_values[[names(basin_data)[i + 1]]] <- c(mi_var1_var2, ce_var2_given_var1, mi_var2_ce_prevVar)
    } else {
      mi_values[[names(basin_data)[i + 1]]] <- c(mi_var1_var2, ce_var2_given_var1)
    }
  }
  
  results[[basin]] <- mi_values
}