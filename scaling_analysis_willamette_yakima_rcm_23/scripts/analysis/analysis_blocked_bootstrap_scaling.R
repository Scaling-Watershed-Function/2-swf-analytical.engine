library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(tidyverse)

local_data <- "./data"
results <- "./results"

# Load the data
data <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                 show_col_types = FALSE)

# Define the bootstrap function
bootstrap_regression <- function(data, n_bootstraps, block_size) {
  results <- list()
  
  for (basin in unique(data$basin)) {
    for (hzt_cat in unique(data$accm_hzt_cat)) {
      basin_data <- data %>% filter(basin == !!basin, accm_hzt_cat == !!hzt_cat)
      
      if (nrow(basin_data) < block_size) next
      
      slopes <- vector("numeric", n_bootstraps)
      intercepts <- vector("numeric", n_bootstraps)
      r_squares <- vector("numeric", n_bootstraps)
      
      for (i in 1:n_bootstraps) {
        sample_indices <- sample(seq_len(nrow(basin_data)), size = block_size, replace = TRUE)
        sample_data <- basin_data[sample_indices, ]
        
        model <- lm(log(accm_totco2_o2g_day) ~ log(wshd_area_km2), data = sample_data)
        slopes[i] <- coef(model)["log(wshd_area_km2)"]
        intercepts[i] <- coef(model)["(Intercept)"]
        r_squares[i] <- summary(model)$r.squared
      }
      
      results[[paste(basin, hzt_cat, sep = "_")]] <- list(
        Slope = median(slopes),
        Intercept = median(intercepts),
        SlopeCI = quantile(slopes, probs = c(0.025, 0.975)),
        InterceptCI = quantile(intercepts, probs = c(0.025, 0.975)),
        RSquared = median(r_squares),
        RSquaredCI = quantile(r_squares, probs = c(0.025, 0.975))
      )
    }
  }
  
  return(results)
}

# Run the bootstrap regression
n_bootstraps <- 1000
block_size <- 30
bootstrap_results <- bootstrap_regression(data, n_bootstraps, block_size)

# Convert the results to a data frame
results_df <- map_df(bootstrap_results, ~tibble(.x), .id = "Basin_HztCat")

# Print the results
print(results_df)

# Plotting residuals for 'Q50'
ggplot(data %>% filter(accm_hzt_cat == 'Q50'), aes(x = log(wshd_area_km2), y = log(accm_totco2_o2g_day))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~basin, scales = 'free_x') +
  geom_smooth(method = 'lm', formula = y ~ x, color = 'blue', se = FALSE) +
  labs(title = "Residuals Plot for Q50", x = "Log of Watershed Area (km^2)", y = "Log of Accumulated Total CO2 (O2g/day)") +
  theme_minimal()
