################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data
################################################################################

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.

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