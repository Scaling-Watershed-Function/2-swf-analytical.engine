allometric_constraint_huc_12 <- function(df, n, sample_size, x_col, y_col) {
  
  # Function to run the regression and return the results
  run_regression <- function(sample_df) {
    formula_string <- paste("log(", y_col, "/", x_col, ") ~", x_col)
    model <- lm(as.formula(formula_string), data = sample_df)
    results <- list(
      slope = coef(model)[2],
      intercept = coef(model)[1],
      r_squared = summary(model)$r.squared
    )
    return(results)
  }
  
  results_data <- data.frame(
    basin = character(),
    avg_slope = numeric(),
    avg_intercept = numeric(),
    ci_slope_low = numeric(),
    ci_slope_high = numeric(),
    ci_intercept_low = numeric(),
    ci_intercept_high = numeric(),
    r2_min = numeric(),
    r2_max = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(basin_level in unique(df$basin)) {
    basin_data <- df[df$basin == basin_level, ]
    regressions <- list()
    
    for(i in 1:n) {
      unique_huc_12_samples <- sample(unique(basin_data$huc_12), sample_size)
      sample_df <- basin_data[basin_data$huc_12 %in% unique_huc_12_samples, ]
      regressions[[i]] <- run_regression(sample_df)
    }
    
    # Sort by r-squared and take top 100 or all if fewer than 100
    sorted_regressions <- regressions[order(sapply(regressions, function(x) x$r_squared), decreasing = TRUE)]
    top_regressions <- sorted_regressions[1:min(100, length(sorted_regressions))]
    
    # Calculate average slope and intercept
    avg_slope <- mean(sapply(top_regressions, function(x) x$slope))
    avg_intercept <- mean(sapply(top_regressions, function(x) x$intercept))
    
    # Bootstrapped confidence intervals
    slopes <- sapply(top_regressions, function(x) x$slope)
    intercepts <- sapply(top_regressions, function(x) x$intercept)
    ci_slope <- quantile(slopes, c(0.025, 0.975))
    ci_intercept <- quantile(intercepts, c(0.025, 0.975))
    
    # R-squared range
    r2_range <- c(min = min(sapply(top_regressions, function(x) x$r_squared)), 
                  max = max(sapply(top_regressions, function(x) x$r_squared)))
    
    tmp_data <- data.frame(
      basin = basin_level,
      avg_slope = avg_slope,
      avg_intercept = avg_intercept,
      ci_slope_low = ci_slope[1],
      ci_slope_high = ci_slope[2],
      ci_intercept_low = ci_intercept[1],
      ci_intercept_high = ci_intercept[2],
      r2_min = r2_range['min'],
      r2_max = r2_range['max'],
      stringsAsFactors = FALSE
    )
    
    results_data <- rbind(results_data, tmp_data)
  }
  
  return(results_data)
}