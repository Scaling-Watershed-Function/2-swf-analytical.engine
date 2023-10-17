allometric_analysis <- function(data, x_col, y_col, n) {
  
  estimate_constraint <- function(data, x_col, y_col) {
    # Extract columns
    x <- log(data[[x_col]])  # Take natural logarithm
    y <- data[[y_col]]
    
    # 1. Estimate the kernel density
    bwf <- function(x) {bw.SJ(x)}
    
    kdensity <- density(x, bw = bwf(x))
    
    # 2. Calculate the cumulative distribution function (CDF)
    cdf <- approxfun(kdensity$x, cumsum(kdensity$y) / sum(kdensity$y))
    
    # 3. Extract x values corresponding to the percentiles of interest
    percentiles <- seq(0.05, 0.95, by = 0.05)
    x_values <- sapply(percentiles, function(p) {
      tryCatch({
        uniroot(function(x) cdf(x) - p, range(log(data[[x_col]])))$root
      }, error = function(e) NA)  # Return NA when error occurs
    })
    
    # 4, 5, 6 & 7. Subset the data based on these x values, find max, median, min of y and compute center value of x
    max_values_y <- numeric(length(x_values) - 1)
    median_values_y <- numeric(length(x_values) - 1)
    min_values_y <- numeric(length(x_values) - 1)
    center_values_x <- numeric(length(x_values) - 1)
    
    for (i in 1:(length(x_values) - 1)) {
      subset_data <- subset(data, log(data[[x_col]]) >= x_values[i] & log(data[[x_col]]) <= x_values[i+1])
      
      max_values_y[i] <- max(subset_data[[y_col]])
      median_values_y[i] <- median(subset_data[[y_col]])
      min_values_y[i] <- min(subset_data[[y_col]])
      center_values_x[i] <- exp(mean(log(subset_data[[x_col]])))  # back-transformed mean of log(x)
    }
    
    # Construct result data frame with customized column names
    df <- data.frame(center_values_x, max_values_y, median_values_y, min_values_y)
    names(df) <- c(paste0(x_col, "_avg"), paste0(y_col, "_max"), paste0(y_col, "_med"), paste0(y_col, "_min"))
    
    return(df)
  }
  
  # Create an empty list to store results_df and regression results from each iteration
  results_list <- vector("list", n)
  regression_results <- list()
  
  for (i in 1:n) {
    # Resample data
    resampled_data <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
    
    # Obtain results_df for resampled data
    results_df_resampled <- estimate_constraint(resampled_data, x_col, y_col)
    
    # Store results_df in list
    results_list[[i]] <- results_df_resampled
    
    # Fit log-linear regression for each y column (Max, Med, Min)
    y_cols <- colnames(results_df_resampled)[2:4]
    for (y_col_name in y_cols) {
      response <- as.numeric(results_df_resampled[[y_col_name]]) / results_df_resampled[[paste0(x_col, "_avg")]]
      model <- lm(log(response) ~ log(results_df_resampled[[paste0(x_col, "_avg")]]))
      
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      r_squared <- summary(model)$r.squared
      
      # Compute RMSE
      predictions <- predict(model)
      residuals <- log(response) - predictions
      rmse <- sqrt(mean(residuals^2))
      
      regression_results[[paste("iteration", i, y_col_name)]] <- data.frame(
        Intercept = intercept,
        Slope = slope,
        R_Squared = r_squared,
        RMSE = rmse
      )
    }
  }
  
  # Convert regression results list to a dataframe
  regression_df <- do.call(rbind, regression_results)
  
  # Extract slopes and intercepts for each y column and compute the average and CI
  summary_df <- data.frame(
    Y_Column = c("max", "med", "min"),
    Avg_Slope = c(mean(regression_df$Slope[grep("_max", rownames(regression_df))]),
                  mean(regression_df$Slope[grep("_med", rownames(regression_df))]),
                  mean(regression_df$Slope[grep("_min", rownames(regression_df))])),
    Slope_Lower_CI = c(quantile(regression_df$Slope[grep("_max", rownames(regression_df))], 0.025),
                       quantile(regression_df$Slope[grep("_med", rownames(regression_df))], 0.025),
                       quantile(regression_df$Slope[grep("_min", rownames(regression_df))], 0.025)),
    Slope_Upper_CI = c(quantile(regression_df$Slope[grep("_max", rownames(regression_df))], 0.975),
                       quantile(regression_df$Slope[grep("_med", rownames(regression_df))], 0.975),
                       quantile(regression_df$Slope[grep("_min", rownames(regression_df))], 0.975)),
    Avg_Intercept = c(mean(regression_df$Intercept[grep("_max", rownames(regression_df))]),
                      mean(regression_df$Intercept[grep("_med", rownames(regression_df))]),
                      mean(regression_df$Intercept[grep("_min", rownames(regression_df))])),
    Intercept_Lower_CI = c(quantile(regression_df$Intercept[grep("_max", rownames(regression_df))], 0.025),
                           quantile(regression_df$Intercept[grep("_med", rownames(regression_df))], 0.025),
                           quantile(regression_df$Intercept[grep("_min", rownames(regression_df))], 0.025)),
    Intercept_Upper_CI = c(quantile(regression_df$Intercept[grep("_max", rownames(regression_df))], 0.975),
                           quantile(regression_df$Intercept[grep("_med", rownames(regression_df))], 0.975),
                           quantile(regression_df$Intercept[grep("_min", rownames(regression_df))], 0.975))
  )
  return(summary_df)
}


# test_dat <- scaling_analysis_dat %>% 
#   group_by(basin) %>% 
#   do(allometric_analysis(., x_col = "wshd_area_km2",
#                          y_col = "accm_totco2_o2g_day",
#                          n = 10))






