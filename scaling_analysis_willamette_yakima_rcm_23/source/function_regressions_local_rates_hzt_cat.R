# Load necessary libraries
librarian::shelf(dplyr, tidyr, purrr, broom)

# Define the function
local_rates_regressions <- function(data) {
  
  # Split data by basin and hzt_cat
  split_data <- split(data, list(data$basin, data$hzt_cat))
  
  # Run linear regression for each subset and gather results
  results <- map_dfr(split_data, ~{
    
    # Run the regression model
    model <- lm(log(totco2_o2g_m2_day) ~ log(wshd_area_km2), data = .x)
    tidy_result <- broom::tidy(model)
    glance_result <- broom::glance(model)
    
    # Extract coefficients and stats using dplyr::filter
    slope_data <- dplyr::filter(tidy_result, term == "log(wshd_area_km2)")
    intercept_data <- dplyr::filter(tidy_result, term == "(Intercept)")
    
    slope <- ifelse(nrow(slope_data) > 0, slope_data$estimate, NA)
    p_value <- ifelse(nrow(slope_data) > 0, slope_data$p.value, NA)
    intercept <- ifelse(nrow(intercept_data) > 0, intercept_data$estimate, NA)
    
    tibble(
      basin = unique(.x$basin),
      hzt_cat = unique(.x$hzt_cat),
      slope = slope,
      intercept = intercept,
      r_squared = glance_result$r.squared,
      p_value = p_value
    )
  })
  
  return(results)
}

# You can now call the function on your data
# results_df <- local_rates_regressions(scaling_analysis_dat)


# Execute the function
#results_df <- run_regressions(scaling_analysis_dat)

# Display the results
#print(results_df)
