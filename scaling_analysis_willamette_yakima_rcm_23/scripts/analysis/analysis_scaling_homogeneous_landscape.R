################################################################################
# Scaling relationships under spatial homogeneity assumptions
################################################################################

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.


librarian::shelf(dplyr,
                 tidyr,
                 purrr,
                 broom,
                 ggplot2,
                 tidyverse,
                 nhdplusTools)

local_data <- "./data"
results <- "./results"
results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"
findings_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/findings"

# Load the data
data <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                 show_col_types = FALSE)

# Identifying relationships to simulate homogeneous water and nutrient inputs

# Since nutrient inputs has minor contribution to spatial variability compared
# to hyporheic exchange, we will assume that respiration values will represent,
# on average, homogeneous nutrient inputs. 

ggplot(data = data,
       aes(x = tot_q_hz_ms,
           y = totco2_o2g_day))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin_cat, ncol = 2)

# Let's create a regression model to estimate the average amount of total
# respiration from total hyporheic exchange

pred_dat <- data %>% 
  select(comid,
         tocomid,
         basin_cat,
         wshd_area_km2,
         tot_q_hz_ms,
         totco2_o2g_day)

mod <- lm(log(totco2_o2g_day) ~ log(tot_q_hz_ms) + basin_cat,
          data = pred_dat)
summary(mod)

# Now let's use this prediction model to estimate the average total aerobic
# respiration assuming a constant hyporheic exchange across the basin. We 
# will vary the hyporheic exchange values according to quantiles 20, 50, and 80,
# and resample the dataset to obtain 25 regression lines for each case. 


# Define the function to perform the required operations
# Define the function to perform the required operations
perform_analysis <- function(data, n_iterations = 25, percentiles = c(20, 50, 80)) {
  results <- list()
  
  for (p in percentiles) {
    for (iteration in 1:n_iterations) {
      # 1. Resample the dataset
      resampled_data <- data[sample(nrow(data), replace = TRUE), ]
      
      # 2. Estimate quantiles per basin type
      quantiles <- resampled_data %>%
        group_by(basin_cat) %>%
        summarise(quantile_value = quantile(tot_q_hz_ms, probs = p/100))
      
      # 3. Predict totco2_o2g_day using the model coefficients
      predictions <- merge(resampled_data, quantiles, by = "basin_cat") %>%
        mutate(predicted_totco2_o2g_day = exp(21.516075 + 1.101146 * log(quantile_value) +
                                                ifelse(basin_cat == "Yakima River (Dry)", -0.956276, 0)))
      
      # Add percentile and iteration info
      predictions$percentile <- p
      predictions$iteration <- iteration
      
      results[[length(results) + 1]] <- predictions
    }
  }
  
  # 5. Combine all results into a new dataset
  final_dataset <- do.call(rbind, results)
  return(final_dataset)
}

# Apply the function to your dataset
final_results <- perform_analysis(pred_dat)


# Calculating cumulative values
final_results <- final_results %>% 
  group_by(basin_cat, percentile, iteration) %>%
  do({
    data_subset <- .
    
    # Ensure data.frame format for calculate_arbolate_sum function
    arbolate_sum_results <- calculate_arbolate_sum(data.frame(
      ID = data_subset$comid,
      toID = data_subset$tocomid,
      length = data_subset$predicted_totco2_o2g_day
    ))
    
    # Merge the arbolate sum results back into the data subset
    data_subset <- merge(data_subset, arbolate_sum_results, by = "comid", all.x = TRUE)
    
    return(data_subset)
  }) %>%
  ungroup()


w1 <- final_results %>% 
  filter(basin_cat=="Willamette River (Wet)" &
         percentile=="20" &
         iteration == 1) %>% 
  mutate(prd_accm_totco2_o2g_day = calculate_arbolate_sum(
    data.frame(
      ID = comid,
      toID = tocomid,
      length = predicted_totco2_o2g_day
    )
  ))

w1 <- final_results %>%
  filter(basin_cat == "Willamette River (Wet)" &
           percentile == "20" &
           iteration == 1) 

# Calculate the arbolate sum separately for each row
arbolate_sum_results <- w1 %>%
  rowwise() %>%
  do({
    data_subset <- .
    arbolate_sum <- calculate_arbolate_sum(data.frame(
      ID = data_subset$comid,
      toID = data_subset$tocomid,
      length = data_subset$predicted_totco2_o2g_day
    ))
    
    # Create a new column for the arbolate sum result
    data.frame(comid = data_subset$comid, prd_accm_totco2_o2g_day = arbolate_sum)
  }) %>%
  ungroup()

# Merge the arbolate sum results back into the original data frame
w1 <- left_join(w1, arbolate_sum_results, by = "comid")



ggplot(data = w1,
       aes(x = wshd_area_km2,
           y = prd_accm_totco2_o2g_day))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()