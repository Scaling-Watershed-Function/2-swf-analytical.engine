################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data: Sensitivity Analysis Block Size
################################################################################

# Algorithm design and code evaluation: Francisco J. Guerrero 
# Code writing: AI-driven coding and data analysis tool developed by OpenAI.


librarian::shelf(dplyr,
                 tidyr,
                 purrr,
                 broom,
                 ggplot2,
                 tidyverse)

local_data <- "./data"
results <- "./results"
results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"
findings_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/findings"

source("./source/function_blocked_bootstrap.R")

# Load the data
data <- read_csv(paste(local_data,"231008_scaling_analysis_dat.csv", sep = "/"),
                 show_col_types = FALSE)

# Identifying the effect of block size on regression results:

# Define a range of block sizes for the sensitivity analysis
block_sizes <- seq(10,100,10)

# Run the bootstrap regression for each block size and collect results
sensitivity_results <- lapply(block_sizes, function(size) {
  cat("Running bootstrap with block size:", size, "\n")
  res <- bootstrap_regression(data, n_bootstraps = 100, block_size = size)
  
  # Adding block size information to the results
  res <- lapply(res, function(x) {
    x$block_size <- size
    x
  })
  
  return(res)
})

# Initialize an empty data frame to store the combined results
combined_results <- data.frame()

# Iterate through each block size result in sensitivity_results
for (i in seq_along(sensitivity_results)) {
  block_size_results <- sensitivity_results[[i]]
  
  # Iterate through each basin_hzt combination within this block size
  for (basin_hzt in names(block_size_results)) {
    # Extract the results for this specific basin_hzt combination
    results <- block_size_results[[basin_hzt]]
    
    # Create a data frame for the current results
    current_df <- data.frame(
      block_size = results$block_size,
      Basin_HztCat = basin_hzt,
      Slope = results$Slope,
      Intercept = results$Intercept,
      SlopeCI_2.5 = results$SlopeCI["2.5%"],
      SlopeCI_97.5 = results$SlopeCI["97.5%"],
      InterceptCI_2.5 = results$InterceptCI["2.5%"],
      InterceptCI_97.5 = results$InterceptCI["97.5%"],
      RSquared = results$RSquared,
      RSquaredCI_2.5 = results$RSquaredCI["2.5%"],
      RSquaredCI_97.5 = results$RSquaredCI["97.5%"]
    )
    
    # Combine with the main data frame
    combined_results <- rbind(combined_results, current_df)
  }
}

# Reshape the combined results for plotting
results_reshaped <- combined_results %>%
  tidyr::pivot_longer(cols = c("Slope", "Intercept", "RSquared"),
                      names_to = "Measure", values_to = "Value")%>%
  mutate(basin = str_extract(Basin_HztCat, "^[^_]+",),
         quantile = str_extract(Basin_HztCat,"(?<=_).*$")) %>% 
  mutate(quantile_cat = factor(quantile, levels = paste0("Q", rev(seq(10, 100, by = 10))))) %>% 
  arrange(basin, desc(quantile_cat))

# View the updated dataframe
head(results_reshaped)

# Plotting
sensitivity_plot <- ggplot(results_reshaped, 
                           aes(x = block_size, 
                               y = Value, 
                               color = quantile_cat)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 50,
             linetype = "dashed",
             linewidth = 1.0)+
  scale_color_viridis_d(name = "Hyporheic \nexchange \nquantiles")+
  facet_wrap(basin ~ Measure, 
             scales = "free_y",
             ncol = 3) +
  labs(title = "Effect of Block Size on Regression Outcomes",
       x = "Block Size",
       y = "Value") +
  theme_minimal()+
  theme(axis.title = element_text(size = 22, 
                                  face = "bold"),
        axis.text = element_text(size = 18),
        strip.text = element_text(size = 20,
                                  face = "bold"))
sensitivity_plot

ggsave(file=paste(findings_png, paste0("guerrero_etal_23_block_bootstrap_sensitivity_analysis.png"),sep = '/'),
       sensitivity_plot,
       width = 24,
       height = 16,
       units = "in")
