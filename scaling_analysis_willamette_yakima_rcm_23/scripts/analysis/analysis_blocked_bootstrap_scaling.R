################################################################################
# Blocked Bootstrap Regression for Scaling Exponent estimation in longitudinally
# dependent cumulative data
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

# We selected a block size of 50

# Run the bootstrap regression
n_bootstraps <- 1000
block_size <- 50
bootstrap_results <- bootstrap_regression(data, n_bootstraps, block_size)

# Convert the results to a data frame
results_df <- map_df(bootstrap_results, ~tibble(.x), .id = "Basin_HztCat")

# Print the results
print(results_df)

# Unlist the nested list structure and create a vector
unlisted_results <- unlist(results_df$.x)

# Determine the number of rows in the final dataframe
num_rows <- length(unlisted_results) / 9

# Create a new dataframe with each row containing a set of 8 values
reshaped_results <- data.frame(matrix(unlisted_results, nrow = num_rows, byrow = TRUE))

# Assign column names to the reshaped dataframe
colnames(reshaped_results) <- c("Slope", "Intercept", "SlopeCI_2.5", "SlopeCI_97.5", 
                                "InterceptCI_2.5", "InterceptCI_97.5", "RSquared", 
                                "RSquaredCI_2.5","RSquaredCI_97.5")

# Add the Basin_HztCat column
reshaped_results$Basin_HztCat <- unique(results_df$Basin_HztCat)

# View the reshaped dataframe
head(reshaped_results)

# Reordering data frame
reordered_results <- reshaped_results %>% 
mutate(basin = str_extract(Basin_HztCat, "^[^_]+",),
       quantile = str_extract(Basin_HztCat,"(?<=_).*$")) %>% 
  mutate(quantile_cat = factor(quantile, levels = paste0("Q", rev(seq(10, 100, by = 10))))) %>% 
  arrange(basin, desc(quantile_cat)) %>% 
  select(basin,
         quantile,
         Slope,
         Intercept,
         RSquared,
         SlopeCI_2.5,
         SlopeCI_97.5,
         InterceptCI_2.5,
         InterceptCI_97.5,
         RSquaredCI_2.5,
         RSquaredCI_97.5)

# Plotting results:
# Convert 'quantile' to a factor with levels ordered as desired
reordered_results$quantile <- factor(reordered_results$quantile, 
                                     levels = c("Q10", "Q20", "Q30", "Q40", "Q50", "Q60", "Q70", "Q80", "Q90", "Q100"))

# Reshape the data for plotting
long_results <- tidyr::pivot_longer(reordered_results, 
                                    cols = c("Slope", "Intercept", "RSquared"), 
                                    names_to = "Measure", 
                                    values_to = "Value")

# Add CI values to the long format data
long_results$CI_lower <- NA
long_results$CI_upper <- NA

long_results$CI_lower[long_results$Measure == "Slope"] <- reordered_results$SlopeCI_2.5
long_results$CI_upper[long_results$Measure == "Slope"] <- reordered_results$SlopeCI_97.5
long_results$CI_lower[long_results$Measure == "Intercept"] <- reordered_results$InterceptCI_2.5
long_results$CI_upper[long_results$Measure == "Intercept"] <- reordered_results$InterceptCI_97.5
long_results$CI_lower[long_results$Measure == "RSquared"] <- reordered_results$RSquaredCI_2.5
long_results$CI_upper[long_results$Measure == "RSquared"] <- reordered_results$RSquaredCI_97.5


# Plot with jitter
# Add jittered positions as new columns in the dataframe
set.seed(123)  # for reproducibility of jitter
jitter_width <- 0.35
long_results <- long_results %>%
  group_by(Measure) %>%
  mutate(
    jittered_x = as.numeric(quantile) + runif(n(), -jitter_width / 2, jitter_width / 2)
  ) %>%
  ungroup()

# Plot with jittered points and error bars using the new positions
bootstrap_results_plot <- ggplot(data = long_results%>%
                                   mutate(Measure = factor(Measure, 
                                                           levels = c("Slope", 
                                                                      "Intercept", 
                                                                      "RSquared"))), 
                                 aes(x = jittered_x, 
                                     y = Value, color = basin)) +
  geom_point(size = 3.0) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.05) +
  scale_x_continuous(name = "Hyporheic Exchange Quantiles", 
                     breaks = 1:length(unique(long_results$quantile)), 
                     labels = seq(10,100,10))+
  scale_color_manual(values = c("darkorchid","darkorange"),
                    name = "Basin")+
  facet_wrap(~ Measure, scales = "free_y") +
  labs(title = "Regression Results (Per Quantile)", y = "Value") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22, 
                                  face = "bold"),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 20,
                                  face = "bold"),
        plot.background = element_rect(color = "white"),
        plot.title = element_text(size = 22,
                                  face = "bold"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16,
                                    face = "bold"),
        legend.position = c(0.9,0.2))
bootstrap_results_plot
ggsave(file=paste(findings_png, paste0("guerrero_etal_23_block_bootstrap_results_quantiles.png"),sep = '/'),
       width = 24,
       height = 8,
       units = "in") 



