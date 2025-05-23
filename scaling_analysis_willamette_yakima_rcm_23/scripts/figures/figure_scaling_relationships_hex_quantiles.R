################################################################################
# FIGURE: Scaling relationships per hyporheic exchange quantile
###############################################################################

gc()

rm()

################################################################################
# WATERSHED SCALING PLOTS
################################################################################
# About this plot
cat(readLines("./metadata/code_instructions.Rmd"),sep = '\n')

# Plot settings
source("./source/design_scaling_graphic_prep.R")
source("./source/function_base_scaling_plots.R")
source("./source/function_regression_inset_plots.R")

# Main Plots

#Willamette
plot_willamette <- create_faceted_plots(scaling_analysis_dat, "willamette")
plot_willamette

# Yakima
plot_yakima <- create_faceted_plots(scaling_analysis_dat, "yakima")
plot_yakima

# Inset plots

# Loading regression estimates dataset

regression_estimates <-  read_csv(
  paste(
    results,
    "guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv",
    sep = '/'),
  show_col_types = FALSE
  )

# Preparing dataset

reg_estimates_long <- regression_estimates %>% 
  select(basin,
         quantile,
         Slope,
         RSquared,
         SlopeCI_2.5,
         SlopeCI_97.5,
         RSquaredCI_2.5,
         RSquaredCI_97.5) %>% 
  gather(.,
         key = "metric",
         value = "value",
         factor_key = TRUE,
         c(3:4))

# Willamette
willamette_inset_plot <- reg_inset_plot(
 selected_basin = "willamette"
  )
willamette_inset_plot

w_inset <- ggplotGrob(willamette_inset_plot)

w_scaling_grob <- plot_willamette +
  annotation_custom(
    w_inset,
    ymin = 5.0,
    ymax = 10.35,
    xmin = -3.25,
    xmax = 0.6
  )
w_scaling_grob

# Yakima
yakima_inset_plot <- reg_inset_plot(
  selected_basin = "yakima"
)
yakima_inset_plot

y_inset <- ggplotGrob(yakima_inset_plot)

y_scaling_grob <- plot_yakima +
  annotation_custom(
    y_inset,
    ymin = 5.0,
    ymax = 10.35,
    xmin = -3.25,
    xmax = 0.6
  )
y_scaling_grob

# Align plots using patchwork

# Combine the plots side by side
aligned_plots <- w_scaling_grob + 
  y_scaling_grob +
  plot_layout(ncol = 2)

# Add a common x-axis title using plot_annotation
aligned_plots <- aligned_plots + 
  plot_annotation(
    caption = expression(bold(paste("Watershed area (", km^2, ")"))),
                  theme = theme(
                    plot.caption = element_text(
                      hjust = 0.525, 
                      face = "bold", 
                      size = 32)))

# Display aligned plots
aligned_plots

ggsave(file=paste(
  results_png,
  ("guerrero_etal_23_scaling_results.png"),
  sep = '/'),
       aligned_plots,
       width = 24,
       height = 12,
       units = "in")

