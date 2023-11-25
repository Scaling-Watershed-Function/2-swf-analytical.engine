################################################################################
# Creating base scaling plots for scaling results including regression coefficient
# grobs
#################################################################################

create_faceted_plots <- function(data, selected_basin) {

    # Filter data for the specified basin
  filtered_data <- data %>% filter(basin == selected_basin)
  
  # Create the plot
  plot <- ggplot(
    filtered_data %>% 
      mutate(
        accm_hzt_cat = factor(
          accm_hzt_cat,
          levels = c("Q10","Q20","Q30","Q40","Q50",
                     "Q60","Q70","Q80","Q90","Q100")
        )
          ),
    aes(
      x = wshd_area_km2, 
      y = accm_totco2_o2g_day, 
      color = accm_hzt_cat
      )
    ) +
    facet_wrap(~basin_cat, ncol = 2) +
    geom_smooth(method = "lm", linewidth = 3.0, fullrange = TRUE) +
    geom_point(alpha = 0.75, size = 3.5) +
    scale_x_log10(
      breaks = breaks, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.001,32000)
      ) +
    scale_y_log10(
      breaks = breaks_c, 
      labels = trans_format("log10", 
                            math_format(10^.x)),
      limits = c(0.0001, 15000000000)
      ) +
    scale_color_viridis_d(name = expression(bold(paste("Cumulative \nhyporheic \nexchange \n(quantiles)")))) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    geom_abline(linewidth = 1.0, linetype = "dashed") +
    theme_httn +
    theme(legend.position = if(selected_basin == "yakima") c(.9,.2) else "none",
          legend.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 32),
          strip.text = element_text(size = 24, face = "bold", hjust = 0),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title.position = "plot") 
  
  # Conditional formatting for yakima basin
  if (selected_basin == "yakima") {
    plot <- plot + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
  } else {
    plot <- plot + ylab(expression(bold(paste("Cumulative Aerobic", " ", respiration[Hyp], "(", gCO[2] * d^-1 * m^-2, ")"))))
  }
  
  # No x-axis title for any plots
  plot <- plot + theme(axis.title.x = element_blank())
  
  return(plot)
}

# Example usage for specific basins
plot_yakima <- create_faceted_plots(scaling_analysis_dat, "yakima")
plot_willamette <- create_faceted_plots(scaling_analysis_dat, "willamette")

# Display plots
plot_yakima
plot_willamette
