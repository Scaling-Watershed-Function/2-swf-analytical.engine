################################################################################
# Regression Insets Plot Function
################################################################################


reg_inset_plot <- function(selected_basin) {
  plot <- ggplot(
    data = reg_estimates_long %>% 
      filter(basin == selected_basin) %>%
      mutate(
        quantile = factor(quantile,
                          levels = c("Q10", "Q20", "Q30", "Q40", "Q50", 
                                     "Q60", "Q70", "Q80", "Q90", "Q100"))
      ),
    aes(
      x = as.factor(quantile),
      y = value,
      color = metric,
      group = metric
    )
  ) +
    geom_line(linetype = "dashed") +
    geom_errorbar(
      aes(
        ymin = ifelse(metric == "Slope", SlopeCI_2.5, RSquaredCI_2.5),
        ymax = ifelse(metric == "Slope", SlopeCI_97.5, RSquaredCI_97.5)
      ),
      width = 0.25,
      position = position_dodge(width = 0.15)
    ) +
    geom_point(size = 4.5) +
    geom_hline(yintercept = c(1.0, 1.2, 0.8), color = 'black', linewidth = c(1.0, 0.5, 0.5), linetype = c("solid", "dashed", "dashed")) +
    scale_y_continuous(limits = c(0.35, 1.7), breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6)) +
    scale_x_discrete(labels = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "100")) +
    scale_color_manual(name = "Metric", values = c("#3F2145", "#996300")) +
    labs(x = "Cumulative Hyp. Exchange (Quantiles)", y = "Values") +
    ggtitle("Regression slopes and r-squared") +
    theme_httn +
    theme(legend.position = "none",
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 14),
          panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.title = element_text(size = 16, face ="bold"),
          plot.background = element_blank())
  
  return(plot)
}

# Example usage
# willamette_inset_plot <- reg_inset_plot("willamette", legend = TRUE)
# willamette_inset_plot
