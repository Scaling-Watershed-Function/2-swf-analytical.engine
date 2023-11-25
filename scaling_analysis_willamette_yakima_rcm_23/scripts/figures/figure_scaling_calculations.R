################################################################################
# FIGURE: Scaling Calculations from Local Rates to Cumulative Values
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

# Facet plots scaling relationships

metric_labels <- function(metric) {
  return(
    c(
      "totco2_o2g_m2_day" = "Resp. rate (gC/m²/day)",
      "stream_area_m2" = "Stream area (m²)",
      "totco2_o2g_day" = "Respired C (gC/day)",
      "accm_totco2_o2g_day" = "Cumulative resp. C (gC/day)"
    )[metric]
  )
}

scaling_analysis_plot <- scaling_analysis_dat %>% 
  select(
         basin_cat,
         stream_order,
         totco2_o2g_m2_day,
         stream_area_m2,
         totco2_o2g_day,
         accm_totco2_o2g_day
         ) %>% 
  gather(.,
         key = "metric",
         value = "value",
         factor_key = TRUE,
         c(3:6)
         ) %>% 
  ggplot(
         aes(x = as.factor(stream_order),
         y = value,
         color = as.factor(stream_order),
         fill = as.factor(stream_order))
         )+
  geom_half_violin(
                   scale = "width",
                   adjust = 1.5,
                   side = "r",
                   alpha = 0.75,
                   trim = FALSE
                   )+
  geom_boxplot(
               width = .2, 
               alpha = .6, 
               show.legend = FALSE,
               color = "black"
               ) +
  gghalves::geom_half_point(
    side = "l", 
    shape = 95,
    range_scale = 0,
    size = 10, 
    alpha = .2
  )+
  scale_y_log10(
    breaks = breaks_c,
    labels = trans_format(
      "log10", 
      math_format(10^.x))
    ) +
  scale_color_manual(
                    values = my_scolors
    )+
  guides(
    color = "none"
  )+
  scale_fill_manual(
                    values = my_scolors,
                    name = "Stream order \n(Strahler)"
                    )+
  annotation_logticks(
    size = 0.45, 
    sides = "l"
    ) +
  xlab(
    "Stream order (Strahler)"
    )+
  ylab(
    "Value"
  )+
  facet_grid(
    metric ~ basin_cat, 
    scales = "free_y",
    space = "free_y",  # Adjusts the space allocated to each facet based on the data
    labeller = labeller(metric = metric_labels),
  )+
  theme(
    legend.position = c(0.9,0.075),
    legend.title = element_text(size = 36, face = "bold"),
    legend.text = element_text(size = 32),
    axis.text = element_text(size = 32),
    strip.text.x = element_text(size = 36, face = "bold", hjust = 0),
    strip.text.y = element_text(size = 36, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 36, face = "bold")
  )
scaling_analysis_plot

ggsave(file=paste(results_png, paste0("guerrero_etal_23_scaling_calculations.png"),sep = '/'),
       scaling_analysis_plot,
       width = 24,
       height = 36,
       units = "in")


