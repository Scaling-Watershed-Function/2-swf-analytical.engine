###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# MAPPING SCALING BEHAVIOR IN THE WIILLAMETTE AND YAKIMA RIVER BASINS
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

# Scaling analysis results

slopes_ci <- read_csv(
  paste(results,
        "guerrero_etal_23_results_cross_validation_block_bootstrap_scaling.csv",
        sep = '/'),
  show_col_types = FALSE
) %>% 
  select(basin,
         quantile,
         SlopeCI_2.5,
         SlopeCI_97.5,
         RSquared) %>% 
  rename(slope_l_ci = SlopeCI_2.5,
         slope_u_ci = SlopeCI_97.5,
         r_squared = RSquared) %>% 
  mutate(b_q = paste(basin,quantile,sep = "_"))

# Scaling Maps

scaling_map_dat <- scaling_analysis_dat %>% 
 dplyr:: select(basin_cat,
         basin,
         longitude,
         latitude,
         mean_ann_pcpt_mm,
         wshd_avg_elevation_m,
         hrel_3,
         accm_hzt_cat) %>% 
  mutate(b_q = paste(basin,accm_hzt_cat,sep = "_")) %>% 
  merge(.,
        slopes_ci %>% 
          select(slope_l_ci,
                 slope_u_ci,
                 r_squared,
                 b_q),
        by = "b_q",
        all.x = TRUE) %>% 
  select(-b_q) %>% 
  mutate(
    scaling_cat = case_when(slope_l_ci <= 1 & slope_u_ci >= 1 & r_squared > 0.8 ~ "Linear",
                            slope_l_ci < 1 & slope_u_ci < 1 & r_squared > 0.8 ~ "Sublinear",
                            slope_l_ci > 1 & slope_u_ci > 1 & r_squared > 0.8 ~ "Superlinear",
                            r_squared < 0.8 ~ "Uncertain"),
    scaling_cat = factor(scaling_cat, levels = c("Uncertain","Sublinear","Linear","Superlinear")),
    log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm)
  )

###############################################################################

generate_plot <- function(data, aes_color_var, scale_color_func, color_name, title, add_y_axis_label = FALSE, ...) {
  plot <- ggplot(data, aes(x = longitude, y = latitude)) +
    geom_point(aes(color = .data[[aes_color_var]]),
               size = 2.125) +
    scale_color_func(name = color_name, ...) +
    facet_wrap(~basin_cat, ncol = 1, scales = "free") +
    labs(title = title) +
    theme(
      legend.position = c(0.85, 0.405),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      axis.title = element_blank(),
      axis.text = element_text(size = 18),
      plot.title = element_text(size = 24, face = "bold"),
      strip.background = element_blank(),
      strip.text = element_blank()
    )
  
  if (add_y_axis_label) {
    plot <- plot + labs(y = "Latitude") + 
      theme(axis.title.y = element_text(size = 24, face = "bold"))
  }
  
  return(plot)
}

# Generate each plot using the function
scaling_plot <- generate_plot(scaling_map_dat, "scaling_cat", scale_color_viridis_d, "Type", "Scaling", TRUE)
precipt_plot <- generate_plot(scaling_map_dat, "log_mean_ann_pcpt_mm", scale_color_viridis_c, "Log[mm]", "Precipitation")
elevat_plot <- generate_plot(scaling_map_dat, "wshd_avg_elevation_m", scale_color_gradientn, "Meters", "Elevation", colours = terrain.colors(10))
heterog_plot <- generate_plot(scaling_map_dat, "hrel_3", scale_color_viridis_c, "Shannon's h", "Landscape entropy", option = "mako")


# Combine the plots side by side
combined_plots <- scaling_plot + precipt_plot + elevat_plot + heterog_plot + plot_layout(ncol = 4)

# Use wrap_elements and labs(tag = ...) for a common x-axis label
combined_plots_with_x_label <- wrap_elements(panel = combined_plots) +
  labs(tag = "Longitude") +
  theme(
    plot.tag = element_text(size = rel(2.25), face = "bold"), # Customize the tag appearance
    plot.tag.position = "bottom" # Position the tag at the bottom (common x-axis label)
  )

ggsave(
  paste(
    results_png,
    "guerrero_etal_23_scaling_maps.png",
    sep = '/'
             ),
  combined_plots_with_x_label,
  height = 12,
  width = 22,
  units = "in",
  dpi = 300
  )



