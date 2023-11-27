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
  mutate(
    quantile_num = as.numeric(sub("[^0-9]+", "", accm_hzt_cat)),
    scaling_cat = 
           case_when(basin == "willamette" & quantile_num >= 60 ~ "Superlinear",
                     basin == "willamette" & quantile_num >= 40 & quantile_num < 60 ~ "Linear",
                     basin == "willamette" & quantile_num < 40 ~ "Uncertain",
                     basin == "yakima" & quantile_num >= 80 ~ "Superlinear",
                     basin == "yakima" & quantile_num == 70 ~ "Linear",
                     basin == "yakima" & quantile_num >= 40 & quantile_num < 70 ~ "Sub-linear",
                     basin == "yakima" & quantile_num < 40 ~"Uncertain"),
    scaling_cat = factor(scaling_cat, levels = c("Uncertain",
                                                 "Sub-linear",
                                                 "Linear",
                                                 "Superlinear")),
    log_mean_ann_pcpt_mm = log(mean_ann_pcpt_mm),
    scaling_cat_num = 
      case_when(scaling_cat == "Uncertain"~1,
                scaling_cat == "Sub-linear"~2,
                scaling_cat == "Linear"~3,
                scaling_cat == "Superlinear"~4)
  ) %>% 
  dplyr::select(basin,
                basin_cat,
                longitude,
                latitude,
                scaling_cat,
                scaling_cat_num,
                log_mean_ann_pcpt_mm,
                wshd_avg_elevation_m,
                hrel_3) %>% 
  gather(key = "characteristic",
         value = "values",
         factor_key = TRUE,
         c(6:9))

###############################################################################

# Plot template

ggplot(
  data = filter(
    scaling_map_dat,#argument 1 (data)
    basin == "willamette" & #argument 2 (basin)
      characteristic == "scaling_cat_num"),# 3(characteristic)
  aes(
    x = longitude,
    y = latitude,
    color = values
  )
)+
  geom_point(size = 2.5)+
  scale_color_viridis_c(option = "cividis",
                        direction = -1)+#argument 4 (color_option)
  theme(legend.position = "none",
        axis.text = element_text(size = 24),
        axis.title = element_blank(),
        axis.text.y = element_blank())
  

map_plot_function <- function(data, selected_basin, attribute, color_option, grad, axis_y) {
  plot <- ggplot(
    data = filter(
      data,
      basin == selected_basin & 
        characteristic == attribute
    ),
    aes(
      x = longitude,
      y = latitude,
      color = values
    )
  ) +
    geom_point(size = 2.5) +
    scale_color_viridis_c(option = color_option,
                          direction = grad) +
    theme(legend.position = "none",
          axis.text = element_text(size = 24),
          axis.title = element_blank())
  
  if (axis_y == TRUE) {
    plot <- plot + theme(axis.text.y = element_text(size = 24))
  } else {
    plot <- plot + theme(axis.text.y = element_blank())
  }
  
  return(plot)
}
  
# Willamette Plots
# Scaling plot
w_scaling_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "scaling_cat_num",
  "cividis",
  1,
  axis_y = TRUE
)

# Precipitation plot

w_precipt_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "log_mean_ann_pcpt_mm",
  "turbo",
  1,
  axis_y = FALSE
)
w_precipt_plot

# Elevation plot
w_elev_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "wshd_avg_elevation_m",
  "viridis",
  1,
  axis_y = FALSE
)
w_elev_plot


# Entropy plot
w_ent_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "hrel_3",
  "mako",
  1,
  axis_y = FALSE
)
w_ent_plot


# Yakima Plots

# Scaling plot
y_scaling_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "scaling_cat_num",
  "cividis",
  1,
  axis_y = TRUE
)
y_scaling_plot

# Precipitation plot

y_precipt_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "log_mean_ann_pcpt_mm",
  "turbo",
  1,
  axis_y = FALSE
)
y_precipt_plot

# Elevation plot
y_elev_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "wshd_avg_elevation_m",
  "viridis",
  1,
  axis_y = FALSE
)
y_elev_plot


# Entropy plot
y_ent_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "hrel_3",
  "mako",
  1,
  axis_y = FALSE
)
y_ent_plot





# Step 1: Determine Global Min and Max Values for Each Variable
global_min_max <- scaling_map_dat %>%
  dplyr::filter(characteristic %in% c("log_mean_ann_pcpt_mm", "wshd_avg_elevation_m")) %>%
  dplyr::group_by(characteristic) %>%
  dplyr::summarize(
    global_min = min(values, na.rm = TRUE),
    global_max = max(values, na.rm = TRUE)
  )

# Step 2: Adjusting the map_plot_function
map_plot_function <- function(data, selected_basin, attribute, color_option, grad, axis_y, scale_min = NA, scale_max = NA) {
  plot_data <- filter(
    data,
    basin == selected_basin & 
      characteristic == attribute
  )
  
  plot <- ggplot(
    data = plot_data,
    aes(
      x = longitude,
      y = latitude,
      color = values
    )
  ) + geom_point(size = 2.5)
  
  # Apply scale limits if they are provided and relevant
  if (!is.na(scale_min) && !is.na(scale_max) && attribute %in% c("log_mean_ann_pcpt_mm", "wshd_avg_elevation_m")) {
    plot <- plot + scale_color_viridis_c(option = color_option, direction = grad, limits = c(scale_min, scale_max))
  } else {
    plot <- plot + scale_color_viridis_c(option = color_option, direction = grad)
  }
  
  # Apply themes
  plot <- plot + theme(legend.position = "none", axis.text = element_text(size = 24), axis.title = element_blank())
  if (axis_y) {
    plot <- plot + theme(axis.text.y = element_text(size = 24))
  } else {
    plot <- plot + theme(axis.text.y = element_blank())
  }
  
  return(plot)
}


# Example of using the modified function
# Extract scale limits for "log_mean_ann_pcpt_mm"
scale_limits_pcpt <- global_min_max %>%
  filter(characteristic == "log_mean_ann_pcpt_mm") %>%
  select(global_min, global_max)

# Use the function for "log_mean_ann_pcpt_mm"
w_precipt_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "log_mean_ann_pcpt_mm",
  "viridis",
  1,
  axis_y = FALSE,
  scale_min = scale_limits_pcpt$global_min,
  scale_max = scale_limits_pcpt$global_max
)
w_precipt_plot

y_precipt_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "log_mean_ann_pcpt_mm",
  "viridis",
  1,
  axis_y = FALSE,
  scale_min = scale_limits_pcpt$global_min,
  scale_max = scale_limits_pcpt$global_max
)
y_precipt_plot






# Use the function for "wshd_avg_elevation_m"

# Extract scale limits for "log_mean_ann_pcpt_mm"
scale_limits_elev <- global_min_max %>%
  filter(characteristic == "wshd_avg_elevation_m") %>%
  select(global_min, global_max)



w_elevat_plot <- map_plot_function(
  scaling_map_dat,
  "willamette",
  "wshd_avg_elevation_m",
  "turbo",
  1,
  axis_y = FALSE,
  scale_min = scale_limits_elev$global_min,
  scale_max = scale_limits_elev$global_max
)
w_elevat_plot



y_elevat_plot <- map_plot_function(
  scaling_map_dat,
  "yakima",
  "wshd_avg_elevation_m",
  "turbo",
  1,
  axis_y = FALSE,
  scale_min = scale_limits_elev$global_min,
  scale_max = scale_limits_elev$global_max
)
y_elevat_plot

################################################################################

# Step 1: Determine Global Min and Max Values for Each Variable
global_min_max <- scaling_map_dat %>%
  filter(characteristic %in% c("log_mean_ann_pcpt_mm", "wshd_avg_elevation_m")) %>%
  group_by(characteristic) %>%
  dplyr::summarize(
    global_min = min(values, na.rm = TRUE),
    global_max = max(values, na.rm = TRUE)
  )

# Modified map_plot_function
map_plot_function <- function(data, selected_basin, attribute, color_option, grad, axis_y, scale_min = NA, scale_max = NA) {
  plot_data <- filter(
    data,
    basin == selected_basin & 
      characteristic == attribute
  )
  
  plot <- ggplot(
    data = plot_data,
    aes(
      x = longitude,
      y = latitude,
      color = values
    )
  ) + geom_point(size = 2.5)
  
  # Apply scale limits if they are provided and relevant
  if (!is.na(scale_min) && !is.na(scale_max) && attribute %in% c("log_mean_ann_pcpt_mm", "wshd_avg_elevation_m")) {
    plot <- plot + scale_color_viridis_c(option = color_option, direction = grad, limits = c(scale_min, scale_max))
  } else {
    plot <- plot + scale_color_viridis_c(option = color_option, direction = grad)
  }
  
  # Apply themes
  plot <- plot + theme(legend.position = "none", axis.text = element_text(size = 24), axis.title = element_blank())
  if (axis_y) {
    plot <- plot + theme(axis.text.y = element_text(size = 24))
  } else {
    plot <- plot + theme(axis.text.y = element_blank())
  }
  
  return(plot)
}

# Define the basins, characteristics, and other settings
basins <- c("willamette", "yakima")
characteristics <- c("scaling_cat_num", "log_mean_ann_pcpt_mm", "wshd_avg_elevation_m", "hrel_3")
color_options <- c("cividis", "turbo", "viridis", "mako")
grads <- rep(1, length(characteristics))
axis_ys <- c(TRUE, FALSE, FALSE, FALSE)

# Loop and generate plots
plots <- list()
for (basin in basins) {
  for (i in seq_along(characteristics)) {
    attribute <- characteristics[i]
    
    # Determine if scale limits are needed
    if (attribute %in% c("log_mean_ann_pcpt_mm", "wshd_avg_elevation_m")) {
      scale_limits <- global_min_max %>%
        filter(characteristic == attribute) %>%
        select(global_min, global_max)
      
      scale_min <- scale_limits$global_min
      scale_max <- scale_limits$global_max
    } else {
      scale_min <- NA
      scale_max <- NA
    }
    
    # Create plot
    plot_name <- paste(basin, attribute, sep = "_")
    plots[[plot_name]] <- map_plot_function(
      scaling_map_dat,
      basin,
      attribute,
      color_options[i],
      grads[i],
      axis_y = axis_ys[i],
      scale_min = scale_min,
      scale_max = scale_max
    )
  }
}


# Combine the plots into two rows, with four columns each
willamette_row <- plots[["willamette_scaling_cat_num"]] + 
  plots[["willamette_log_mean_ann_pcpt_mm"]] + 
  plots[["willamette_wshd_avg_elevation_m"]] + 
  plots[["willamette_hrel_3"]]

yakima_row <- plots[["yakima_scaling_cat_num"]] + 
  plots[["yakima_log_mean_ann_pcpt_mm"]] + 
  plots[["yakima_wshd_avg_elevation_m"]] + 
  plots[["yakima_hrel_3"]]

# Ensure that each row is treated as a single entity
willamette_row <- willamette_row + plot_layout(ncol = 4, nrow = 1)
yakima_row <- yakima_row + plot_layout(ncol = 4, nrow = 1)

# Stack the two rows on top of each other using the '/' operator
combined_plot <- willamette_row / yakima_row + 
  plot_layout(guides = 'collect')

# Add common axis labels (if needed)
combined_plot <- combined_plot + 
  theme(
    axis.title.x = element_text("Longitude"),
    axis.title.y = element_text("Latitude")
  )

# Collect the legends into a single column on the right
combined_plot <- combined_plot & 
  theme(legend.position = 'right')

# Print the combined plot
print(combined_plot)


# First, ensure that the color scales are consistent across plots
# Then, specify the legend titles for each plot using `labs()`

plots[["willamette_scaling_cat_num"]] <- plots[["willamette_scaling_cat_num"]] + labs(color = "Scaling type")
plots[["willamette_log_mean_ann_pcpt_mm"]] <- plots[["willamette_log_mean_ann_pcpt_mm"]] + labs(color = "Precipitation log [mm]")
plots[["willamette_wshd_avg_elevation_m"]] <- plots[["willamette_wshd_avg_elevation_m"]] + labs(color = "Elevation (m)")
plots[["willamette_hrel_3"]] <- plots[["willamette_hrel_3"]] + labs(color = "Landscape entropy")

plots[["yakima_scaling_cat_num"]] <- plots[["yakima_scaling_cat_num"]] + labs(color = "Scaling type")
plots[["yakima_log_mean_ann_pcpt_mm"]] <- plots[["yakima_log_mean_ann_pcpt_mm"]] + labs(color = "Precipitation log [mm]")
plots[["yakima_wshd_avg_elevation_m"]] <- plots[["yakima_wshd_avg_elevation_m"]] + labs(color = "Elevation (m)")
plots[["yakima_hrel_3"]] <- plots[["yakima_hrel_3"]] + labs(color = "Landscape entropy")

# Combine the plots into two rows, with four columns each
willamette_row <- plots[["willamette_scaling_cat_num"]] + 
  plots[["willamette_log_mean_ann_pcpt_mm"]] + 
  plots[["willamette_wshd_avg_elevation_m"]] + 
  plots[["willamette_hrel_3"]]

yakima_row <- plots[["yakima_scaling_cat_num"]] + 
  plots[["yakima_log_mean_ann_pcpt_mm"]] + 
  plots[["yakima_wshd_avg_elevation_m"]] + 
  plots[["yakima_hrel_3"]]

# Ensure that each row is treated as a single entity
willamette_row <- willamette_row + plot_layout(ncol = 4, nrow = 1)
yakima_row <- yakima_row + plot_layout(ncol = 4, nrow = 1)


# Stack the two rows on top of each other using the '/' operator
combined_plot <- willamette_row / yakima_row 

# Add common axis labels to the first plot of each row, which will apply to all plots
combined_plot <- combined_plot & 
  theme(
    axis.title.x = element_text("Longitude"),
    axis.title.y = element_text("Latitude")
  ) &
  plot_layout(guides = 'collect') & 
  guide_area()

# Print the combined plot
print(combined_plot)



#####################################################################################

# First, update each plot with its respective legend title and ensure axis titles are set
for (basin in c("willamette", "yakima")) {
  plots[[paste(basin, "scaling_cat_num", sep = "_")]] <- 
    plots[[paste(basin, "scaling_cat_num", sep = "_")]] + 
    labs(color = "Scaling type") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  plots[[paste(basin, "log_mean_ann_pcpt_mm", sep = "_")]] <- 
    plots[[paste(basin, "log_mean_ann_pcpt_mm", sep = "_")]] + 
    labs(color = "Precipitation log [mm]") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  plots[[paste(basin, "wshd_avg_elevation_m", sep = "_")]] <- 
    plots[[paste(basin, "wshd_avg_elevation_m", sep = "_")]] + 
    labs(color = "Elevation (m)") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
  plots[[paste(basin, "hrel_3", sep = "_")]] <- 
    plots[[paste(basin, "hrel_3", sep = "_")]] + 
    labs(color = "Landscape entropy") + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}

# Combine the plots into two rows, with four columns each
willamette_row <- plots[["willamette_scaling_cat_num"]] + 
  plots[["willamette_log_mean_ann_pcpt_mm"]] + 
  plots[["willamette_wshd_avg_elevation_m"]] + 
  plots[["willamette_hrel_3"]]

yakima_row <- plots[["yakima_scaling_cat_num"]] + 
  plots[["yakima_log_mean_ann_pcpt_mm"]] + 
  plots[["yakima_wshd_avg_elevation_m"]] + 
  plots[["yakima_hrel_3"]]

# Ensure that each row is treated as a single entity
willamette_row <- willamette_row + plot_layout(ncol = 4, nrow = 1)
yakima_row <- yakima_row + plot_layout(ncol = 4, nrow = 1)

# Stack the two rows on top of each other using the '/' operator
combined_plot <- willamette_row / yakima_row 

# Collect the guides
combined_plot <- combined_plot & plot_layout(guides = 'collect')

# Add the guide area for the legends
combined_plot <- combined_plot & guide_area()

# Set common axis titles for the combined plot
combined_plot <- combined_plot & 
  theme(
    axis.title.x = element_text("Longitude"),
    axis.title.y = element_text("Latitude"),
    legend.position = 'right'
  )

# Print the combined plot
print(combined_plot)







librarian::shelf(cowplot)

# Turn off the legends for all individual plots
for (name in names(plots)) {
  plots[[name]] <- plots[[name]] + theme(legend.position = "none")
}

# Combine the plots into two rows, with four columns each
willamette_row <- plot_grid(plotlist = lapply(names(plots)[grepl("willamette", names(plots))], function(x) plots[[x]]), ncol = 4)
yakima_row <- plot_grid(plotlist = lapply(names(plots)[grepl("yakima", names(plots))], function(x) plots[[x]]), ncol = 4)

# Extract the legend from a representative plot (make sure this plot has a legend)
legend_plot <- plots[[names(plots)[1]]] + theme(legend.position = "bottom")
legend <- get_legend(legend_plot)

# Combine the plot grid with the extracted legend
final_plot <- plot_grid(
  willamette_row, yakima_row, 
  legend, 
  ncol = 1, 
  rel_heights = c(1, 1, 0.1) # Adjust the relative heights of the plot grid and the legend
)



# Combine the plots into a grid
combined_plot <- plot_grid(
  plots[["willamette_scaling_cat_num"]], plots[["willamette_log_mean_ann_pcpt_mm"]],
  plots[["willamette_wshd_avg_elevation_m"]], plots[["willamette_hrel_3"]],
  plots[["yakima_scaling_cat_num"]], plots[["yakima_log_mean_ann_pcpt_mm"]],
  plots[["yakima_wshd_avg_elevation_m"]], plots[["yakima_hrel_3"]],
  ncol = 4, align = 'v', labels = NULL
)

# Define the common axis labels and column titles
x_label <- "Longitude"
y_label <- "Latitude"
column_titles <- c("Scaling type", "Precipitation", "Elevation", "Landscape heterogeneity")

# Add the common axis labels
combined_plot <- combined_plot + 
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold", angle = 90)
  ) & 
  labs(
    x = x_label, 
    y = y_label
  )

# Add the column titles
combined_plot <- combined_plot + 
  draw_label(column_titles[1], x = 0.125, y = 1, hjust = 0.5, vjust = 0, size = 14, fontface = "bold") +
  draw_label(column_titles[2], x = 0.375, y = 1, hjust = 0.5, vjust = 0, size = 14, fontface = "bold") +
  draw_label(column_titles[3], x = 0.625, y = 1, hjust = 0.5, vjust = 0, size = 14, fontface = "bold") +
  draw_label(column_titles[4], x = 0.875, y = 1, hjust = 0.5, vjust = 0, size = 14, fontface = "bold")

# Add row labels to denote the basins
combined_plot <- combined_plot + 
  draw_label("Willamette", x = 0, y = 0.75, hjust = 0, vjust = 0.5, size = 14, fontface = "bold", angle = 90) +
  draw_label("Yakima", x = 0, y = 0.25, hjust = 0, vjust = 0.5, size = 14, fontface = "bold", angle = 90)

# Print the combined plot with annotations
print(combined_plot)

ggsave("combined_plot_with_labels.png", combined_plot, width = 16, height = 9, dpi = 300)




# Open a new window with specified size
dev.new(width = 16, height = 9)

# Now plot
print(combined_plot)











  ggplot(data = scaling_map,
    aes(
      x = longitude,
      y = latitude,
      color = scaling_cat
      )
  ) +
  geom_point()+
  # geom_point(size = 3.5) +
  # scale_color_brewer(name = "Scaling type",
  #                    palette = "YlOrRd",
  #                    direction = 1)+
  # scale_color_manual(
  #   name = "Scaling type",
  #   values = my_rcolors
  # )+
  # scale_color_colorblind()+
  # scale_color_manual(
  #   values = c("#F0E442", "#56B4E9","#009E73","#E69F00")
  # )+
scale_color_viridis_d(option = "cividis",
                      direction = -1)+
  labs(x = "Longitude",
       y = "Latitude")+
  facet_wrap(~basin_cat,
             ncol = 2,
             scales = "free")+
  theme_httn+
  theme(
    panel.background = element_rect(fill = "gray95"),
    legend.position = c(0.93,0.85),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 32),
    strip.text = element_text(size = 24, face = "bold", hjust = 0),
  )


ggsave(file=paste(
  results_png,
  ("guerrero_etal_23_scaling_maps.png"),
  sep = '/'),
  scaling_map,
  width = 20,
  height = 14,
  units = "in")


################################################################################

p <- ggplot(data = scaling_analysis_dat,
            aes(
              x = longitude,
              y = latitude,
              color = log(mean_ann_pcpt_mm)
            ))+
  scale_color_continuous_sequential(palette = "YlGnBu", rev = TRUE)+
  geom_point()+
  facet_wrap(
    ~basin_cat,
    ncol = 2,
    scales = "free"
  )
p



e <- ggplot(data = scaling_analysis_dat,
            aes(
              x = longitude,
              y = latitude,
              color = wshd_avg_elevation_m
            ))+
  scale_color_gradientn(colours = terrain.colors(10))+
  geom_point()+
  facet_wrap(
    ~basin_cat,
    ncol = 2,
    scales = "free"
  )
e

h <- ggplot(data = scaling_analysis_dat,
            aes(
              x = longitude,
              y = latitude,
              color = hrel_3,
            ))+
  scale_color_continuous_sequential(palette = "YlOrRd", rev = TRUE)+
  geom_point()+
  facet_wrap(
    ~basin_cat,
    ncol = 2,
    scales = "free"
  )
h
################################################################################
