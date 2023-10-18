################################################################################
# Scaling and inset plots including regression lines and R2
################################################################################
generate_inset_plot <- function(data, basin, color_var, color_scale, legend_title) {
  plot_data <- filter(data, basin == !!basin)
  
quant_i <- ggplot(data = plot_data,
                  aes(x = wshd_area_km2,
                      y = accm_totco2_o2g_day / wshd_area_km2,
                      color = .data[[color_var]])) +
  geom_smooth(method = "lm", fullrange = TRUE, alpha = 0.3) +
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  scale_color_manual(values = color_scale) +
  geom_abline(slope = 1, intercept = 2.5, linewidth = 1.25, linetype = "dashed") +
  guides(color = guide_legend(title = legend_title)) +
  theme_httn +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 16),
        panel.background = element_blank()) +
  guides(alpha = "none")

# Convert quant_i ggplot into grob
quant_grob <- ggplotGrob(quant_i)

return(quant_grob)
}

generate_plot <- function(data, basin, color_var, legend_title, label_plot, color_scale, faceting = FALSE) {
  if (faceting) {
    main_quant <- ggplot(data = data %>%
                           filter(basin == !!basin),
                         aes(x = wshd_area_km2,
                             y = accm_totco2_o2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_point(size = 3.5, alpha = 0.45) +
      geom_point(data = data %>%
                   filter(basin == !!basin, .data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2_o2g_day / wshd_area_km2),
                 size = 3.5) +
      geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
      scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      theme_httn +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) +
      facet_wrap(as.formula(paste("~", color_var)), ncol = 4)  # Faceting based on color_var (rst_cat)
  } else {
    quant_ins <- generate_inset_plot(data, basin, color_var, color_scale, legend_title)
    
    plot_data <- filter(data, basin == !!basin)
    
    main_quant <- ggplot(data = plot_data,
                         aes(x = wshd_area_km2,
                             y = accm_totco2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_abline(slope = 1, intercept = 3, linewidth = 1.25, linetype = "dashed") +
      geom_point(size = 3.5, alpha = 0.45) +
      geom_point(data = plot_data %>%
                   filter(.data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day / wshd_area_km2),
                 size = 2.5) +
      scale_x_log10(breaks = breaks, 
                    labels = trans_format("log10", math_format(10^.x)),
                    limits = c(0.01,30000)) +
      scale_y_log10(breaks = breaks_c,
                    labels = trans_format("log10", math_format(10^.x)),
                    limits = c(0.001,500000)) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
      annotate(geom="text",
               x=0.01,
               y=400000,
               label= label_plot,
               parse = TRUE,
               color="black",
               size = 14)+
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      annotation_custom(grob = quant_ins, xmin = 2.25, xmax = 4.35, ymin = -1.25, ymax = 2) +
      theme_httn +
      theme(legend.position = c(0.05, 0.70),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) 
  }
  
  return(main_quant)
}