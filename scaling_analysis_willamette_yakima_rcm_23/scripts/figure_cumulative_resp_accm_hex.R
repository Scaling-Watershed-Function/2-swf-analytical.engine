################################################################################
# FIGURE: Cumulative co2 production color coded by cumulative hyporheic exchange
###############################################################################
gc()

rm()

################################################################################
# WATERSHED SCALING PLOTS
################################################################################
# About this plot
cat(readLines("./metadata/code_instructions.Rmd"),sep = '\n')

# Plot settings
source("./source/script_graphic_prep_design.R")

# Marginal plots hyporheic exchange

# Willamette

accm_w_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = accm_hzt_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = expression(bold(paste("Cumulative \nhyporheic \nexchange\nquantiles"))),
                     values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.025,
           y=340000,
           label='bold("A")',
           parse = TRUE,
           color="black",
           size = 14)+
  theme_httn+
  theme(legend.position = c(0.90,0.15),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 18),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 32),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
accm_w_resp_rates_hex

# Create a separate boxplot
w_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"), 
                         aes(x = reorder(accm_hzt_cat, -accm_totco2_o2g_day/wshd_area_km2, FUN = median, descending = TRUE), 
                             y = accm_totco2_o2g_day/wshd_area_km2,
                             color = accm_hzt_cat,
                             fill = accm_hzt_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.001,500000))+
  scale_color_manual(values = my_dcolors)+
  scale_fill_manual(values = my_dcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
w_boxplot_plot

# Arrange the plots side by side using grid.arrange
w_combined_plot <- grid.arrange(accm_w_resp_rates_hex, 
                                w_boxplot_plot, 
                                ncol = 2,
                                widths = c(3,0.5))
w_combined_plot


# Yakima
accm_y_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = accm_hzt_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.025,
           y=340000,
           label='bold("B")',
           parse = TRUE,
           color="black",
           size = 14)+
  theme_httn+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 32),
        plot.margin = margin(-0.1, 0, -0.1, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
accm_y_resp_rates_hex

# Create a separate boxplot
y_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"), 
                         aes(x = reorder(accm_hzt_cat, -accm_totco2_o2g_day/wshd_area_km2, FUN = median, descending = TRUE), 
                             y = accm_totco2_o2g_day/wshd_area_km2,
                             color = accm_hzt_cat,
                             fill = accm_hzt_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.001,500000))+
  scale_color_manual(values = my_dcolors)+
  scale_fill_manual(values = my_dcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
y_boxplot_plot

# Arrange the plots side by side using grid.arrange
y_combined_plot <- grid.arrange(accm_y_resp_rates_hex, 
                                y_boxplot_plot, 
                                ncol = 2,
                                widths = c(3,0.5))
y_combined_plot


# Yakima and Willamette plot

combined_plot <- grid.arrange(w_combined_plot,
                              y_combined_plot,
                              left = textGrob(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")"))),
                                              rot = 90,
                                              just = "centre",
                                              gp = gpar(col = "black", fontsize = 44)),
                              bottom = textGrob(expression(bold(paste("Watershed area"," ","(", km^2, ")"))),
                                                gp = gpar(col = "black", fontsize = 44)),
                              ncol=1)
combined_plot
ggsave(file=paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_wyrb_hex.png"),sep = '/'),
       combined_plot,
       width = 16,
       height = 24,
       units = "in")



