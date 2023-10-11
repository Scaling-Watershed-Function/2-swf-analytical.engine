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

# Figure function
source("./source/script_scaling_plots_insets.R")


# Willamette River Basin
w_plot_quant <- generate_plot(data = scaling_analysis_dat,
                            basin = "willamette",
                            color_var = "accm_hzt_cat",
                            legend_title = "Cumulative \nhyporheic \nexchange \n(quantiles)",
                            label_plot = 'bold("A")',
                            color_scale = my_dcolors,
                            faceting = FALSE)
w_plot_quant

wrb_scaling_plot <- w_plot_quant +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))
wrb_scaling_plot

# Yakima River Basin
y_plot_quant <- generate_plot(data = scaling_analysis_dat,
                            basin = "yakima",
                            color_var = "accm_hzt_cat",
                            legend_title = "Cumulative \nhyporheic \nexchange \n(quantiles)",
                            label_plot = 'bold("B")',
                            color_scale = my_dcolors,
                            faceting = FALSE)
y_plot_quant

yrb_scaling_plot <- y_plot_quant +
  theme(legend.position = "none",
        axis.title = element_blank(),
        plot.margin = margin(-0.1, 0, -0.1, 0, "cm"),)
yrb_scaling_plot

combined_plot <- grid.arrange(wrb_scaling_plot,
                              yrb_scaling_plot,
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
       width = 14,
       height = 24,
       units = "in")










# Marginal plots hyporheic exchange

# Willamette

accm_w_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = accm_hzt_cat))+
  geom_abline(slope = 1, linetype = "dashed", linewidth = 0.85)+
  geom_point(alpha = 0.75, size = 3.5)+
  geom_smooth(method = 'lm',
              fullrange = TRUE)+
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
  geom_abline(slope = 1, linetype = "dashed", linewidth = 0.85)+
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

# Faceted Plot

accm_facet_resp_rates_hex <- ggplot(data = scaling_analysis_dat %>% 
                                      gather(key = "Cover",
                                             value = "fraction",
                                             c(forest_3scp,
                                               shrub_3scp,
                                               human_3scp)) %>% 
                                      filter(fraction >80) %>% 
                                      mutate(Cover = fct_relevel(Cover,c("human_3scp",
                                                            "shrub_3scp",
                                                            "forest_3scp"))) %>% 
                                      filter(accm_hzt_cat == "Q80+" | accm_hzt_cat == "Q10"),
                                    aes(x = wshd_area_km2,
                                    y = accm_totco2_o2g_day/wshd_area_km2,
                                    color = Cover))+
  geom_abline(slope = 1, linetype = "dashed", linewidth = 0.85)+
  geom_point(aes(alpha = fraction), size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = "Land cover",
                     values = c("#7b3294", "#FFC618","#008837"),
                     labels = c("Humanscapes", "Shrublandscapes", "Forestscapes")) +
  ylab(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")"))))+
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")"))))+
  annotation_logticks(size = 0.75, sides = "tblr") +
  guides(alpha = "none")+
  theme_httn+
  theme(legend.position = c(0.90,0.15),
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 18),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))+
  facet_wrap(basin_cat~accm_hzt_cat, nrow = 2)
  accm_facet_resp_rates_hex
