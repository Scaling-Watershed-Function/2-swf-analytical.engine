################################################################################
# FIGURE: Local respiration rates color coded by  hyporheic exchange
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
      "totco2_o2g_m2_day" = "Total CO2 (g/m²/day)",
      "stream_area_m2" = "Stream Area (m²)",
      "totco2_o2g_day" = "Total CO2 (g/day)",
      "accm_totco2_o2g_day" = "Accumulated CO2 (g/day)"
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
    size = 3, 
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
  facet_wrap(
    ~ metric + basin_cat, 
    ncol = 2,
    scales = "free_y",
    labeller = labeller(metric = metric_labels)
             )
scaling_analysis_plot








local_co2_mass_hex <- ggplot(data = scaling_analysis_dat %>% 
                                 mutate(accm_hzt_cat = factor(accm_hzt_cat,
                                                         levels = c("Q10","Q20","Q30","Q40","Q50",
                                                                    "Q60","Q70","Q80","Q90","Q100"))),
                               aes(x = wshd_area_km2,
                                   y = totco2_o2g_day,
                                   color = accm_hzt_cat))+
  facet_wrap(~basin_cat, ncol = 2)+
  # geom_smooth(method = "lm",
  #             linewidth = 3.0)+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks,
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")"))))+
  ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")"))))+
  scale_color_viridis_d(name = expression(bold(paste("Hyporheic \nexchange \nflux (m/s) \nquantiles"))))+
  # scale_color_manual(name = expression(bold(paste("Hyporheic \nexchange \nflux (m/s) \nquantiles"))),values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(intercept = 1.5, 
             linewidth = 1.0, 
             linetype = "dashed")+
  theme_httn+
  theme(legend.position = "right",
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 32),
        strip.text = element_text(size = 24, face = "bold", hjust = 0),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
local_co2_mass_hex
ggsave(file=paste(results_png, paste0("guerrero_etal_23_accm_ab_resp_wyrb_hex_lines_side_wshd_area.png"),sep = '/'),
       local_resp_rates_hex,
       width = 24,
       height = 12,
       units = "in")











# Side by side plots

local_co2_mass_hex <- ggplot(data = scaling_analysis_dat %>% 
                                 mutate(hzt_cat = factor(hzt_cat,
                                                         levels = c("Q10","Q20",
                                                                    "Q30","Q40",
                                                                    "Q50","Q60",
                                                                    "Q70","Q80",
                                                                    "Q90","Q100"))),
                                 aes(x = wshd_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = hzt_cat))+
  facet_wrap(~basin_cat, nrow = 2)+
  # geom_smooth(method = "lm",
  #             linewidth = 3.0)+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks,
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.0000001,1000)) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")"))))+
  ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")"))))+
  scale_color_manual(name = expression(bold(paste("Hyporheic \nexchange \nflux (m/s) \nquantiles"))),values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_vline(xintercept = 1000, 
             linewidth = 1.0, 
             linetype = "dashed")+
  theme_httn+
  theme(legend.position = "none",
        legend.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 32),
        strip.text = element_text(size = 24, face = "bold", hjust = 0),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
local_co2_mass_hex


ggsave(file=paste(results_png, paste0("guerrero_etal_23_local_ab_resp_wyrb_hex_lines_side_wshd_area.png"),sep = '/'),
       local_resp_rates_hex,
       width = 24,
       height = 12,
       units = "in")


local_rates_mod <- lm(log(totco2_o2g_m2_day)~(log(wshd_area_km2)+hzt_cat)*basin,
                      data = filter(scaling_analysis_dat,stream_order<6))
summary(local_rates_mod)










# Marginal plots hyporheic exchange

# Willamette

local_w_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                               aes(x = wshd_area_km2,
                                   y = totco2_o2g_m2_day,
                                   color = hzt_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  # geom_smooth()+
  # scale_x_log10(breaks = breaks, 
  #               labels = trans_format("log10", math_format(10^.x)),
  #               limits = c(0.001,150)) +
  scale_x_log10(breaks = breaks,
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.0000001,1000)) +
  scale_color_manual(name = expression(bold(paste("Hyporheic \nexchange \nflux (m/s) \nquantiles"))),values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.050,
           y=340,
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
local_w_resp_rates_hex

# Create a separate boxplot
w_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"), 
                       aes(x = reorder(hzt_cat, -totco2_o2g_m2_day, FUN = median, descending = TRUE), 
                           y = totco2_o2g_m2_day,
                           color = hzt_cat,
                           fill = hzt_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.0000001,1000))+
  scale_color_manual(values = my_dcolors)+
  scale_fill_manual(values = my_dcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
w_boxplot_plot

# Arrange the plots side by side using grid.arrange
w_combined_plot <- grid.arrange(local_w_resp_rates_hex, 
                              w_boxplot_plot, 
                              ncol = 2,
                              widths = c(3,0.5))
w_combined_plot


# Yakima
local_y_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                 aes(x = wshd_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = hzt_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  # scale_x_log10(breaks = breaks, 
  #               labels = trans_format("log10", math_format(10^.x)),
  #               limits = c(0.001,150)) +
  scale_x_log10(breaks = breaks,
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.0000001,1000)) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")"))))+
  ylab()
  scale_color_manual(values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.050,
           y=340,
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
local_y_resp_rates_hex

# Create a separate boxplot
y_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"), 
                         aes(x = reorder(hzt_cat, -totco2_o2g_m2_day, FUN = median, descending = TRUE), 
                             y = totco2_o2g_m2_day,
                             color = hzt_cat,
                             fill = hzt_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.0000001,1000))+
  scale_color_manual(values = my_dcolors)+
  scale_fill_manual(values = my_dcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
y_boxplot_plot

# Arrange the plots side by side using grid.arrange
y_combined_plot <- grid.arrange(local_y_resp_rates_hex, 
                                y_boxplot_plot, 
                                ncol = 2,
                                widths = c(3,0.5))
y_combined_plot


# Yakima and Willamette plot

combined_plot <- grid.arrange(w_combined_plot,
                              y_combined_plot,
                              left = textGrob(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")"))),
                                              rot = 90,
                                              just = "centre",
                                              gp = gpar(col = "black", fontsize = 44)),
                              bottom = textGrob(expression(bold(paste("Watershed area"," ","(", km^2, ")"))),
                                                gp = gpar(col = "black", fontsize = 44)),
                              ncol=1)
combined_plot
ggsave(file=paste(results_png, paste0("guerrero_etal_23_local_ab_resp_wyrb_hex_wshd_area.png"),sep = '/'),
       combined_plot,
       width = 16,
       height = 24,
       units = "in")

