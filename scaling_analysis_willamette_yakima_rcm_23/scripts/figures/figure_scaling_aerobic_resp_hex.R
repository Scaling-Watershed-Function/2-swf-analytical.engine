################################################################################
# FIGURE: Cumulative aerobic respiration vs. watershed area (color-code: hyporheic exchange
# quantiles)
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

# Figure
cumulative_ab_resp_hex <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2_o2g_day/wshd_area_km2,
                color = accm_hzt_cat))+
  geom_point(alpha = 0.5, size = 2.5)+
  geom_smooth(data = filter(scaling_analysis_dat, hzt_cat == "Q80+"),
              aes(x = wshd_area_km2,
                  y = accm_totco2_o2g_day/wshd_area_km2),
              method = "lm",
              fullrange = TRUE,
              inherit.aes = FALSE,
              color = "#d62670")+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 1.5, linewidth = 2, linetype = "dashed") +
  scale_color_manual(name =expression(bold(paste("Cumulative \nHyporheic \nexchange \nquantiles","(",m * s^-1,")"))),
                       values = my_dcolors)+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
cumulative_ab_resp_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_resp_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


















# Willamette Cumulative aerobic respiration vs. watershed area (color-code: hyporheic exchange
# quantiles) and marginal densities

w_cumulative_ab_resp_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = accm_hzt_cat))+
  geom_point(alpha = 0.5, size = 2.5)+
  geom_smooth(data = filter(scaling_analysis_dat, hzt_cat == "Q80+"),
              aes(x = wshd_area_km2,
                  y = accm_totco2_o2g_day/wshd_area_km2),
              method = "lm",
              fullrange = TRUE,
              inherit.aes = FALSE,
              color = "#d62670")+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 1.5, linewidth = 2, linetype = "dashed") +
  scale_color_manual(name =expression(bold(paste("Cumulative \nHyporheic \nexchange \nquantiles","(",m * s^-1,")"))),
                     values = my_dcolors)+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
w_cumulative_ab_resp_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_willamette_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_resp_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_willamette_cumulative_ab_resp_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


w_cumulative_ab_resp_hex_m <- ggMarginal(w_cumulative_ab_resp_hex,
                                         groupColour = TRUE,
                                         groupFill = TRUE,
                                         xparams = list(bw=0.25),
                                         yparams = list(bw=0.25))

w_cumulative_ab_resp_hex_m


# Yakima Cumulative aerobic respiration vs. watershed area (color-code: hyporheic exchange
# quantiles) and marginal densities

librarian::shelf(ggdensity)

y_cumulative_ab_resp_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                   aes(x = wshd_area_km2,
                                       y = accm_totco2_o2g_day/wshd_area_km2,
                                       color = accm_hzt_cat))+
  geom_point(alpha = 0.5, size = 2.5)+
  geom_smooth(data = filter(scaling_analysis_dat, hzt_cat == "Q80+"),
              aes(x = wshd_area_km2,
                  y = accm_totco2_o2g_day/wshd_area_km2),
              method = "lm",
              fullrange = TRUE,
              inherit.aes = FALSE,
              color = "#d62670")+
  # geom_hdr()+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 1.5, linewidth = 2, linetype = "dashed") +
  scale_color_manual(name =expression(bold(paste("Cumulative \nHyporheic \nexchange \nquantiles","(",m * s^-1,")"))),
                     values = my_dcolors)+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
y_cumulative_ab_resp_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_yakima_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_resp_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_yakima_cumulative_ab_resp_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")

y_cumulative_ab_resp_hex_m <- ggMarginal(y_cumulative_ab_resp_hex,
                                         groupColour = TRUE,
                                         groupFill = TRUE,
                                         xparams = list(bw=0.25),
                                         yparams = list(bw=0.25))

y_cumulative_ab_resp_hex_m






cumulative_ab_resp_hex_faceted <- ggplot(data = scaling_analysis_dat,
                                         aes(x = wshd_area_km2,
                                             y = accm_totco2_o2g_day/wshd_area_km2,
                                             color = basin_cat))+
  geom_abline(slope = 0.45, intercept = 4.25)+
  geom_point(alpha = 0.5, size = 0.5)+
  scale_x_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "bl") +
  scale_color_manual(name ="Basin",
                     values = c("#5e3c99","#e66101"))+
  facet_wrap(basin_cat~accm_hzt_cat,
             ncol = 8)+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 10))
cumulative_ab_resp_hex_faceted
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.svg"),sep = '/'),
                 width = 20,
                 height = 10,
                 bg = "transparent")
print(cumulative_ab_resp_hex_faceted)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.png"),sep = '/'),
       width = 20,
       height = 10,
       units = "in")



cumulative_ab_resp_hex_entropy_faceted <- ggplot(data = scaling_analysis_dat,
                                         aes(x = wshd_area_km2,
                                             y = accm_totco2_o2g_day/wshd_area_km2,
                                             color = ent_cat_w))+
  geom_abline(slope = 0.45, intercept = 4.25)+
  geom_point(alpha = 0.5, size = 0.5)+
  scale_x_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "bl") +
  scale_color_manual(name ="Landscape \nentropy \nquantiles",
                     values = my_mcolors)+
  facet_wrap(basin_cat~accm_hzt_cat,
             ncol = 8)+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 10))
cumulative_ab_resp_hex_entropy_faceted
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.svg"),sep = '/'),
                 width = 20,
                 height = 10,
                 bg = "transparent")
print(cumulative_ab_resp_hex_faceted)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.png"),sep = '/'),
       width = 20,
       height = 10,
       units = "in")


scaling_analysis_dat$hzt_cat_r <- factor(scaling_analysis_dat$hzt_cat,
                                         levels = rev(levels(scaling_analysis_dat$hzt_cat)))

cumulative_ab_resp_hex_forest_faceted <- ggplot(data = scaling_analysis_dat,
                                                 aes(x = max(wshd_area_km2)/wshd_area_km2,
                                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                                     color = forest_scp_3))+
  # geom_abline(slope = -0.45, intercept = 6.25)+
  geom_point(alpha = 0.5, size = 0.5)+
  scale_x_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "bl") +
  facet_wrap(basin_cat~hzt_cat_r,
             ncol = 8)+
  theme_httn+
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 10))
cumulative_ab_resp_hex_forest_faceted
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.svg"),sep = '/'),
                 width = 20,
                 height = 10,
                 bg = "transparent")
print(cumulative_ab_resp_hex_faceted)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex_faceted.png"),sep = '/'),
       width = 20,
       height = 10,
       units = "in")

