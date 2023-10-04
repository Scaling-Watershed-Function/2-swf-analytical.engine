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
source("./scripts/script_graphic_prep_design.R")

# Figure
cumulative_ab_resp_hex_faceted <- ggplot(data = scaling_analysis_dat,
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = log(doc_stream_mg_l)))+
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
  # scale_color_manual(name =expression(bold(paste("Cumulative \nHyporheic \nexchange \nquantiles","(",m * s^-1,")"))),
  #                    values = my_dcolors)+
  facet_wrap(basin_cat~hzt_cat, nrow = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
cumulative_ab_resp_hex_faceted
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