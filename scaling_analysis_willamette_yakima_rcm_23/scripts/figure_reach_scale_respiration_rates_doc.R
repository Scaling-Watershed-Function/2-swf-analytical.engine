################################################################################
# FIGURE: Local respiration rates color coded by DOC concentrations
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

local_w_resp_rates_doc <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                                 aes(x = ctch_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = doc_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,150)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.0000001,1000)) +
  scale_color_manual(name = expression(bold(paste("Dissolved \norganic \ncarbon (mg/l) \nquantiles"))),
                     values = my_rcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=75,
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
local_w_resp_rates_doc

# Create a separate boxplot
w_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"), 
                         aes(x = reorder(doc_cat, -totco2_o2g_m2_day, FUN = median, descending = TRUE), 
                             y = totco2_o2g_m2_day,
                             color = doc_cat,
                             fill = doc_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.0000001,1000))+
  scale_color_manual(values = my_rcolors)+
  scale_fill_manual(values = my_rcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
w_boxplot_plot

# Arrange the plots side by side using grid.arrange
w_combined_plot <- grid.arrange(local_w_resp_rates_doc, 
                                w_boxplot_plot, 
                                ncol = 2,
                                widths = c(3,0.5))
w_combined_plot


# Yakima
local_y_resp_rates_doc <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                 aes(x = ctch_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = doc_cat))+
  geom_point(alpha = 0.75, size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,150)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.0000001,1000)) +
  scale_color_manual(values = my_rcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=75,
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
local_y_resp_rates_doc

# Create a separate boxplot
y_boxplot_plot <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"), 
                         aes(x = reorder(doc_cat, -totco2_o2g_m2_day, FUN = median, descending = TRUE), 
                             y = totco2_o2g_m2_day,
                             color = doc_cat,
                             fill = doc_cat)) +
  geom_boxplot(alpha = 0.85) +
  xlab("Category") +
  ylab("Respiration") +
  scale_y_log10(limits = c(0.0000001,1000))+
  scale_color_manual(values = my_rcolors)+
  scale_fill_manual(values = my_rcolors)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
y_boxplot_plot

# Arrange the plots side by side using grid.arrange
y_combined_plot <- grid.arrange(local_y_resp_rates_doc, 
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
                              bottom = textGrob(expression(bold(paste("Catchment area"," ","(", km^2, ")"))),
                                                gp = gpar(col = "black", fontsize = 44)),
                              ncol=1)
combined_plot
ggsave(file=paste(results_png, paste0("guerrero_etal_23_local_ab_resp_wyrb_doc.png"),sep = '/'),
       combined_plot,
       width = 16,
       height = 24,
       units = "in")



