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



###############################################################################
# Cumulative respiration and landscape structure
################################################################################

landuse_scaling_dat <- scaling_analysis_dat %>% 
  select(wshd_area_km2,
         basin,
         basin_cat,
         sto_fct,
         accm_hzt_cat,
         ht_3,
         hrel_3,
         hr3_cat,
         accm_ent_cat,
         frs3_cat,
         accm_water_exchng_kg_d,
         accm_totco2_o2g_day,
         accm_totco2_ang_day,
         forest_3scp,
         human_3scp,
         shrub_3scp,
         accm_mean_ann_runf_mm) %>% 
  gather(c(forest_3scp,
           human_3scp,
           shrub_3scp),
         key="use",
         value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("forest_3scp","shrub_3scp","human_3scp"))) %>% 
  arrange(use) 

# Willamette River Basin

accm_w_resp_rates_lulc <- ggplot(data = filter(landuse_scaling_dat, basin == "willamette"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = use))+
  geom_point(aes(alpha = fraction), size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = "Land cover",
                     values = c("#008837", "#FFC618", "#7b3294"),
                     labels = c("Forestscapes", "Shrublandscapes", "Humanscapes")) +
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
               plot.title = element_text(size = 32, face ="bold"))+
  guides(alpha = "none")
accm_w_resp_rates_lulc 


accm_w_resp_rates_hrel <- ggplot(data = filter(landuse_scaling_dat, basin == "willamette"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = hr3_cat))+
  geom_point(aes(alpha = fraction), size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = "Relative \nentropy \nquantiles",
                     values = my_mcolors) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.025,
           y=340000,
           label='bold("B")',
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
        plot.title = element_text(size = 32, face ="bold"))+
  guides(alpha = "none")
accm_w_resp_rates_hrel 

w_combined_plot <- grid.arrange(accm_w_resp_rates_lulc , 
                                accm_w_resp_rates_hrel , 
                                ncol = 2)
w_combined_plot

# Yakima River Basin

accm_y_resp_rates_lulc <- ggplot(data = filter(landuse_scaling_dat, basin == "yakima"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = use))+
  geom_point(aes(alpha = fraction), size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = "Land cover",
                     values = c("#008837", "#FFC618", "#7b3294"),
                     labels = c("Forestscapes", "Shrublandscapes", "Humanscapes")) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.025,
           y=340000,
           label='bold("C")',
           parse = TRUE,
           color="black",
           size = 14)+
  theme_httn+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 32),
        plot.margin = margin(-0.1, 0, -0.1, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
accm_y_resp_rates_lulc 


accm_y_resp_rates_hrel <- ggplot(data = filter(landuse_scaling_dat, basin == "yakima"),
                                 aes(x = wshd_area_km2,
                                     y = accm_totco2_o2g_day/wshd_area_km2,
                                     color = hr3_cat))+
  geom_point(aes(alpha = fraction), size = 3.5)+
  scale_x_log10(breaks = breaks, 
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.01,30000)) +
  scale_y_log10(breaks = breaks_c,
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(0.001,500000)) +
  scale_color_manual(name = "Relative \nentropy \nquantiles",
                     values = my_mcolors) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=0.025,
           y=340000,
           label='bold("D")',
           parse = TRUE,
           color="black",
           size = 14)+
  theme_httn+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(size = 32),
        plot.margin = margin(-0.1, 0, -0.1, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))+
  guides(alpha = "none")
accm_y_resp_rates_hrel 


y_combined_plot <- grid.arrange(accm_y_resp_rates_lulc , 
                                accm_y_resp_rates_hrel , 
                                ncol = 2)
y_combined_plot


all_combined_plot <- grid.arrange(w_combined_plot,
                                  y_combined_plot,
                                  left = textGrob(expression(bold(paste("Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")"))),
                                                  rot = 90,
                                                  just = "centre",
                                                  gp = gpar(col = "black", fontsize = 44)),
                                  bottom = textGrob(expression(bold(paste("Watershed area"," ","(", km^2, ")"))),
                                                    gp = gpar(col = "black", fontsize = 44)),
                                  nrow = 2)
all_combined_plot


ggsave(file=paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_wyrb_lulc.png"),sep = '/'),
       all_combined_plot,
       width = 32,
       height = 24,
       units = "in")













#################################################################################
#Plotting function
generate_lnd_plot <- function(data, accm_var, ylab_expression, wrap_col) {
  ggplot_obj <- ggplot(data = data,
                       aes(x = wshd_area_km2,
                           y = !!sym(accm_var) / wshd_area_km2,
                           color = use)) +
    facet_wrap(~basin_cat, ncol = wrap_col) +
    geom_abline(slope = 1.0, color = "red", linetype = "solid", linewidth = 0.75) +
    geom_point(aes(alpha = fraction), size = 2.5) +
    scale_alpha_continuous(guide = "none") + 
    scale_x_log10(breaks = breaks_c, 
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_y_log10(breaks = breaks_c, 
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
    ylab(ylab_expression) +  # Use the provided ylab expression
    scale_color_manual(name = "Land use",
                       values = c("#008837", "#FFC618", "#7b3294"),
                       labels = c("Forestscapes", "Shrublandscapes", "Humanscapes")) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    theme_httn +
    theme(legend.position = c(0.925, 0.15),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 16),
          strip.text = element_text(size = 22, face = "bold"))
  
  return(ggplot_obj)
}
# Cummulative hyporheic exchange
ylab_expression <-expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1 * km^-2, ")")))

land_hex <- generate_lnd_plot(data = filter(landuse_scaling_dat,
                                            fraction <10),
                              accm_var = "accm_water_exchng_kg_day",
                              ylab_expression = ylab_expression,
                              wrap_col = 1)
land_hex
