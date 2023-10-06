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
         basin_cat,
         sto_fct,
         accm_hzt_cat,
         w_ht,
         frst_cat,
         accm_water_exchng_kg_day,
         accm_totco2_o2g_day,
         accm_totco2_ang_day,
         forest_scp_3,
         humans_scp_3,
         shrubl_scp_3,
         mean_ann_runf_mm) %>% 
  gather(c(10:12),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("forest_scp_3","shrubl_scp_3","humans_scp_3"))) %>% 
  arrange(use) 

# Test plot

p <- ggplot(data = landuse_scaling_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2_o2g_day/wshd_area_km2,
                color =log(mean_ann_runf_mm)))+
  facet_wrap(basin_cat~frst_cat,nrow = 2)+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()
p


summary(scaling_analysis_dat)



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
                                            fraction >90),
                              accm_var = "accm_water_exchng_kg_day",
                              ylab_expression = ylab_expression,
                              wrap_col = 1)
land_hex
