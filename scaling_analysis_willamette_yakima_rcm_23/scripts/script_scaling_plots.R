###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River Basins
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md
gc()

rm()

librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 ggExtra,#adding marginal distributions (does not work with facet_wrap)
                 ggdist,# potentially working with facet_wrap
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable,# To manipulate ggplot objects
                 car,#partial residual plots
                 caret,#variable import
                 MuMIn, #ensemble models for partial residuals
                 ggeffects,
                 sp,#reading shape files
                 sf,# reading shape files
                 leaflet,#creating html widget maps
                 fedmatch,# customizable merge
                 svglite,#save plots as svg files (lighter?)
                 knitr,#compiling r-makrdown/quarto docs
                 nhdplusTools)#across the network calculation

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())


# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:20)
breaks_c <- 10^seq(-10,20,by=4)
minor_breaks <- rep(1:9, 31)*(10^rep(-10:20, each=9))

set.seed(2703)

# Landscape colors from nlcd color scale
nlcd_colors_c <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")
nlcd_colors_w <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")

# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv

# NLDC Categories
nlcd_cat_c <- c("c_water_scp","c_human_scp","c_barren_scp","c_forest_scp","c_shrub_scp","c_grass_scp")
nlcd_cat_w <- c("w_water_scp","w_human_scp","w_barren_scp","w_forest_scp","w_shrub_scp","w_grass_scp")

# Assigning names to a color scale
names(nlcd_colors_c) <- nlcd_cat_c
names(nlcd_colors_w) <- nlcd_cat_w

# Creating a quasi-sequential color palette for discrete categories
# Source: https://www.ibm.com/design/language/color/

my_dcolors <- c("#a6c8ff","#78a9ff","#4589ff","#0f62fe",
                "#00539a","#003a6d","#012749","#061727")

my_rcolors <- c("#fff1f1","#ffd7d9","#ffb3b8","#fa4d56",
                "#da1e28","#a2191f","#750e13","#2d0709")

my_mcolors <- c("#ffd6e8","#ffafd2","#ff7eb6","#ee5396",
                "#d62670","#9f1853","#740937","#510224")


#Data:

local_data <- "./data"

results <- "./results"

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"

scaling_analysis_dat <- read_csv(paste(local_data,"rcm_23_model_data.csv",sep = '/'),
                                 show_col_types = FALSE)

###### ADD DATA dictionary here!


################################################################################
# Calculating Quantiles

#I'm going to try calculating the quantiles with Hmisc::cut2, which allows
# for the inclusion of zeroes

# https://stackoverflow.com/questions/46750635/cut-and-quantile-in-r-in-not-including-zero

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

# assigning names to color scale
names(my_dcolors) <- qlabel
names(my_mcolors) <- qlabel

scaling_analysis_dat <- scaling_analysis_dat %>% 
  group_by(basin) %>% 
  mutate(ent_cat_w = factor(Hmisc::cut2(w_hrel, g = 8),labels = qlabel),
         ent_cat_c = factor(Hmisc::cut2(c_hrel, g = 8),labels = qlabel),
         rst_cat = factor(Hmisc::cut2(tot_rt_hz_s, g = 8),labels = qlabel),
         hzt_cat = factor(Hmisc::cut2(tot_q_hz_ms, g = 8),labels = qlabel),
         pct_cat = factor(Hmisc::cut2(mean_ann_pcpt_mm, g = 8),labels = qlabel),
         rnf_cat = factor(Hmisc::cut2(mean_ann_runf_mm, g = 8),labels = qlabel),
         sto_fct = as.factor(stream_order),
         forest_scp_3 = w_forest_scp + w_water_scp,
         humans_scp_3 = w_human_scp,
         shrubl_scp_3 = w_shrub_scp + w_grass_scp + w_barren_scp) %>% 
  ungroup() %>% 
  mutate(basin_cat = as.factor(if_else(basin == "yakima",
                                       "Yakima River (drier)",
                                       "Willamette River (wetter)")))

# write.csv(scaling_analysis_dat,paste(local_data,"scaling_analysis_quantiles_data.csv", sep = '/'),
#          row.names = FALSE)

p <- ggplot(data = scaling_analysis_dat,
            aes(x = reach_slope,
                fill = basin_cat,
                color = basin_cat))+
  geom_density(alpha = 0.5)+
  scale_x_log10()+
  facet_wrap(~hzt_cat, ncol = 4)
p

################################################################################
# Raw plots
################################################################################

# Note: I'm saving these plots as svg files because is not only the appropriate
# format for plot resolution, but also because is three times ligther than the
# PNG version.

# To save any of these plots as a png you can use: 

# ggsave(file=paste(results, paste0("guerrero_etal_23_scaling_local_respiration_rates.png"),sep = '/'),
#        width = 18,
#        height = 12,
#        units = "in")

# With the correspondent modifications to the file name and dimensions. 

# Local respiration rates and stream order

local_rates_plot <- ggplot(data = scaling_analysis_dat,
                           aes(x = as.factor(stream_order),
                               y = totco2g_day/stream_area_m2,
                               color = sto_fct))+
  xlab(expression(bold("Stream order (Strahler)")))+
  ylab(expression(bold(paste("Local respiration rates"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
local_rates_plot
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_scaling_local_respiration_rates.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(local_rates_plot)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_scaling_local_respiration_rates.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")



################################################################################
# WATERSHED SCALING PLOTS
################################################################################

# Cumulative hyporheic exchange

cumulative_hex <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_water_exchng_kg_day/wshd_area_km2,
                color = log(mean_ann_runf_mm,10)))+
  geom_point(alpha = 0.5, size = 2.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg/day.km^2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 4.5, linewidth = 2, linetype = "dashed") +
  scale_color_continuous(name = "[Log] Mean annual\nrunoff (mm)")+
  guides(color = guide_colourbar(barheight = 6))+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))
cumulative_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_hyporheic_exchange.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_hyporheic_exchange.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


# Cumulative residence time (not sure about the method to calculate it, perhaps 
# it needs to be flux-weighted)

cumulative_res <- ggplot(data = scaling_analysis_dat,
                         aes(x = wshd_area_km2,
                             y = accm_tot_rt_hz_s,
                             color = sto_fct))+
  geom_point(alpha = 0.5, size = 2.5)+
  geom_smooth(color="black",method = 'lm')+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Cumulative residence time (s)")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 4.5, linewidth = 2, linetype = "dashed") +
  scale_color_discrete(name = "Stream \norder")+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))
cumulative_res
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_residence_time.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_res)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_residence_time.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")

# Cumulative aerobic respiration and hyporheic exchange

cumulative_ab_res_hex <- ggplot(data = scaling_analysis_dat,
                         aes(x = accm_water_exchng_kg_day,
                             y = accm_totco2_o2g_day,
                             color = sto_fct))+
  geom_point(alpha = 0.5, size = 2.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, linewidth = 2, linetype = "dashed", intercept = -3.25) +
  scale_color_discrete(name = "Stream \norder")+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))
cumulative_ab_res_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_res_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


# Cumulative anaerobic respiration and hyporheic exchange

cumulative_anb_res_hex <- ggplot(data = scaling_analysis_dat,
                                aes(x = accm_water_exchng_kg_day,
                                    y = accm_totco2_ang_day,
                                    color = sto_fct))+
  geom_point(alpha = 0.5, size = 2.5)+
  geom_smooth(color="black", method = 'lm')+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1, ")")))) +
  ylab(expression(bold(paste(" Cumulative anaerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, linewidth = 2, linetype = "dashed", intercept = -5) +
  scale_color_discrete(name = "Stream \norder")+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))
cumulative_anb_res_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_anb_respiration_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_anb_res_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_anb_respiration_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


# Scaling cumulative aerobic respiration and hyporheic exchange

cumulative_ab_res_hex <- ggplot(data = scaling_analysis_dat,
                                aes(x = accm_water_exchng_kg_day,
                                    y = accm_totco2_o2g_day,
                                    color = sto_fct))+
  geom_point(alpha = 0.5, size = 2.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, linewidth = 2, linetype = "dashed", intercept = -3.25) +
  scale_color_discrete(name = "Stream \norder")+
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))
cumulative_ab_res_hex
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_res_hex)
dev.off()
ggsave(paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_hex.png"),sep = '/'),
       width = 20,
       height = 12,
       units = "in")


###############################################################################
# Cumulative respiration and landscape structure
################################################################################
landuse_scaling_dat <- scaling_analysis_dat %>% 
  select(wshd_area_km2,
         basin_cat,
         sto_fct,
         hzt_cat,
         w_ht,
         accm_water_exchng_kg_day,
         accm_totco2_o2g_day,
         accm_totco2_ang_day,
         forest_scp_3,
         humans_scp_3,
         shrubl_scp_3) %>% 
  gather(c(9:11),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("forest_scp_3","shrubl_scp_3","humans_scp_3"))) %>% 
  arrange(use) 

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

land_hex <- generate_lnd_plot(data = landuse_scaling_dat,
                                  accm_var = "accm_water_exchng_kg_day",
                                  ylab_expression = ylab_expression,
                                  wrap_col = 1)
land_hex
ggsave(paste(results_png,"guerrero_etal_cumulative_exchange_land_vert.png", sep = '/'),
       width = 12,
       height = 21.5,
       units = "in")
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_exchange_land_vert.svg"),sep = '/'),
                 width = 12,
                 height = 21.5,
                 bg = "transparent")
print(land_hex)
dev.off()


# Cummulative aerobic respiration
ylab_expression <-expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))

land_ab_resp <- generate_lnd_plot(data = landuse_scaling_dat,
                                  accm_var = "accm_totco2_o2g_day",
                                  ylab_expression = ylab_expression,
                                  wrap_col = 1)
land_ab_resp
ggsave(paste(results_png,"guerrero_etal_cumulative_ab_respiration_land_vert.png", sep = '/'),
       width = 12,
       height = 21.5,
       units = "in")
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_respiration_land_vert.svg"),sep = '/'),
                 width = 12,
                 height = 21.5,
                 bg = "transparent")
print(land_ab_resp)
dev.off()


# Cummulative anaerobic respiration
ylab_expression <-expression(bold(paste(" Cumulative anaerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))

land_anb_resp <- generate_lnd_plot(data = landuse_scaling_dat,
                                  accm_var = "accm_totco2_ang_day",
                                  ylab_expression = ylab_expression,
                                  wrap_col = 1)
land_anb_resp
ggsave(paste(results_png,"guerrero_etal_cumulative_anb_respiration_land_vert.png", sep = '/'),
       width = 12,
       height = 21.5,
       units = "in")
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_anb_respiration_land_vert.svg"),sep = '/'),
                 width = 12,
                 height = 21.5,
                 bg = "transparent")
print(land_anb_resp)
dev.off()


# Faceting by HEX quantiles

p <- ggplot(data = filter(landuse_scaling_dat, basin_cat == "Yakima River (dryer)"),
            aes(x = wshd_area_km2,
                y = accm_water_exchng_kg_day/wshd_area_km2,
                color = use))+
  geom_point(aes(alpha = fraction), size = 2.5)+
  geom_smooth(color = "black", linewidth = 1.5, method = 'lm', linetype = "dashed")+
  scale_x_log10()+
  scale_y_log10()+
  xlab(NULL)+
  ylab(NULL)+
  scale_color_manual(name = "Land use",
                     values = c("#008837", "#FFC618", "#7b3294"),
                     labels = c("Forestscapes", "Shrublandscapes", "Humanscapes"))+
  geom_abline(linewidth = 0.5, linetype = "dotted", intercept = 3, color = "darkred")+
  scale_alpha_continuous(guide = "none") +
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 22, face = "bold"))+
  facet_wrap(~hzt_cat, nrow = 2)
p
ggsave(paste(results_png,"yakima_river_hex_faceted.png", sep = '/'),
       width = 16,
       height = 10,
       units = "in")
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_willamette_river_hex_faceted.svg"),sep = '/'),
                 width = 16,
                 height = 10,
                 bg = "transparent")
print(p)
dev.off()






# Marginal plots test

p <- ggplot(data = filter(landuse_scaling_dat, basin_cat == "Yakima River (dryer)"),
            aes(x = wshd_area_km2,
                y = accm_totco2_o2g_day/wshd_area_km2,
                color = use))+
  geom_point(aes(alpha = fraction), size = 2.5)+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_manual(name = "Land use",
                     values = c("#008837", "#FFC618", "#7b3294"),
                     labels = c("Forestscapes", "Shrublandscapes", "Humanscapes"))+
  scale_alpha_continuous(guide = "none") +
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))
p


# Add marginal density plots along the y-axis filled by "use" with alpha 0.5
p_with_marginals <- ggExtra::ggMarginal(p, 
                                        margins = "y", 
                                        type = "density", 
                                        groupColour = TRUE,
                                        groupFill = TRUE,
                                        alpha = 0.5)

# Print the plot
print(p_with_marginals)

# Note, does not produce the intended output which should be more akin to the
# landscape heterogeneity























# Cumulative aerobic and landscape entropy

################################################################################
# WATERSHED SCALING PLOTS
################################################################################
generate_inset_plot <- function(data, basin, color_var, color_scale, legend_title) {
  plot_data <- filter(data, basin == !!basin)
  
  quant_i <- ggplot(data = plot_data,
                    aes(x = wshd_area_km2,
                        y = accm_totco2g_day / wshd_area_km2,
                        color = .data[[color_var]])) +
    geom_smooth(method = "lm", fullrange = TRUE, alpha = 0.3) +
    scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
    xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
    ylab(expression(bold(paste("Sediment respiration"," ","(", gCO[2]*m^-2*d^-1, ")")))) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    scale_color_manual(values = color_scale) +
    geom_abline(slope = 1, intercept = 2.5, linewidth = 2, linetype = "dashed") +
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

generate_plot <- function(data, basin, color_var, legend_title, color_scale, plot_title, faceting = FALSE) {
  if (faceting) {
    main_quant <- ggplot(data = data %>%
                           filter(basin == !!basin),
                         aes(x = wshd_area_km2,
                             y = accm_totco2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_point(size = 2.5, alpha = 0.35) +
      geom_point(data = data %>%
                   filter(basin == !!basin, .data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day / wshd_area_km2),
                 size = 2.5) +
      geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
      scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste(" Cumulative"," ", Respiration[Sed],"(", gCO[2] * network^-1 * d^-1, ")")))) +
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      theme_httn +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) +
      ggtitle(plot_title) +
      facet_wrap(as.formula(paste("~", color_var)), ncol = 4)  # Faceting based on color_var (rst_cat)
  } else {
    quant_ins <- generate_inset_plot(data, basin, color_var, color_scale, legend_title)
    
    plot_data <- filter(data, basin == !!basin)
    
    main_quant <- ggplot(data = plot_data,
                         aes(x = wshd_area_km2,
                             y = accm_totco2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_point(size = 2.5, alpha = 0.35) +
      geom_point(data = plot_data %>%
                   filter(.data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day / wshd_area_km2),
                 size = 2.5) +
      geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
      scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste(" Cumulative"," ", Respiration[Sed],"(", gCO[2] * network^-1 * d^-1, ")")))) +
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      annotation_custom(grob = quant_ins, xmin = 2.25, xmax = 4.35, ymin = -1.25, ymax = 2) +
      theme_httn +
      theme(legend.position = c(0.15, 0.75),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) +
      ggtitle(plot_title)
  }
  
  return(main_quant)
}
################################################################################
# Landscape entropy, residence time, hyporheic exchange, mean annual runoff, and scaling

# Yakima River Basin

# Landscape entropy
plot_basin_abbv <- "yrb"
plot_quant <- generate_plot(data = scaling_analysis_dat,
                            basin = "Yakima River",
                            color_var = "ent_cat_w",
                            legend_title = "Landscape Entropy\n(quantiles)",
                            color_scale = my_mcolors,
                            plot_title = "Yakima River Basin (dryer)",
                            faceting = FALSE)

print(plot_quant)
# Save the plot as an SVG file
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_scaling_respiration_entropy.svg"),sep = '/'),
                 width =12,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

