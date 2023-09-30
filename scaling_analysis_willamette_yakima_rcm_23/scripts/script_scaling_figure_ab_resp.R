###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River Basins
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: RIVER CORRIDOR MODEL (By Kyongho Son and Yilin Fang). Slope and D50 
# data were constrained by Downstream Hydraulic Geometry (DHG) and then used in a 
# Random Forest Model to fill gaps in hyporheic hydraulics (lateral and vertical 
# exchange fluxes and residence times). This new data were coupled with corresponding
# substrate concentrations and fed into the RCM for prediction of both aerobic and
# anaerobic respiration

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

#Calculating the quantiles with Hmisc::cut2, which allows for the inclusion of zeroes

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
         d50_cat = factor(Hmisc::cut2(d50_m, g = 8),labels = qlabel),
         accm_hzt_cat = factor(Hmisc::cut2(tot_q_hz_ms, g = 8),labels = qlabel),
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


################################################################################
# WATERSHED SCALING PLOTS
################################################################################

# Cumulative aerobic respiration vs. watershed area (color-code: hyporheic exchange
# quantiles)

cumulative_ab_resp_hex <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2_o2g_day/wshd_area_km2,
                color = accm_hzt_cat))+
  geom_point(alpha = 0.5, size = 2.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * km^-2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 1.5, linewidth = 2, linetype = "dashed") +
  scale_color_manual(name =expression(bold(paste("Cumulative \nHyporheic \nexchange \nquantiles"," ","(",m * s^-1,")"))),
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

