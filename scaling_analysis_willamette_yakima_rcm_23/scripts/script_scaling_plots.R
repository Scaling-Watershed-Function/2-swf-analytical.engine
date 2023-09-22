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
                                       "Yakima River (dryer)",
                                       "Willamette River (wetter)")))

# write.csv(scaling_analysis_dat,paste(local_data,"scaling_analysis_quantiles_data.csv", sep = '/'),
#          row.names = FALSE)

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


################################################################################
# WATERSHED SCALING PLOTS
################################################################################

# Cumulative hyporheic exchange

cumulative_hex <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_water_exchng_kg_day/wshd_area_km2,
                color = log(water_exchng_kg_day)))+
  geom_point(alpha = 0.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg/day.km^2, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 4.5, linewidth = 2, linetype = "dashed") +
  guides(color = guide_legend(title = "Local hyporheic\nexchange kg/day\n[log]")) +
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_hyporheic_exchange.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_hex)
dev.off()


# Cumulative residence time

cumulative_res <- ggplot(data = scaling_analysis_dat,
                         aes(x = wshd_area_km2,
                             y = accm_tot_rt_hz_s,
                             color = log(tot_rt_hz_s)))+
  geom_smooth(color="darkred")+
  geom_point(alpha = 0.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Cumulative residence time (s)")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, intercept = 4.5, linewidth = 2, linetype = "dashed") +
  guides(color = guide_legend(title = "Local res. time (s) [log]")) +
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_residence_time.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_res)
dev.off()


# Cumulative aerobic respiration and hyporheic exchange

cumulative_ab_res_hex <- ggplot(data = scaling_analysis_dat,
                         aes(x = accm_water_exchng_kg_day,
                             y = accm_totco2_o2g_day,
                             color = sto_fct))+
  geom_smooth(color="darkred")+
  geom_point(alpha = 0.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1, ")")))) +
  ylab(expression(bold(paste(" Cumulative aerobic"," ", Respiration[Hyp],"(", gCO[2] * d^-1, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, linewidth = 2, linetype = "dashed") +
  guides(color = guide_legend(title = "Stream order\nStrahler")) +
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_ab_resp_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_ab_res_hex)
dev.off()


# Cumulative anaerobic respiration and hyporheic exchange

cumulative_anb_res_hex <- ggplot(data = scaling_analysis_dat,
                                aes(x = accm_water_exchng_kg_day,
                                    y = accm_totco2_ang_day,
                                    color = sto_fct))+
  geom_smooth(color="darkred")+
  geom_point(alpha = 0.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Cumulative hyporheic exchange"," ","(", kg * d^-1, ")")))) +
  ylab(expression(bold(paste(" Cumulative anaerobic"," ", Respiration[Hyp],"(", gCO[2] * d^-1, ")")))) +
  annotation_logticks(size = 0.75, sides = "tblr") +
  geom_abline(slope = 1, linewidth = 2, linetype = "dashed") +
  guides(color = guide_legend(title = "Stream order\nStrahler")) +
  facet_wrap(~basin_cat, ncol = 2)+
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_cumulative_anb_respiration_hex.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(cumulative_anb_res_hex)
dev.off()

# Cumulative respiration and landscape structure

landuse_scaling_dat <- scaling_analysis_dat %>% 
  select(wshd_area_km2,
         basin_cat,
         accm_water_exchng_kg_day,
         accm_totco2_o2g_day,
         accm_totco2_ang_day,
         forest_scp_3,
         humans_scp_3,
         shrubl_scp_3) %>% 
  gather(c(6:8),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("forest_scp_3","shrubl_scp_3","humans_scp_3"))) %>% 
  arrange(use) 

generate_lnd_plot <- function(data, accm_var) {
  ggplot_obj <- ggplot(data = data,
                       aes(x = wshd_area_km2,
                           y = !!sym(accm_var) / wshd_area_km2,
                           color = use)) +
    facet_wrap(~basin_cat, ncol = 2) +
    geom_abline(slope = 1.0, color = "red", linetype = "solid", size = 0.75) +
    geom_point(aes(alpha = fraction), size = 2.5) +
    scale_alpha_continuous(guide = "none") + 
    scale_x_log10(breaks = breaks_c, 
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    scale_y_log10(breaks = breaks_c, 
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
    ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*km^-2*d^-1,")"))))+
    scale_color_manual(name = "Land use",
                       values = c("#008837", "#FFC618", "#7b3294"),
                       labels = c("Forestscapes", "Shrublandscapes", "Humanscapes")) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    theme_httn +
    theme(legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 16),
          plot.title = element_text(size = 16),
          strip.text = element_text(size = 16, face = "bold", hjust = 0))
  
  return(ggplot_obj)
}

land_ab_resp <- generate_lnd_plot(data = landuse_scaling_dat,
                                  accm_var = "accm_totco2_o2g_day")
land_ab_resp














