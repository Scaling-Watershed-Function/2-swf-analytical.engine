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
source("./source/script_graphic_prep_design.R")

# Marginal plots hyporheic exchange

# Willamette


local_w_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                               aes(x = ctch_area_km2,
                                   y = totco2_o2g_m2_day,
                                   color = hzt_cat))+
  geom_point(alpha = 0.75, size = 1.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  # xlab(expression(bold(paste("Catchment area"," ","(", km^2, ")")))) +
  # ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")")))) +
  scale_color_manual(values = my_dcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  annotate(geom="text",
           x=75,
           y=340,
           label='bold("A")',
           parse = TRUE,
           color="black",
           size = 14)+
  theme_httn+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
local_w_resp_rates_hex

# Marginal plot

local_w_resp_rates_hex_m <- ggMarginal(local_w_resp_rates_hex,
                                         groupColour = TRUE,
                                         groupFill = TRUE,
                                         margins = "y",
                                         type = "boxplot",
                                         data = scaling_analysis_dat)
local_w_resp_rates_hex_m
# Yakima


local_y_resp_rates_hex <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                 aes(x = ctch_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = hzt_cat))+
  geom_point(alpha = 0.75, size = 1.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  # xlab(expression(bold(paste("Catchment area"," ","(", km^2, ")")))) +
  # ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")")))) +
  scale_color_manual(values = my_dcolors)+
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
        plot.margin = margin(0, 0, 0, 0, "cm"),
        plot.title = element_text(size = 32, face ="bold"))
local_y_resp_rates_hex

local_y_resp_rates_hex_m <- ggMarginal(local_y_resp_rates_hex,
                                       groupColour = TRUE,
                                       groupFill = TRUE,
                                       margins = "y",
                                       type = "boxplot")
local_y_resp_rates_hex_m
