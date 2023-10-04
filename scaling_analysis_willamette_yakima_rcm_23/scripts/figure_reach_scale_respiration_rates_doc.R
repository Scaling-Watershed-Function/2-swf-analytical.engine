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
source("./scripts/script_graphic_prep_design.R")

# Marginal plots dissolved organic carbon

# First we need to create the categorical data for DOC using quantiles



# Willamette

local_w_resp_rates_doc <- ggplot(data = filter(scaling_analysis_dat, basin == "willamette"),
                                 aes(x = ctch_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = doc_cat))+
  geom_point(alpha = 0.75, size = 1.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Catchment area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")")))) +
  scale_color_manual(values = my_rcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
local_w_resp_rates_doc

local_w_resp_rates_doc_m <- ggMarginal(local_w_resp_rates_doc,
                                       groupColour = TRUE,
                                       groupFill = TRUE,
                                       margins = "y",
                                       yparams = list(bw=0.25))
local_w_resp_rates_doc_m

# Yakima

local_y_resp_rates_doc <- ggplot(data = filter(scaling_analysis_dat, basin == "yakima"),
                                 aes(x = ctch_area_km2,
                                     y = totco2_o2g_m2_day,
                                     color = doc_cat))+
  geom_point(alpha = 0.75, size = 1.5)+
  scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
  xlab(expression(bold(paste("Catchment area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste("Aerobic"," ", respiration[Hyp],"(", gCO[2] * d^-1 * m^-2, ")")))) +
  scale_color_manual(values = my_rcolors)+
  annotation_logticks(size = 0.75, sides = "tblr") +
  theme_httn+
  theme(legend.position = c(0.925, 0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 32, face = "bold"))
local_y_resp_rates_doc

local_y_resp_rates_doc_m <- ggMarginal(local_y_resp_rates_doc,
                                       groupColour = TRUE,
                                       groupFill = TRUE,
                                       margins = "y",
                                       yparams = list(bw=0.25))
local_y_resp_rates_doc_m

