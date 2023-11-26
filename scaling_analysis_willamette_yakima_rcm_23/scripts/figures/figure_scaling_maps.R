###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# MAPPING SCALING BEHAVIOR IN THE WIILLAMETTE AND YAKIMA RIVER BASINS
###############################################################################

gc()

rm()

################################################################################
# WATERSHED SCALING PLOTS
################################################################################
# About this plot
cat(readLines("./metadata/code_instructions.Rmd"),sep = '\n')

# Plot settings
source("./source/design_scaling_graphic_prep.R")

# Scaling Maps

scaling_map <- scaling_analysis_dat %>% 
  select(basin_cat,
         basin,
         longitude,
         latitude,
         water_exchng_kg_d,
         accm_totco2_o2g_day,
         accm_hzt_cat) %>% 
  mutate(
    quantile_num = as.numeric(sub("[^0-9]+", "", accm_hzt_cat)),
    scaling_cat = 
           case_when(basin == "willamette" & quantile_num >= 60 ~ "Superlinear",
                     basin == "willamette" & quantile_num >= 40 & quantile_num < 60 ~ "Linear",
                     basin == "willamette" & quantile_num < 40 ~ "Uncertain",
                     basin == "yakima" & quantile_num >= 80 ~ "Superlinear",
                     basin == "yakima" & quantile_num == 70 ~ "Linear",
                     basin == "yakima" & quantile_num >= 40 & quantile_num < 70 ~ "Sub-linear",
                     basin == "yakima" & quantile_num < 40 ~"Uncertain"),
    scaling_cat = factor(scaling_cat, levels = c("Uncertain",
                                                 "Sub-linear",
                                                 "Linear",
                                                 "Superlinear"))
  ) %>% 
  ggplot(
    aes(
      x = longitude,
      y = latitude,
      color = scaling_cat
      )
  ) +
  geom_point(size = 3.5) +
  # scale_color_brewer(name = "Scaling type",
  #                    palette = "YlOrRd",
  #                    direction = 1)+
  scale_color_manual(
    name = "Scaling type",
    values = my_rcolors
  )+
  labs(x = "Longitude",
       y = "Latitude")+
  facet_wrap(~basin_cat,
             ncol = 2,
             scales = "free")+
  theme_httn+
  theme(
    panel.background = element_rect(fill = "gray95"),
    legend.position = c(0.93,0.85),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 32),
    strip.text = element_text(size = 24, face = "bold", hjust = 0),
  )
scaling_map

ggsave(file=paste(
  results_png,
  ("guerrero_etal_23_scaling_maps.png"),
  sep = '/'),
  scaling_map,
  width = 20,
  height = 14,
  units = "in")







