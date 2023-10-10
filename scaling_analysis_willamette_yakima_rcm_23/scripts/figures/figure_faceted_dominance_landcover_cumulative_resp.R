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
         wshd_stream_dens,
         wshd_basin_slope,
         basin,
         basin_cat,
         sto_fct,
         accm_hzt_cat,
         mean_ann_flow_m3s,
         mean_ann_temp_dc,
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
         accm_wshd_stream_dens,
         accm_mean_ann_runf_mm,
         accm_stream_area_m2,
         no3_stream_mg_l,
         accm_doc_load_kg_d) %>% 
  gather(c(forest_3scp,
           human_3scp,
           shrub_3scp),
         key="use",
         value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("forest_3scp","shrub_3scp","human_3scp"))) %>% 
  arrange(use) %>% 
  mutate(dominance = as.factor(if_else(fraction > 80, "80%-Dominance",
                                       ifelse(fraction<80 & fraction > 70,"70%-Dominance",
                                              if_else(fraction<70 & fraction >60, "60%-Dominance",
                                                      if_else(fraction < 60 & fraction > 50, "50%-Dominance",
                                                              if_else(fraction <50 & fraction > 40, "40%-Dominance",
                                                                      if_else(fraction <40 & fraction > 30, "30%-Dominance",
                                                                              if_else(fraction < 30 & fraction > 20, "20%-Dominance",
                                                                                      ifelse(fraction < 20, "10%-Dominance",NA))))))))))





land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
land_cover_dominance_plot <- ggplot(data = filter(landuse_scaling_dat, dominance == "10%-Dominance" | dominance == "80%-Dominance"),
                                    aes(x = wshd_area_km2,
ggsave(file=paste(results_png, paste0("guerrero_etal_23_cumulative_ab_resp_wyrb_dominance_80_10.png"),sep = '/'),
       land_cover_dominance_plot,
       width = 12,
       height = 12,
       units = "in")