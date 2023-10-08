################################################################################
# FIGURE: Slopes, Intercepts, and R-squared values for continuous regression 
# of Cumulative aerobic respiration vs. watershed area along gradients of hyporheic
# exchange and land use
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

library(dplyr)
library(purrr)

p <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = bnkfll_depth_m/bnkfll_width_m,
                color = rnf_cat))+
  geom_point()+
  scale_x_log10()+
  scale_color_manual(values = my_dcolors)+
  facet_wrap(~basin, ncol = 2)
p


p <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = reach_slope,
                color = rnf_cat))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_manual(values = my_dcolors)+
  facet_wrap(~basin, ncol = 2)
p


p <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_water_exchng_kg_day/wshd_area_km2,
                color = ent_cat_w))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  scale_color_manual(values = my_mcolors)+
  facet_wrap(~basin, ncol = 2)
p



p <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = accm_water_exchng_kg_day/wshd_area_km2,
                color = log((tot_q_hz_ms*stream_area_m2)/mean_ann_flow_m3s)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(basin~hzt_cat, nrow = 2)+
  theme(legend.position = "none")
p

p <- ggplot(data = scaling_analysis_dat,
            aes(x = wshd_area_km2,
                y = (tot_q_hz_ms*stream_area_m2)/mean_ann_flow_m3s,
                color = forest_scp_3))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
p





p <- ggplot(data = scaling_analysis_dat,
            aes(x = reach_slope,
                y = forest_scp_3))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~basin, ncol = 2)
p


scaling_mw_dat <- scaling_analysis_dat %>% #mw = moving window analyses
  select(basin,
         wshd_area_km2,
         accm_totco2_o2g_day,
         reach_slope,
         tot_q_hz_ms,
         forest_scp_3,
         humans_scp_3,
         shrubl_scp_3)



result <- scaling_mw_dat %>%
  arrange(basin, desc(forest_scp_3)) %>%
  group_by(basin) %>%
  mutate(
    window_size = ceiling(n() / 1000)  # Calculate window size to achieve around 160 rows
  ) %>%
  group_modify(~ {
    group_data <- .
    window_size <- first(group_data$window_size)
    n_rows <- nrow(group_data)
    window_results <- list()
    
    for (i in seq(1, n_rows, by = window_size)) {
      start <- i
      end <- min(i + window_size - 1, n_rows)
      window_data <- slice(group_data, start:end)
      lm_result <- lm(log(accm_totco2_o2g_day / wshd_area_km2) ~ log(wshd_area_km2), data = window_data)
      
      window_results[[i]] <- data.frame(
        slope = coef(lm_result)[2],
        intercept = coef(lm_result)[1],
        r_squared = summary(lm_result)$r.squared,
        avg_tot_q_hz_ms = mean(window_data$tot_q_hz_ms),
        avg_forest_scp_3 = mean(window_data$forest_scp_3),
        avg_humans_scp_3 = mean(window_data$humans_scp_3),
        avg_shrubl_scp_3 = mean(window_data$shrubl_scp_3),
        sdv_reach_slope = sd(window_data$reach_slope)
      )
    }
    
    # Combine the window results into a data frame
    window_results_df <- do.call(rbind, window_results)
    
    # Add row numbers for reference
    window_results_df$window_number <- 1:nrow(window_results_df)
    
    window_results_df
  })

# View the resulting data frame
head(result)


p <- ggplot(data = result,
            aes(x = sdv_reach_slope,
                y = avg_tot_q_hz_ms))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin,ncol = 2)
p

p <- ggplot(data = result, 
            aes(x = avg_forest_scp_3,
                y = r_squared,
                color = basin))+
  geom_point(size = 1.5, alpha = 0.5)+
  # scale_y_continuous(limits = c(-5,5))+
  geom_smooth(method = "loess",
              linewidth = 0.5,
              span = 0.8)
p

p_m <- p + ggMarginal(data = result,
                     x = avg_tot_q_hz_ms,
                     y = r_squared,
                     groupColour = TRUE, 
                     type = "density")
p_m
