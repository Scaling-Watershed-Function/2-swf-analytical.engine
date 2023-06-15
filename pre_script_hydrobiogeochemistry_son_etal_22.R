################################################################################
# Hydrobiogeochemical data
################################################################################

# Author: Francisco J. Guerrero

# Data source: enhdplus2v2, son et el., 2022
gc()

librarian::shelf(utils,
                 tidyverse)
# Input output file paths:

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"

phys_dat_mod8 <- read_csv(paste(raw_data,"230504_phys_dat_mod8.csv", sep = "/"),
                          show_col_types = FALSE)

# Artificial paths
art_path <- nrow(filter(phys_dat_mod8, reach_type == "ArtificialPath"))
# Canals and Ditches
can_dtch <- nrow(filter(phys_dat_mod8, reach_type == "CanalDitch"))
# Connectors
con_path <- nrow(filter(phys_dat_mod8, reach_type == "Connector"))

# Natural Channels
phys_dat_mod9 <- filter(phys_dat_mod8, reach_type == "StreamRiver")


summary(filter(phys_dat_mod9, basin == "yakima"))
summary(filter(phys_dat_mod9, basin == "willamette"))

# Total flow lines corresponding to natural channels in Yakima is 5978 and 8634 for Willamette

bgc_dat0 <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = "/"),
                    show_col_types = FALSE)

# Extracting Son et al's., data for Willamete and Yakima

son_pnw_dat_wo_na <- filter(bgc_dat0, is.na(pred_do)==FALSE)

son_pnw_dat <- filter(son_pnw_dat_wo_na, time_type == "annual")
summary(son_pnw_dat)

son_pnw_nat <- son_pnw_dat %>% 
  merge(.,
        phys_dat_mod9,
        by = "comid",
        all.x = TRUE) %>% 
  filter(reach_type == "StreamRiver")

# It yields 12755 observations with no NAs. So the current dataset would be reduced 1857 observations, 
# equivalent to 13 %
summary(son_pnw_nat)


bgc_dat <- phys_dat_mod9 %>% 
  merge(.,
        bgc_dat0 %>%
          filter(time_type=="annual") %>% 
          select(comid,
                 logRT_total_hz_s,
                 logq_hz_total_m_s,
                 logtotco2g_m2_day),
        by = "comid",
        all.x = TRUE)

bgc_plot <- na.omit(bgc_dat) %>% 
  select(reach_slope,
         logq_hz_total_m_s,
         logRT_total_hz_s,
         basin,
         stream_order) %>% 
  ggplot(aes(x = reach_slope,
             y = logRT_total_hz_s,
             color = as.factor(stream_order)))+
  scale_x_log10()+
  geom_point()+
  facet_wrap(basin~as.factor(stream_order),ncol = 7)
bgc_plot

rt_mod <- lm(logRT_total_hz_s ~ log(reach_slope) + 
               basin + 
               stream_order+
               log(mean_ann_flow_m3s)+
               log(wshd_area_km2)+
               log(reach_length_km),
             data = bgc_dat,
             na.action = na.omit)
summary(rt_mod)

bgc_dat_c1_valid <- bgc_dat %>% 
  select(logRT_total_hz_s,
         reach_slope,
         basin,
         stream_order,
         mean_ann_flow_m3s,
         wshd_area_km2,
         reach_length_km) %>% 
  mutate(pred_lgRT = predict.lm(rt_mod,.)) %>% 
  ggplot(aes(logRT_total_hz_s,
             pred_lgRT,
             color = basin))+
  geom_point()+
  geom_abline()+
  facet_wrap(~basin)
bgc_dat_c1_valid

bgc_dat_c1 <- bgc_dat %>% 
  mutate(logRT_total_hz_s = if_else(is.na(logRT_total_hz_s),
                                    predict.lm(rt_mod,.),
                                    logRT_total_hz_s))

summary(bgc_dat_c1)


bgc_plot2 <- na.omit(bgc_dat) %>% 
  ggplot(aes(x = logRT_total_hz_s,
             y = logq_hz_total_m_s,
             color = as.factor(stream_order)))+
  # scale_x_log10()+
  geom_point()+
  facet_wrap(basin~as.factor(stream_order),ncol = 7)
bgc_plot2

# hz mod

hz_mod <- bgc_dat_c1 %>% 
  select(logRT_total_hz_s,
         logq_hz_total_m_s,
         reach_slope,
         basin,
         stream_order,
         mean_ann_flow_m3s,
         wshd_area_km2,
         reach_length_km) %>% 
  mutate(logRT2 = logRT_total_hz_s^2) %>% 
  lm(logq_hz_total_m_s ~ logRT_total_hz_s+
              logRT2,
             data = .,
             na.action = na.omit)
summary(hz_mod)

bgc_dat_c2_valid <- bgc_dat %>% 
  select(logRT_total_hz_s,
         logq_hz_total_m_s,
         reach_slope,
         basin,
         stream_order,
         mean_ann_flow_m3s,
         wshd_area_km2,
         reach_length_km) %>% 
  mutate(logRT2 = logRT_total_hz_s^2) %>% 
  mutate(pred_logq_hz = predict.lm(hz_mod,.)) %>% 
  ggplot(aes(logq_hz_total_m_s,
             pred_logq_hz,
             color = basin))+
  geom_point()+
  geom_abline()+
  facet_wrap(~basin)
bgc_dat_c2_valid


bgc_dat_c2 <- bgc_dat_c1 %>% 
  mutate(logq_hz_total_m_s = if_else(is.na(logq_hz_total_m_s),
                                    predict.lm(hz_mod,.),
                                    logq_hz_total_m_s))

summary(bgc_dat_c2)


# Total respiration

bgc_plot3 <- bgc_dat_c2 %>% 
  filter(.,is.na(logtotco2g_m2_day)==FALSE) %>% 
  ggplot(aes(x = logq_hz_total_m_s,
             y = logtotco2g_m2_day,
             color = as.factor(stream_order)))+
  geom_point()+
  facet_wrap(basin~as.factor(stream_order),ncol = 7)
bgc_plot3

totc_mod <- lm(logtotco2g_m2_day ~ logq_hz_total_m_s +
                 basin+
                 stream_order,
               data = bgc_dat_c2,
               na.action = na.omit)
summary(totc_mod)

bgc_dat_c3 <- bgc_dat_c2 %>% 
  mutate(logtotco2g_m2_day = if_else(is.na(logtotco2g_m2_day),
                                     predict.lm(totc_mod,.),
                                     logtotco2g_m2_day))
summary(bgc_dat_c3)

bgc_dat_c4 <- bgc_dat_c3 %>% 
  mutate(stream_area_m2 = bnkfll_width_m*(reach_length_km*1000),
         logtotco2g_m2_sed_day = logtotco2g_m2_day/stream_area_m2,
         tot_co2g_m2_sed_day = 10^logtotco2g_m2_sed_day,
         tot_o2g_com_m2_sed_day = -tot_co2g_m2_sed_day * 32)
summary(bgc_dat_c4)

co2_plot <- bgc_dat_c4 %>% 
  ggplot(aes (x = wshd_area_km2,
              y = logtotco2g_m2_day,
              color = logRT_total_hz_s))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  facet_wrap(~basin, ncol = 2)
co2_plot

bgc_dat_c5<- bgc_dat_c4 %>% 
  merge(.,
        bgc_dat0 %>% 
          filter(time_type == "annual") %>% 
          select(comid,
                 pred_do,
                 pred_doc,
                 no3_conc_mg_l),
        by = "comid",
        all.x = TRUE)


bgc_plot4 <- bgc_dat_c5 %>% 
  filter(is.na(pred_do)==FALSE) %>% 
  ggplot(aes(x = no3_conc_mg_l,
             y = pred_doc,
             color = stream_order))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~basin, ncol = 2)
bgc_plot4

write.csv(bgc_dat_c5,paste(raw_data,"230504_bgc_dat_c5.csv", sep = '/'),
          row.names = FALSE)
