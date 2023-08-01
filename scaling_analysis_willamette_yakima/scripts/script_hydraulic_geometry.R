################################################################################
# Scaling watershed function: Data Analysis-Hydraulic Geometry
################################################################################

# Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table,
                 betareg,
                 Hmisc)

phys_hyporheic_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/processed_data/river_corridor_physical_hyporheic_characteristics/data/qaqc_river_corridors_physical_hyporheic_char.csv",
                               show_col_types = FALSE)

# Checking relationship between watershed area and cumulative stream area

p <- ggplot(data = nsi_rcm_phys_qaqc_dat,
            aes(x = wshd_area_km2,
                y = accm_stream_area_m2))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# Checking scaling exponents

sa_exp_mod <- lm(log(accm_stream_area_m2)~log(wshd_area_km2),
                 data = filter(nsi_rcm_phys_qaqc_dat,basin == "yakima"))
summary(sa_exp_mod)

confint(sa_exp_mod)

# Confidence interval [1.083248, 1.089011] does not include 1.00 for Willamette
# Confidence interval [1.095475, 1.106150] does not include 1.00 for Yakima

# Predicting D50 values

d50_mod <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+log(mean_ann_flow_m3s)+log(wshd_area_km2))*basin+stream_order,
              data = nsi_rcm_phys_qaqc_dat,
              na.action = na.omit)

summary(d50_mod)

# And fill d50 gaps across our dataset

nsi_rcm_phys_qaqc_dat <-  nsi_rcm_phys_qaqc_dat %>% 
  mutate(pred_d50_m = exp(predict.lm(d50_mod,.)),
         pred_d50_mm = pred_d50_m*1000,
         pred_d50_um = pred_d50_mm*1000)
summary(nsi_rcm_phys_qaqc_dat)


# Comparing exisiting vs. predicted d5 data

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m))+
  geom_point(alpha = 0.55)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# D50 changes with stream order
p <- ggplot(data = nsi_rcm_phys_qaqc_dat, 
            aes(x = as.factor(stream_order),
                y = pred_d50_m,
                color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.55)+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# Compared to changes in totco2
p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(t_co2g_day>0), 
            aes(x = as.factor(stream_order),
                y = t_co2g_day,
                color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.55)+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# Now let's calculate the expected value for stream width according to downstream
# hydraulic geometry

nsi_rcm_phys_qaqc_dat <-  nsi_rcm_phys_qaqc_dat %>% 
  mutate(theor_stream_width_m = 1.33*(mean_ann_flow_m3s)^0.44 * (pred_d50_m)^-0.11 * (reach_slope)^-0.22,
         theor_stream_area_m2 = reach_length_km*1000*theor_stream_width_m,
         accm_theor_stream_area_m2 = calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = theor_stream_area_m2))) 


# Checking relationship between watershed area and cumulative stream area

p <- ggplot(data = nsi_rcm_phys_qaqc_dat,
            aes(x =wshd_area_km2,
                y = accm_theor_stream_area_m2))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p


# Checking scaling exponents

sa_exp_mod <- lm(log(accm_theor_stream_area_m2)~log(wshd_area_km2),
                 data = filter(nsi_rcm_phys_qaqc_dat,basin == "yakima"))

confint(sa_exp_mod)

# Confidence interval [1.145193, 1.153920] does not include 1.00 for Willamette
# Confidence interval [1.200390, 1.213823] does not include 1.00 for Yakima
