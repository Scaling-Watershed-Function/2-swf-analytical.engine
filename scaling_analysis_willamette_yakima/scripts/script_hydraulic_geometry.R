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

source_data <- "../../1-swf-knowledge.base/datasets/raw_data/rcm_2022_model_data/data/shapefiles"

scaling_int_dat <- read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima/data/interpolated_scaling_resp_dat.csv",
                               show_col_types = FALSE)

nsi_rcm_ntwk_dat <- st_transform(st_read(paste(source_data,"river_corridors_respiration_geom.shp",sep = "/")),4326)

local_data <- "./data"

# Checking relationship between watershed area and cumulative stream area

stream_area_plot <- ggplot(data = scaling_int_dat,
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
stream_area_plot

# Checking scaling exponents

sa_exp_mod <- lm(log(accm_stream_area_m2)~log(wshd_area_km2),
                 data = filter(scaling_int_dat,basin == "yakima"))
confint(sa_exp_mod)

# Confidence interval [1.083248, 1.089011] does not include 1.00 for Willamette
# Confidence interval [1.095475, 1.106150] does not include 1.00 for Yakima

# Predicting median particle size with downstream hydraulic geometry

summary(scaling_int_dat$d50_m)

# We have 1095 NAs and with a minimum value of 1e-06

# Let's look at a plot of D50 vs stream order and watershed area.
d50_plot_1 <- ggplot(data = scaling_int_dat,
                       aes(x = as.factor(stream_order),
                           y = d50_m,
                           color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  labs(x = "Stream order (Strahler)", y = "Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_1


d50_plot_2 <- ggplot(data = scaling_int_dat,
                       aes(x = wshd_area_km2,
                           y = d50_m,
                           color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_2

# We can observe that the data is constrained by minimum and max values. We run 
# a regression based on Lee and Julien (2006) which based on hydraulic geometry 
# principles defines an equivalence between channel width and D50. We will run an 
# analysis that include the bounded values and another without them.

d50_mod <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+
           log(mean_ann_flow_m3s)+log(wshd_area_km2))*basin+stream_order,
              data = scaling_int_dat,
              na.action = na.omit)

summary(d50_mod)

# Let's check model performance excluding default values for reach slope (>0.00001)

d50_mod_i <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+
             log(mean_ann_flow_m3s))*basin+stream_order,
              data = filter(scaling_int_dat, d50_m > 0.00001),
              na.action = na.omit)

summary(d50_mod_i)

# We find that the model excluding very low values for D50 has better performance


# And fill d50 gaps across our dataset

scaling_int_dat <-  scaling_int_dat %>% 
  mutate(pred_d50_m = exp(predict.lm(d50_mod_i,.)))
summary(scaling_int_dat)

# We find that the model has a ~260  prediction below the threshold for exclusion, 
# represents 1.6% of the data compared to the initial 742 values in the same category

filter(scaling_int_dat, d50_m < 0.00001)

# Comparing exisiting vs. predicted d50 data

p <- ggplot(data = scaling_int_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m,
                color = log(mean_ann_runf_mm)))+
  geom_point(alpha = 0.55)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# mean annual runoff creat at least two group values in Yakima River Basin

p <- ggplot(data = scaling_int_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m,
                color = log(mean_ann_runf_mm)))+
  geom_point(alpha = 0.55)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(basin~stream_order, nrow = 2)+
  theme_minimal()
p

# As expected the predictions perform better for larger particle sizes (i.e. above 1e-5)

# D50 changes with stream order
p <- ggplot(data = scaling_int_dat, 
            aes(x = as.factor(stream_order),
                y = pred_d50_m,
                color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.55)+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# Pattern with stream order is maintained

d50_plot_2 <- ggplot(data = filter(scaling_int_dat, d50_m > 0.00001),
                     aes(x = wshd_area_km2,
                         y = pred_d50_m,
                         color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Predicted Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_2

# And we observe less bias in the data

# Now let's calculate the expected value for stream width according to downstream
# hydraulic geometry

scaling_int_dat <-  scaling_int_dat %>% 
  mutate(theor_stream_width_m = 1.33*(mean_ann_flow_m3s)^0.44 * (pred_d50_m)^-0.11 * (reach_slope)^-0.22,
         theor_stream_area_m2 = reach_length_km*1000*theor_stream_width_m,
         accm_theor_stream_area_m2 = calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = theor_stream_area_m2))) 


# Checking relationship between watershed area and cumulative stream area

p <- ggplot(data = scaling_int_dat,
            aes(x =wshd_area_km2,
                y = accm_theor_stream_area_m2))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  geom_smooth(method = 'lm')+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p


# Checking scaling exponents

sa_exp_mod <- lm(log(accm_theor_stream_area_m2)~log(wshd_area_km2),
                 data = filter(scaling_int_dat,basin == "yakima"))

confint(sa_exp_mod)

# Confidence interval [1.145193, 1.153920] does not include 1.00 for Willamette
# Confidence interval [1.200390, 1.213823] does not include 1.00 for Yakima

# We replace d50 and stream area by the predicted values based on hydraulic geometry

scaling_hydraul_geom_dat <- scaling_int_dat #%>% 
#   mutate(stream_area_m2 = theor_stream_area_m2,
#          stream_width_m = theor_stream_width_m,
#          d50_m = pred_d50_m,
#          accm_stream_area_m2 = accm_theor_stream_area_m2) %>% 
#   select(-c(theor_stream_area_m2,
#             theor_stream_width_m,
#             pred_d50_m,
#             accm_theor_stream_area_m2))

# Saving New dataset
write.csv(scaling_hydraul_geom_dat,paste(local_data,"hydraulic_geom_scaling_resp_dat.csv", sep = '/'),
          row.names = FALSE)
