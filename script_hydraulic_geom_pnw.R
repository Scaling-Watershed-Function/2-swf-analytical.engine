###############################################################################
# HYDRAULIC GEOMETRY DATA FOR THE WILLAMETTE AND YAKIMA RIVER BASINS
###############################################################################

# Author: Francisco J. Guerrero

# Data source: enhdplus2v2
gc()

librarian::shelf(utils,
                 tidyverse)
# Input output file paths:

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"

nhdp_2_pnw_raw <- read_csv(paste(raw_data,"230410_hydro_info_pnw.csv", sep = "/"),
                       show_col_types = FALSE)

summary(nhdp_2_pnw_raw)

nhdp_2_pnw_nas <- nhdp_2_pnw_raw %>% 
  select(ComID,
         FL_GNIS_Na,
         LENGTHKM,
         REACHCODE,
         StreamOrde,
         FromNode,
         ToNode,
         Hydroseq,
         TotLngthKm,
         TotAreaKM2,
         SLOPE,
         CatAreaKm2,
         PrecipV,
         TempV,
         MAFlowUcfs,
         RunOffV,
         HUC_4) %>% 
  rename(comid = ComID,
         stream_name = FL_GNIS_Na,
         flowline_length_km = LENGTHKM,
         reach_code = REACHCODE,
         stream_order = StreamOrde,
         from_node = FromNode,
         to_node = ToNode,
         hydroseq = Hydroseq,
         tot_flowline_length_km = TotLngthKm,
         tot_ups_area_km2 = TotAreaKM2,
         slope = SLOPE,
         catch_area_km2 = CatAreaKm2,
         mean_annual_precp = PrecipV,
         mean_annual_temp = TempV,
         mean_annual_flow_cfs = MAFlowUcfs,
         mean_annual_runoff = RunOffV,
         huc_4 = HUC_4)

summary(nhdp_2_pnw_nas)

# This data set contain all NAs (2873/19733). However these NAs are present in both
# the NHDPlus V2 and the NLCD 2001 datasets.

# Let's take a look at the NAs dataset only. I will start by looking at the flowline
# lengths within corresponding to NA values:

na_dat <- nhdp_2_pnw_nas %>% 
  select(comid,
         flowline_length_km,
         reach_code,
         huc_4,
         stream_order) %>% 
  filter(is.na(stream_order)) %>% 
  mutate(basin = if_else(huc_4==1703,"Yakima","Willamette"))


summary(na_dat)

na_plot <- ggplot(na_dat,aes(x=basin,
             y = flowline_length_km,
             color = basin ))+
  scale_y_log10()+
  geom_boxplot()
na_plot

# It looks like most of the COMIDs with NAs correspond to small low order streams
# with median flowline lenghts ~ 1 km. This needs to be corroborated in the map. 



# Let's explore some properties of this dataset: 

# Relationship between flowline lenghts and stream order

flow_line_p <- ggplot(nhdp_2_pnw_nas,
                      aes(x = as.factor(stream_order),
                          y = flowline_length_km,
                          color = as.factor(stream_order),
                          fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~huc_4, ncol = 2)
flow_line_p
  
# Cummulative flowline length and stream order
tot_flow_line_p <- ggplot(nhdp_2_pnw_nas,
                      aes(x = as.factor(stream_order),
                          y = tot_flowline_length_km,
                          color = as.factor(stream_order),
                          fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~huc_4, ncol = 2)
tot_flow_line_p

# flowline length and catchment area
flow_line_area_p <- ggplot(nhdp_2_pnw_nas,
                           aes(x = catch_area_km2,
                               y = flowline_length_km,
                               color = as.factor(stream_order),
                               fill = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  geom_smooth(aes(x = catch_area_km2,
                  y = flowline_length_km),
              inherit.aes = FALSE,
              method = "lm")+
  scale_x_log10()+
  scale_y_log10()#+
  # facet_wrap(~huc_4, ncol = 2)
flow_line_area_p


# Flowline length and watershed area
flow_line_w_area_p <- ggplot(nhdp_2_pnw_nas,
                               aes(x = tot_ups_area_km2,
                                   y = flowline_length_km,
                                   color = as.factor(stream_order),
                                   fill = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~huc_4, ncol = 2)
flow_line_w_area_p

# Cumulative Flowline length and watershed area
tot_flow_line_area_p <- ggplot(nhdp_2_pnw_nas,
                               aes(x = tot_ups_area_km2,
                                   y = tot_flowline_length_km,
                                   color = as.factor(stream_order),
                                   fill = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~huc_4, ncol = 2)
tot_flow_line_area_p

# Mean annual precipitation and stream orders (to be used as part of the calculation
# of bankfull width)

map_order_p <- ggplot(nhdp_2_pnw_nas,
                      aes(x = mean_annual_precp, 
                          fill = as.factor(stream_order),
                          color = as.factor(stream_order)))+
  geom_density(alpha = 0.5)+
  scale_x_log10()+
  facet_wrap(~huc_4, ncol = 2)
map_order_p

# So there is variation of MAP across stream orders (which I take as indicator that 
# MAP values are not the same across the watershed)

# We will estimate bankfull width following Bechie and Imachi (2013):

# W = 0.177(A^0.397)(MAP^0.453) with precipt in cm/year

nhdp_2_pnw_na <- nhdp_2_pnw_nas %>% 
  mutate(bnkf_width_m = 0.177*(tot_ups_area_km2^0.397)*(mean_annual_precp/10)^0.453) %>% 
  mutate(stream_area_m2 = (flowline_length_km/10)*bnkf_width_m)

# Stream area and order
stream_area_p <- ggplot(nhdp_2_pnw_na,
                          aes(x = as.factor(stream_order),
                              y = stream_area_m2,
                              color = as.factor(stream_order),
                              fill = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  facet_wrap(~huc_4, ncol = 2)
stream_area_p

# It seems that we have some entities (comids) with total stream area = 0. Let's
# take a look

area_0 <- nhdp_2_pnw_nas %>% 
  filter(tot_ups_area_km2==0)
area_0

summary(area_0)

# 43 data points with short stream lengths, order 1, with a total upstream area = 0.Yet these
# points have non-zero mean annual runoff

runoff_p <- ggplot(nhdp_2_pnw_nas,
                  aes(x = tot_flowline_length_km,
                      y = mean_annual_runoff,
                      color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()
runoff_p

# I will replace the missing areas with the predicted value from flowline length and catchment
# area

reg_dat <- filter(nhdp_2_pnw_nas,catch_area_km2>0)

# All data with NAs, correspond to catchment areas == 0 km2

pred_c_area <- lm(log(catch_area_km2)~log(flowline_length_km),data=reg_dat)
summary(pred_c_area)

# We are going to replace zero values with predicted values. I will first explore 
# which other variables are correlated with catchment area

pair_plot <- reg_dat %>% 
  select(stream_order,
         tot_flowline_length_km,
         tot_ups_area_km2,
         slope,
         catch_area_km2,
         mean_annual_precp,
         mean_annual_flow_cfs,
         mean_annual_temp,
         mean_annual_runoff) %>% 
  mutate(st_ord = stream_order,
         log_tot_flowline = log(tot_flowline_length_km,10),
         log_tot_w_area = log(tot_ups_area_km2,10),
         log_slope = log(slope,10),
         log_c_area = log(catch_area_km2,10),
         log_pt = log(mean_annual_precp,10),
         log_q = log(mean_annual_flow_cfs)) %>% 
  select(stream_order,
         log_tot_flowline,
         log_tot_w_area,
         log_slope,
         log_c_area,
         log_pt,
         log_q) %>% 
  pairs(.)
pair_plot
  
  pairs(.,c(10:16))
pair_plot