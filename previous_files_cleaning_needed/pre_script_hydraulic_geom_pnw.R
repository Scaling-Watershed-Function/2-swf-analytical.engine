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

phys_dat_mod8 <- read_csv(paste(raw_data,"230504_phys_dat_mod8.csv", sep = "/"),
                       show_col_types = FALSE)


# There are two physical variables we want to calculate from this data set: D50 and 
# residence times. We will use the Hydraulic Geometry framework (HG) for these 
# calculations. 

# We will take advantage of the independent calculations of Bankfull width and depth,
# as well as the slopes, to solve equations for these variables that include D50

# For D50 we will use two approaches: 
# Lee-Julien (2006) equations for Downstream Hydraulic Geometry (Bankfull estimates) and
# Jha et al., (2022) D50-based stream power


# for both approaches we need certain constants pre-defined

g <- 9.8 #acceleration due to gravity in m/s^2
rho <- 1 #density of water (kg/m3)
sg <- 2.65 #sediment specific gravity
q_cnt <- 3.004 #multiplier constant for discharge
q_exp <- 0.426 #exponent for Q
s_exp <- -0.153# exponent for slope
d_exp <- -0.002# exponent for d50


phys_dat_hg_lj <- phys_dat_mod8 %>% 
  mutate(q_term = q_cnt * mean_ann_flow_m3s^q_exp,
         s_term = reach_slope^s_exp,
         b_term = bnkfll_width_m/(q_term * s_term),
         b_exp = 1/d_exp,
         d50_mm.lj = (b_term^b_exp))
summary(phys_dat_hg_lj)


phys_dat_hg_sp <- phys_dat_mod8 %>% 
  mutate(stream_pwr = (rho*g*mean_ann_flow_m3s*reach_slope)/bnkfll_width_m,
         b_term = (bnkfll_width_m/0.1*rho)^2/3,
         g_term = g*(sg-1),
         d50_m.sp = (b_term/g_term))

summary(phys_dat_hg_sp)

# checking magnitudes: 
summary(1/phys_dat_hg_sp$stream_pwr)

# Hyporheic exchange

c1 <- 20.595
c2 <- 0.097
fe <- 0.42
c3 <- -1.4625
c4 <- 0.6639
c5 <- 0.3232
c6 <- 1.9132

rest_dat <- phys_dat_mod8 %>% 
  select(comid,
         bnkfll_width_m,
         bnkfll_depth_m,
         mean_ann_vel_ms,
         reach_slope_length_km,
         reach_slope,
         sinuosity) %>% 
  rename(b = bnkfll_width_m,
         h = bnkfll_depth_m,
         v = mean_ann_vel_ms,
         l = reach_slope_length_km,
         s = reach_slope,
         si = sinuosity) %>% 
  mutate(f_term = (8*g*h*s)/v^2,
         u_crit = (g*h*s)^0.5,
         d_coef = 2*((b/h)^1.5)*h*u_crit,
         p_1 = c1*(1/c2*(f_term)^fe),
         p_2 = (v/u_crit)^c3,
         p_3 = (b/h)^c4,
         p_4 = ((v*l)/d_coef)^c5,
         p_5 = si^c6*(h/u_crit),
         rest_time = (p_1*p_2*p_3*p_4*p_5)^-1)

summary(rest_dat)

p <- ggplot(rest_dat,
            aes(x = s,
                y = rest_time))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()
p
