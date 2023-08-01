################################################################################
# Scaling watershed function: Data Analysis-Interpolation & Cumulative values
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

scaling_resp_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/processed_data/river_corridor_physical_hyporheic_characteristics/data/qaqc_river_corridors_physical_hyporheic_char.csv",
                             show_col_types = FALSE)

nsi_rcm_ntwk_dat <- st_transform(st_read(paste(source_data,"river_corridors_respiration_geom.shp",sep = "/")),4326)



# Filling gaps for slope and roughness with: interpolate_missing_values()

# Let's start with reach slope, which we expect should be related to ctch_area_km2
# and ctch_basin_slope

# We will use a pair of functions to interpolate missing values (n<50) (e.g., roughness, reach_slope)
# Helper function to get immediate neighbors' median value
get_immediate_neighbors_median <- function(data, column, comid, tocomid) {
  immediate_neighbors <- c(comid, tocomid)
  values <- data[[column]][data$comid %in% immediate_neighbors & data[[column]] >= 0]
  median_value <- median(values, na.rm = TRUE)
  return(median_value)
}

# Helper function to calculate subsample based on specific conditions
calculate_subsample <- function(data, column, i) {
  same_basin <- data$basin == data$basin[i]
  pcpt_range <- range(data$mean_ann_pcpt_mm[same_basin])
  area_range <- range(data$wshd_area_km2[same_basin])
  pcpt_bin <- cut(data$mean_ann_pcpt_mm, breaks = seq(pcpt_range[1], pcpt_range[2], length.out = 10))
  area_bin <- cut(data$wshd_area_km2, breaks = seq(area_range[1], area_range[2], length.out = 10))
  subsample <- data[[column]][same_basin & (data$stream_order == data$stream_order[i]) &
                                (pcpt_bin == pcpt_bin[i]) & (area_bin == area_bin[i])]
  return(subsample)
}

# Main function to interpolate missing values
interpolate_missing_values <- function(data, column, regression = TRUE) {
  
  column <- rlang::sym(column)
  
  data <- data %>%
    mutate(!!column := ifelse(!!column < 0 | is.na(!!column), NA, !!column))
  
  for (i in seq_len(nrow(data))) {
    
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[rlang::as_string(column)]][i])) {
      immediate_median <- get_immediate_neighbors_median(data, rlang::as_string(column), data$comid[i], data$tocomid[i])
      
      # If there are no immediate neighbors, replace with the median value from the defined subsample
      if (is.na(immediate_median)) {
        # Calculate the subsample based on 'stream_order', 'mean_ann_pcpt_mm', 'wshd_area_km2', and 'basin'
        subsample <- calculate_subsample(data, rlang::as_string(column), i)
        immediate_median <- median(subsample, na.rm = TRUE)
      }
      
      # If the value is still NA and regression is TRUE, replace with the predicted value from the log-linear model
      if (is.na(immediate_median) & regression) {
        # Make sure we only use rows with non-NA and positive values for 'mean_ann_pcpt_mm' and 'wshd_area_km2' 
        # to fit the model
        valid_rows <- !is.na(data[[rlang::as_string(column)]]) & data$mean_ann_pcpt_mm > 0 & data$wshd_area_km2 > 0
        # Create the formula dynamically
        formula <- as.formula(paste(rlang::as_string(column), "~ log(stream_order) + log(mean_ann_pcpt_mm) + log(wshd_area_km2)"))
        model <- lm(formula, data = data[valid_rows, ])
        # Predict the value for the current row
        immediate_median <- as.numeric(predict(model, newdata = data[i, ])[1])
        
        # If the predicted value is less than 0, replace with the minimum positive value in the column
        if (immediate_median < 0) {
          immediate_median <- min(data[[rlang::as_string(column)]][data[[rlang::as_string(column)]] > 0], na.rm = TRUE)
        }
      }
      
      # Assign the calculated value to the missing value
      data[[rlang::as_string(column)]][i] <- immediate_median
    }
  }
  
  return(data)
}

# Use the function
roughness_int <- interpolate_missing_values(data = nsi_rcm_phys_dat_m4 %>% 
                                              select(comid,
                                                     tocomid,
                                                     basin,
                                                     stream_order,
                                                     mean_ann_pcpt_mm,
                                                     wshd_area_km2,
                                                     roughness),
                                            column = "roughness",
                                            regression = TRUE)

reach_slope_int <- interpolate_missing_values(data = nsi_rcm_phys_dat_m4 %>% 
                                                mutate(reach_slope = ifelse(reach_slope == 0.00000001,
                                                                            NA,
                                                                            reach_slope)) %>% 
                                                select(comid,
                                                       tocomid,
                                                       basin,
                                                       stream_order,
                                                       mean_ann_pcpt_mm,
                                                       wshd_area_km2,
                                                       reach_slope),
                                              "reach_slope",
                                              regression = TRUE)

totco2_int <- interpolate_missing_values(data = nsi_rcm_phys_dat_m4 %>% 
                                           select(comid,
                                                  tocomid,
                                                  basin,
                                                  stream_order,
                                                  mean_ann_pcpt_mm,
                                                  wshd_area_km2,
                                                  t_co2g_day),
                                         "t_co2g_day",
                                         regression = TRUE)

summary(roughness_int)
summary(reach_slope_int)
summary(totco2_int)


p <- ggplot(data = reach_slope_int,
            aes(x = wshd_area_km2,
                y = reach_slope,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

p <- ggplot(data = totco2_int %>% 
              filter(t_co2g_day>0),
            aes(x = wshd_area_km2,
                y = t_co2g_day,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(data = totco2_int %>% 
                filter(t_co2g_day > 0),
              aes(x = wshd_area_km2,
                  y = t_co2g_day),
              inherit.aes = FALSE)+
  facet_wrap(~basin, ncol = 2)
p


nsi_rcm_phys_dat_m5 <- nsi_rcm_phys_dat_m4 %>%
  select(-c(reach_slope,roughness,t_co2g_day)) %>% 
  merge(.,
        roughness_int %>% 
          select(comid,
                 roughness),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        reach_slope_int %>% 
          select(comid,
                 reach_slope),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        totco2_int %>% 
          select(comid,
                 t_co2g_day),
        by = "comid",
        all.x = TRUE) 

summary(nsi_rcm_phys_dat_m5)


# We observe a number of datapoints with reach_slope = 0.00000001. These correspond
# to default values assigned at NHDPlus when no other values were available.  Let's
# take a look

summary(filter(nsi_rcm_phys_dat_m5, reach_slope < 0.0000001))

# We find 229 of these values in this dataset (~ 1%) which seems better than the updated
# version of NHDPlus 2.1. (~6%)

# Recalculating wshd stream density, and cumulative variables
nsi_rcm_phys_dat_m6 <-  nsi_rcm_phys_dat_m5 %>% 
  select(-c(accm_basin_area_km2,
            accm_basin_slope,
            accm_stream_slope,
            accm_stream_dens)) %>% 
  mutate(stream_area_m2 = (reach_length_km*bnkfll_width_m)*1000) %>% 
  group_by(basin) %>% 
  mutate(across(c(wshd_stream_dens,
                  tot_stream_length_km,
                  wshd_area_km2,
                  ctch_area_km2,
                  ctch_stream_dens,
                  ctch_basin_slope,
                  reach_slope,
                  t_co2g_day,
                  stream_area_m2), ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = .x))) %>% 
           set_names(paste0("accm_", names(select(., wshd_stream_dens:stream_area_m2))))) %>% 
  ungroup()

summary(nsi_rcm_phys_dat_m6)

test_dat_connectivity <- nsi_rcm_phys_dat_m6 %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# Connectivity is maintained