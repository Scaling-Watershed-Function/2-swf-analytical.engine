###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis)

set.seed(2703)


################################################################################
# Reduced dimensionality -scapes
################################################################################

# We can combine the results from the information contribution analysis to reduce
# the dimensionality of the land use data without loosing important information. 

# In the case of qualitative variables, our first category should
# start with the land cover with the highest information contribution, in this case,
# forest_evg. Since other forest types contribute with less information, we can group 
# them within the same category, that we will refer to as "Forestcapes". The next 
# land use with the second largest information contribution is grass, although followed
# immediately by pastures and crops, these last two are managed by humans and should not
# be grouped together. These landscapes units will correspond to "grasslandscapes". 
# Although grasses and shrublands can co-occur, they represent two different types of 
# communities between the Willamette valley and the Yakima River Basin. The shrubland category, 
# will be indexed as "shrublandscapes". 

# Following this approach we will have the following new categories for land use at both
# the catchment and the watershed scales.

# Forestcapes: forest_scp =forest_evg + forest_dcd + forest_mxd
# Grasslandscapes: grass_scp = grass
# Shrublandscapes: shrub_scp = shrub
# Waterscapes: water_scp = snow + water + wetland_wood + wetland_herb
# Humanscapes: human_scp = pasture + crops + developed_op + developed_lw + developed_md + developed_hg
# Barrenscapes: barren_scp = barren

# We modify our initial data set accordingly

wshd_inf_land_dat <- wshd_lnd_dat %>% 
  mutate(forest_scp =forest_evg + forest_dcd + forest_mxd,
         grass_scp = grass,
         shrub_scp = shrub,
         water_scp = snow + water + wetland_wood + wetland_herb,
         human_scp = pasture + crops + developed_op + developed_lw + developed_md +developed_hg,
         barren_scp = barren)


wshd_land_ent_dat <- wshd_inf_land_dat %>% 
  rowwise() %>% 
  mutate(ht = entropy(c(forest_scp,
                        grass_scp,
                        shrub_scp,
                        water_scp,
                        human_scp,
                        barren_scp),
                      unit = "log2"),
         hmax = log(6,2),
         hrel = ht/hmax) %>%
  ungroup()

# We can further reduce dimensions by grouping categories into three groups: forest-scapes
# will include water_scapes, shrubland-scapes could include grass_scapes and  (exploratory analysis
# showed no difference between merging or not merging these landscapes together)

write.csv(swf_land_ent_dat,paste(assets_processed,"230622_landscape_heterogeneity_pnw.csv",sep = '/'),
          row.names = FALSE)
