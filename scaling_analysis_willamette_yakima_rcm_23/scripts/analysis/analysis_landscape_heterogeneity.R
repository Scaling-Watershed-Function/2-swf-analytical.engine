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
                 entropy, usethis, GGally)

set.seed(2703)

# Data

# Local data saving
local_data <- "./data" 

#  Local figure export
results <- "./results" #For svg files that can be explored online

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"

# GitHub import
wshd_lnd_dat <- filter(read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima_rcm_23/data/231008_land_use_cover_pnw.csv",
                         show_col_types = FALSE), level == "watershed")

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

wshd_lnd_dat <- wshd_lnd_dat %>% 
  mutate(forest_scp =forest_evg + forest_dcd + forest_mxd,
         grass_scp = grass,
         shrub_scp = shrub,
         water_scp = snow + water + wetland_wood + wetland_herb,
         human_scp = pasture + crops + developed_op + developed_lw + developed_md +developed_hg,
         barren_scp = barren) %>% 
  rowwise() %>% 
  mutate(ht = entropy(c(forest_scp,
                        grass_scp,
                        shrub_scp,
                        water_scp,
                        human_scp,
                        barren_scp),
                      unit = "log2"),
         hmax = log(6,2),
         hrel = ht/hmax,
         simpson_d = 1 - ((forest_scp/100)^2 + (grass_scp/100)^2 + (shrub_scp/100)^2+
                               (water_scp/100)^2 + (human_scp/100)^2 + (barren_scp/100)^2),
         simpson_d = ifelse(simpson_d<0,0,simpson_d)) %>%
  ungroup()

p <- ggplot(data = wshd_lnd_dat,
            aes(x = hrel,
                y = simpson_d,
                color = forest_scp))+
  geom_point()
p

# We can use entropy plots to assess the role of different variables in relation to 
# changes en entropy

wshd_ent_plot <- wshd_lnd_dat %>% 
  select(20:29) %>%
  gather(variable, value,-ht, -hmax, -basin, -hrel) %>%
  ggplot(aes(x = value, y = hrel)) +
  geom_point() +
  facet_grid(rows = vars(basin), cols = vars(variable)) +
  labs(x = NULL, y = NULL) +
  theme_bw()
wshd_ent_plot


# Most of the changes in the entropy content can be traced with forest_scp, human_scp, 
# and shrub_scp. Let's take a look at a approximated plot to downstream changes in 
# entropy

# Let's take a look at the relationship between categories

pair_plot <- wshd_lnd_dat %>% 
  select(basin,
         forest_scp,
         grass_scp,
         shrub_scp,
         water_scp,
         human_scp,
         barren_scp)


# Create the paired plot
pair_plot <- ggpairs(data = pair_plot, 
                     aes(color = basin),
                     columns = c("forest_scp", "grass_scp", "shrub_scp", 
                                 "water_scp", "human_scp", "barren_scp"))

# Print the paired plot
print(pair_plot)

plot_name <- paste0("guerrero_etal_23_landscape_correlations",".jpg")

ggsave(paste(results_png,plot_name,sep = '/'),
       width = 18,
       height = 14,
       units = "in")


write.csv(wshd_lnd_dat,paste(local_data,"231008_landscape_heterogeneity_pnw.csv",sep = '/'),
          row.names = FALSE)
