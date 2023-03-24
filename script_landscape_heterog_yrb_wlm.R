###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

################################################################################
#Settings:
################################################################################
#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy)

set.seed(2703)


# NLDC Legend Colors and categories
# NLDC Colors

nlcd_colors <- c("#5475a8","#ffffff","#e8d1d1","#e29e8c","#f00f00",
                 "#b50000","#d2cdc0","#85c77e","#38814e","#d4e7b0",
                 "#dcca8f","#e2e2c1","#fbf65d","#ca9146","#c8e6f8",
                 "#64b3d5")
# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv


# NLDC Categories
nlcd_cat <- c("water","snow","developed_op","developed_lw","developed_md",
              "developed_hg","barren","forest_dcd","forest_evg","forest_mxd",
              "shrub","grass","pasture","crops","wetland_wood",
              "wetland_herb")

# Assigning names to a color scale
names(nlcd_colors) <- nlcd_cat
#Source: https://statisticsglobe.com/r-assign-fixed-colors-to-categorical-variables-in-ggplot2-plot
################################################################################
# Import-export paths
################################################################################
# Local import
assets_data <- "../1-swf-knowledge.base/assets/data/raw" 

# Local export
assets_processed <- "../1-swf-knowledge.base/assets/data/processed"

# Figures
assets_figs <- "../1-swf-knowledge.base/assets/plots"

################################################################################
# Datasets
################################################################################

lnd_dat <- read_csv(paste(assets_data,"230321_pnw_2001_landcover.csv", sep = '/'),
                    show_col_types = FALSE)

lnd_dat <- lnd_dat %>% 
  mutate(basin = if_else(huc_4=="1703","Yakima","Willamette")) # run this step in the data processing script

summary(lnd_dat)

###############################################################################
# Analysis level 
###############################################################################

# It takes the values ("catchment" or "watershed")

analysis_level <- "watershed"

if (analysis_level== "catchment") {
  lnd_inf <- lnd_dat %>% 
    select(starts_with("cat")) %>% 
    select(-"cat_sink_area_km2")
  lnd_inf <- lnd_inf/rowSums(lnd_inf)
  if(sum(rowSums(lnd_inf))/nrow(lnd_inf)==1){
    print("data ready for analysis: all proportions per row adding to 1 ")
  } else{
    print("Verify! data NOT ready for analysis: NOT all proportions per row adding to 1 ")
  }
} else{
  lnd_inf <- lnd_dat %>% 
    select(starts_with("wsd")) 
  lnd_inf <- lnd_inf/rowSums(lnd_inf)
  if(sum(rowSums(lnd_inf))/nrow(lnd_inf)==1){
    print("data ready for analysis: all proportions per row adding to 1 ")
  } else{
    print("Verify! data NOT ready for analysis: NOT all proportions per row adding to 1 ")
  }
}

glimpse(lnd_inf)

################################################################################
# Information content analysis
################################################################################

# The NLCD 2001 includes 16 land use categories for both the catchment scale and 
# the watershed scale. It is common practice for statistical analysis to reduce
# the number of land use types to a more manageable quantity (5-6). The aggregation
# criteria may vary among researchers and it is biased towards land use categories
# with highest proportions. 

# Here, we use an information-theory derived criteria to identify not only the
# categories that contribute the most to the spatial heterogeneity across the 
# landscape but also data-driven criteria for aggregation of these categories for 
# modeling purposes. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# We will run this analysis across the entire data set, including the Willamette and 
# the Yakima data in the estimations.

################################################################################
#Estimating information content
################################################################################

colnames(lnd_inf) <- nlcd_cat

ncols <- ncol(lnd_inf)
nrows <- nrow(lnd_inf)
use <- as.data.frame(colnames(lnd_inf))
colnames(use) <- "land_use"

inf_mat<-matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yj","H","Hmax","I")))#Matrix with information content 
#content results

im0<-lnd_inf/sum(lnd_inf)  #Normalizing matrices

#Calculating information contributions
for (j in 1:ncols){
  yjn=sum(im0[,j])
  hn=entropy(im0[,j],unit="log2")
  hmaxn=log2(nrows)
  inf_mat[j,1]= yjn
  inf_mat[j,2]= hn
  inf_mat[j,3]= hmaxn
  inf_mat[j,4]=yjn%*%(hmaxn-hn)
}
inf_mat

inf_mat_p <- select(cbind(use,inf_mat),-Hmax)
colnames(inf_mat_p) <- c("nlcd","relative_abundance", "entropy", "information_content") 
inf_mat_p <-inf_mat_p[order(inf_mat_p$information_content),]

inf_cont_levels <- as.vector(inf_mat_p$nlcd)

inf_mat_g <- gather(inf_mat_p,key = "Information_metric", value = "Value", factor_key = TRUE, c(2:4))

p <-   ggplot(inf_mat_g,aes(x=factor(nlcd, levels = inf_cont_levels), 
                                 y = Value, fill = nlcd))+
  geom_bar(position="dodge",stat="identity") +
  ylab("Information metric")+
  xlab("Land use (NLDC-2001 (2019))")+
  scale_fill_manual(values = nlcd_colors)+
  facet_wrap(~Information_metric, ncol = 3, scales="free_x")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none",
        panel.grid.major.y = element_blank())+
  ggtitle(paste("Information contribution per land use category:",analysis_level,"scale",sep = " "))+
  coord_flip()
p

# geom bar required a workaround as explained by: 
# https://stackoverflow.com/questions/29525130/ggplot2-plotting-a-single-value-in-a-bar-graph

#################################################################################
#Estimating bootsrapped information content
#################################################################################

b_inf_mat= matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yjn","Hn","Hmaxn","In")))

list_yj=list()# in this list we will store the results from the 1000 iterations
list_hj=list()
list_ic=list()

for (i in 1:100){
  if (i==101){
    break
  }
  nmp=lnd_inf[sample(nrow(lnd_inf),size=nrows,replace=TRUE),]
  im=nmp/sum(nmp)  #Normalizing matrices
  #Calculating information contributions
  for (j in 1:ncols){
    yjn=sum(im[,j])
    hn=entropy(im[,j],unit="log2")
    hmaxn=log2(nrows)
    b_inf_mat[j,1]= yjn
    b_inf_mat[j,2]= hn
    b_inf_mat[j,3]= hmaxn
    b_inf_mat[j,4]=yjn%*%(hmaxn-hn)
  }
  
  list_ic[[i]]<-b_inf_mat[,4]
  
}

inf_dat=do.call("rbind", list_ic)# it combines all the lists in the same file
inf_dat=as.data.frame(inf_dat)
colnames(inf_dat) <- nlcd_cat

inf_dat_long <- inf_dat %>% 
  gather(key="land_use",
         value="information_contribution",
         factor_key = TRUE) %>% 
  group_by(land_use) %>% 
  mutate(inf_avg = mean(information_contribution),
         inf_sd = sd(information_contribution))


p <- ggplot(inf_dat_long,aes(x=reorder(land_use,inf_avg), y = inf_avg, fill = land_use))+
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=inf_avg-inf_sd, ymax=inf_avg+inf_sd), width=.2,
                position=position_dodge(.9)) +
  ylab("Information contribution (bootstrapped average)")+
  xlab("Land use (NLDC-2001 (2019))")+
  scale_fill_manual(values = nlcd_colors)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")+
  ggtitle(paste("Information contribution per land use category (bootstrapped):",analysis_level,"scale",sep = " "))+
  coord_flip()
p

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

lnd_dat <- lnd_dat %>% 
  mutate(c_forest_scp =cat_forest_evg + cat_forest_dcd + cat_forest_mxd,
         c_grass_scp = cat_grass,
         c_shrub_scp = cat_shrub,
         c_water_scp = cat_snow + cat_water + cat_wetland_wood + cat_wetland_herb,
         c_human_scp = cat_pasture + cat_crops + cat_developed_op + cat_developed_lw + cat_developed_md + cat_developed_hg,
         c_barren_scp = cat_barren,
         w_forest_scp =wsd_forest_evg + wsd_forest_dcd + wsd_forest_mxd,
         w_grass_scp = wsd_grass,
         w_shrub_scp = wsd_shrub,
         w_water_scp = wsd_snow + wsd_water + wsd_wetland_wood + wsd_wetland_herb,
         w_human_scp = wsd_pasture + wsd_crops + wsd_developed_op + wsd_developed_lw + wsd_developed_md + wsd_developed_hg,
         w_barren_scp = wsd_barren)

write.csv(lnd_dat,paste(assets_processed,"230324_inf_cont_lnd.csv",sep = '/'))
