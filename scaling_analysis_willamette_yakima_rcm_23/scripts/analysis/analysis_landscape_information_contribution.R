###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LAND-USE LAND-COVER INFORMATION CONTENT ANALYSIS
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


# NLDC Legend Colors
# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv

# Open water	#5475a8
# Perenn. ice/snow	#ffffff
# Open space	#e8d1d1
# Low intens. dev.	#e29e8c
# Med. intens. dev.	#f00f00
# High intens. dev.	#b50000
# Barren	#d2cdc0
# Deciduous forest	#85c77e
# Evergreen forest	#38814e
# Mixed forest	#d4e7b0
# Shrub/scrub	#dcca8f
# Grassland	#e2e2c1
# Pasture/hay	#fbf65d
# Cultivated crops	#ca9146
# Woody wetland	#c8e6f8
# Other wetland	#64b3d5

# NLDC Colors

nlcd_colors <- c("#5475a8","#ffffff","#e8d1d1","#e29e8c","#f00f00",
                 "#b50000","#d2cdc0","#85c77e","#38814e","#d4e7b0",
                 "#dcca8f","#e2e2c1","#fbf65d","#ca9146","#c8e6f8",
                 "#64b3d5")

nlcd_cat <- c("water",
              "snow",
              "developed_op",
              "developed_lw",
              "developed_md",
              "developed_hg",
              "barren",
              "forest_dcd",
              "forest_evg",
              "forest_mxd",
              "shrub",
              "grass",
              "pasture",
              "crops",
              "wetland_wood",
              "wetland_herb")

# Data

# GitHub import
ctch_lnd_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/raw_data/nlcd_2001_v2019/data/nlcd_2001_v2019_NLCD01_CAT_CONUS.csv",
                         show_col_types = FALSE)

wshd_lnd_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/raw_data/nlcd_2001_v2019/data/nlcd_2001_v2019_NLCD01_TOT_CONUS.csv",
                         show_col_types = FALSE)

rcm_23_model_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/raw_data/rcm_2023_model_data/data/rcm_23_model_output_data.csv",
                             show_col_types = FALSE)

# Local data saving
local_data <- "./data" 

#  Local figure export
results <- "./results" #For svg files that can be explored online

results_png <- "/Users/guerrero-fj/Library/Mobile Documents/com~apple~CloudDocs/scaling_watershed_function/analytical_engine/scaling_analysis_willamette_yakima_23/results"


# Pre-processing data inputs

# Catchment data

colnames(ctch_lnd_dat) <- gsub("CAT_","",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_11","water",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_12","snow",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_21","developed_op",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_22","developed_lw",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_23","developed_md",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_24","developed_hg",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_31","barren",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_41","forest_dcd",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_42","forest_evg",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_43","forest_mxd",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_52","shrub",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_71","grass",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_81","pasture",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_82","crops",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_90","wetland_wood",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_95","wetland_herb",colnames(ctch_lnd_dat))

ctch_lnd_dat$level <- "catchment"

# Watershed data

colnames(wshd_lnd_dat) <- gsub("TOT_","",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_11","water",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_12","snow",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_21","developed_op",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_22","developed_lw",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_23","developed_md",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_24","developed_hg",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_31","barren",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_41","forest_dcd",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_42","forest_evg",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_43","forest_mxd",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_52","shrub",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_71","grass",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_81","pasture",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_82","crops",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_90","wetland_wood",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_95","wetland_herb",colnames(wshd_lnd_dat))

wshd_lnd_dat$level <- "watershed"

# Merging datasets with model data

wshd_lnd_dat <- wshd_lnd_dat %>% 
  merge(.,
        rcm_23_model_dat %>% 
          select(comid,
                 basin),
        by = "comid",
        all.y = TRUE) %>% 
  select(-NODATA)


ctch_lnd_dat <- ctch_lnd_dat %>% 
  merge(.,
        rcm_23_model_dat %>% 
          select(comid,
                 basin),
        by = "comid",
        all.y = TRUE) %>% 
  select(-NODATA)



# Dataset for analysis

swf_land_dat <- rbind(ctch_lnd_dat,
                      wshd_lnd_dat) 

summary(swf_land_dat)

write.csv(swf_land_dat,paste(local_data,"231008_land_use_cover_pnw.csv",sep = '/'),
          row.names = FALSE)

################################################################################
# Information content analysis
################################################################################

# The NLCD 2001 includes 16 land use categories for both the catchment scale and 
# the watershed scale. It is common practice for statistical analysis to reduce
# the number of land use types to a more manageable quantity (5-6). The aggregation
# criteria may vary among researchers and it is not necessarily guided by data. 

# Here, we use an information-theory derived criteria to identify not only the
# categories that contribute the most to the spatial heterogeneity across the 
# landscape but also data-driven criteria for aggregation of these categories for 
# modeling purposes. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# We will run this analysis across the entire data set, including the Willamette and 
# the Yakima data in the estimations.

# You can select the level of analysis you want to start with: 

###############################################################################
# Analysis level 
###############################################################################

# It takes the values ("catchment" or "watershed")

analysis_level <- "watershed"

if (analysis_level== "catchment") {
  lnd_inf <- swf_land_dat %>% 
    filter(level == "catchment")%>% 
    select(c(3:18))
} else {
  lnd_inf <- swf_land_dat %>% 
    filter(level == "watershed")%>% 
    select(c(3:18))
}

################################################################################
#Estimating original values for information content
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

inf_mat <- cbind(use,inf_mat)

inf_mat$land_use <- factor(inf_mat$land_use,levels = nlcd_cat)

p <- ggplot(inf_mat,aes(x=reorder(land_use,I), y = I, fill = land_use))+
  geom_bar(position="dodge",stat="identity") +
  ylab("Information contribution")+
  xlab("Land use (NLDC-2001 (2019))")+
  scale_fill_manual(values = nlcd_colors)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste("Information contribution per land use category:",analysis_level,"scale",sep = " "))+
  theme_minimal()+
  coord_flip()
p

# geom bar required a workaround as explained by: 
# https://stackoverflow.com/questions/29525130/ggplot2-plotting-a-single-value-in-a-bar-graph

#Estimating bootsrapped information content

b_inf_mat= matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yjn","Hn","Hmaxn","In")))

list_yj=list()# in this list we will store the results from the 1000 iterations
list_hj=list()
list_ic=list()

for (i in 1:1000){
  if (i==1001){
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
  scale_fill_manual(values = nlcd_colors, name = "Land Use")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste("Information contribution per land use category (bootstrapped):",analysis_level,"scale",sep = " "))+
  theme_minimal()+
  coord_flip()
p

plot_name <- paste0("guerrero_etal_23_inf_contrib_",analysis_level,".jpg")

ggsave(paste(results_png,plot_name,sep = '/'),
       width = 12,
       height = 10,
       units = "in")

