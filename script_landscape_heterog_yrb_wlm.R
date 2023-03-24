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

# Local import
assets_data <- "../1-swf-knowledge.base/assets/data/raw" 

# Local export
assets_processed <- "../1-swf-knowledge.base/assets/data/processed"


lnd_dat <- read_csv(paste(assets_data,"230321_pnw_2001_landcover.csv", sep = '/'),
                    show_col_types = FALSE)

lnd_dat <- lnd_dat %>% 
  mutate(basin = if_else(huc_4=="1703","Yakima","Willamette"))

summary(lnd_dat)

###############################################################################
# Deriving data sets for both catchment and watershed scale
###############################################################################

# Catchment data set

lnd_pnw_cat0 <- lnd_dat %>% 
  select(starts_with("cat")) %>% 
  select(-"cat_sink_area_km2") %>% 
  mutate(total = rowSums(across(where(is.numeric))))

summary(lnd_pnw_cat0)

# We expect all the rows to sum up to 100%. We found a few observations that go 
# a bit above or below that value. To make sure everything adds to 1 (fraction) 
# we recalculate land use fractions by dividing by the total row sums

lnd_pnw_cat <- lnd_pnw_cat0[-17]/rowSums(lnd_pnw_cat0[-17])

# and quickly verify by using:

summary(rowSums(lnd_pnw_cat))


# Watershed dataset

# We will proceed similarly with the watershed dataset:

lnd_pnw_wsd0 <- lnd_dat %>% 
  select(starts_with("wsd")) %>% 
  mutate(total = rowSums(across(where(is.numeric))))

summary(lnd_pnw_wsd0)

# We have several observations that are above or below 100 (or 1), so we recalculate
# fractions:

lnd_pnw_wsd <- lnd_pnw_wsd0[-17]/rowSums(lnd_pnw_wsd0[-17])

# and quickly verify by using:

summary(rowSums(lnd_pnw_wsd))


################################################################################
# Information content analysis
################################################################################

# The NLCD 2001 includes 16 land use wsdegories for both the wsdchment scale and 
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

analysis_level <- "catchment"

if (analysis_level== "catchment") {
  lnd_inf <- lnd_pnw_cat
} else {
  lnd_inf <- lnd_pnw_wsd
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
  scale_fill_manual(values = nlcd_colors)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste("Information contribution per land use category (bootstrapped):",analysis_level,"scale",sep = " "))+
  theme_minimal()+
  coord_flip()
p


