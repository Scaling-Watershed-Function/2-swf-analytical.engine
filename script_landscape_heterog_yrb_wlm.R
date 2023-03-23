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

# We will run this analysis across the entire dataset, including the Willamette and 
# the Yakima data in the estimations.

# Let's start with local analysis

# Local data set

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
#April 29 2019 Loon Lake Information Content DataAnalysis (MPEq and rMPEq)
################################################################################
#Chapter 3 figures
################################################################################

###################################################
#LOADING LIBRARIES AND SETTING WORKING DIRECTORIES
###################################################

#Loading libraries

library(ggplot2)
library(reshape2)
library(lubridate)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(nlme)
library(doBy)
library(MASS)
library(lsmeans)
library(carData)
library(utils)
library(multcompView)
library(VIF)
library(cowplot)
library(scales)
library(imputeTS)
library(RColorBrewer)
library(tidyr)
library(gridExtra)
library(lattice)
library(grid)
library(gridBase)
library(entropy)
library(TSA)

#Setting up working directory

setwd("C:/Users/FranciscoJose/Documents/PhDOSU/ThesisDatAnalysis/loonlk/180811_inf_cont")


################################################################################
#Step 2.1  Information Content Analysis-Random sampling of dataset
################################################################################
#
#Once we have computed the information contribution of each frequency component 
#to overall entropy (or its reduction), we want to obtain the standard deviations
#for the information content for each grain size by randomly sampling the matrix
#and calculating the information contribution of each grain size in each iteration.
#Then, we calculate the mean information contribution from all grain sizes as well as the
#confidence intervals for such contributions
#######################################################


#Dataset with grain size information

lgs<-read.csv("180811_llake_g_dat.csv")#A matrix with grain size percentages from laser difractometry (l)ake(g)rain(s)ize
dlk<-read.csv("190428_diameters_llk.csv")#Vector with particle size classes (otherwise imported as factors)

#Particle size histograms

lgsh<-t(lgs)

h1<-as.data.frame(lgsh[2:83,10])
colnames(h1)<-c("percent")
h1<-cbind(dlk,h1)

p<-ggplot(h1,aes(diameter,percent, fill= percent))+
  geom_area(alpha=0.5)+
  geom_line()+
  scale_x_log10()
p


lgs<-dplyr::select(lgs,-z)#Exlcuding column with depth values


#Setting parameters for calculations
nrows=815 #Number of depth samples taken in each iteration 
ncols=82 #Number of grain size components

#Estimating original values for information content
ic0<-matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yj","H","Hmax","I")))#Matrix with information content 
#content results

im0<-lgs/sum(lgs)  #Normalizing matrices

#Calculating information contributions
for (j in 1:ncols){
  yjn=sum(im0[,j])
  hn=entropy(im0[,j],unit="log2")
  hmaxn=log2(nrows)
  ic0[j,1]= yjn
  ic0[j,2]= hn
  ic0[j,3]= hmaxn
  ic0[j,4]=yjn%*%(hmaxn-hn)
}
ic0
ic0<-as.data.frame(ic0)
ic0<-cbind(dlk,ic0)

#write.csv(ic0,"190428_InformationContent_llk.csv")

#Estimating bootsrapped information content

ic= matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yjn","Hn","Hmaxn","In")))

list_yj=list()# in this list we will store the results from the 10000 iterations
list_hj=list()
list_ic=list()

for (i in 1:10000){
  if (i==10001){
    break
  }
  nmp=lgs[sample(nrow(lgs),size=nrows,replace=TRUE),]
  im=nmp/sum(nmp)  #Normalizing matrices
  #Calculating information contributions
  for (j in 1:ncols){
    yjn=sum(im[,j])
    hn=entropy(im[,j],unit="log2")
    hmaxn=log2(nrows)
    ic[j,1]= yjn
    ic[j,2]= hn
    ic[j,3]= hmaxn
    ic[j,4]=yjn%*%(hmaxn-hn)
  }
  
   list_yj[[i]]<-ic[,1]
   list_hj[[i]]<-ic[,2]
   list_ic[[i]]<-ic[,4]
  
}
m_yj=do.call("rbind", list_yj)# it combines all the lists in the same file
m_yj=as.data.frame(m_yj)#it converts the joined list into a dataframe that can be manipulated with R functions
m_yj1=t(sapply(m_yj, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE))))# produces a string of data with means and s.d.
Yj_mean=as.numeric(m_yj1[,1]); Yj_sd=as.numeric(m_yj1[,2])#it transforms the previous string into numeric data

m_hj=do.call("rbind", list_hj)
m_hj=as.data.frame(m_hj)
m_hj1=t(sapply(m_hj, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE))))
Hj_mean=as.numeric(m_hj1[,1]); Hj_sd=as.numeric(m_hj1[,2])

m_ic=do.call("rbind", list_ic)
m_ic=as.data.frame((m_ic))
m_ic1=t(sapply(m_ic, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE))))
Ic_mean=as.numeric(m_ic1[,1]); Ic_sd=as.numeric(m_ic1[,2])

df=as.data.frame(cbind(Yj_mean,Yj_sd,Hj_mean,Hj_sd,Ic_mean,Ic_sd))#it combines the vectors of means and s.d.s

df<-cbind(dlk,df)


#Plotting histograms for Yj, entropy, and information content
ma<-m_yj[,21:40]
mb<-m_hj[,41:61]
mc<-m_ic[,62:82]

ma<-gather(ma,"class","yj")
mb<-gather(mb,"class","hj")
mc<-gather(mc,"class","ic")


p<-ggplot(ma,aes(yj))+
  geom_histogram()+
  facet_wrap(~as.factor(class),scale="free")
p

#Quantile-quantile plots
p1<-ggplot(ma,aes(sample=yj))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~as.factor(class),scale="free")
p1

#These plots showed that we could assume normality for the sampling distributions

#Calculating normal bootstrap confidence interval with a correction for bias

a<-0.95
z<-qnorm(a)
Ic0<-ic0$I
df<-cbind(Ic0,df)

df$up_ci<-(2*Ic0-Ic_mean)+(z*Ic_sd)
df$lw_ci<-(2*Ic0-Ic_mean)-(z*Ic_sd)

write.csv(df,"190428_inf_stats_llk_10k_bootst.csv")

#Plotting results


#Preparing datasets for plotting
df1<-dplyr::select(df,diameter,Yj_mean,Ic_mean,up_ci,lw_ci)
df1$Yj_mean<-rescale(Yj_mean,to= c(0, 1),from = range(Yj_mean, na.rm = TRUE, finite = TRUE))

rd<-dplyr::select(df, diameter, Ic_mean,up_ci,lw_ci)
rd1<-gather(rd,"metric","val.",c(2:4),factor_key = TRUE)
rd1$val.<-rescale(rd1$val.,to= c(0, 1),from = range(rd1$val, na.rm = TRUE, finite = TRUE))
ic1<-spread(rd1,"metric","val.")

#Rescaling datastets
rdf1<-cbind(df1$diameter,df1$Yj_mean,ic1$Ic_mean,ic1$up_ci,ic1$lw_ci)
colnames(rdf1)<-c("Diameter","Yj","Ic","Up_ci","Lw_ci")
rdf1<-as.data.frame(rdf1)

#Extracting rows with highest information content within each group
rdf2<-rdf1[c(15,23,37,58,67,78),]

#Plot
p<-ggplot(rdf1,aes(Diameter,Yj))+
  geom_area(alpha=0.3,fill="orange")+
  geom_ribbon(aes(ymin=Lw_ci,ymax=Up_ci),fill="gray70",alpha=0.35)+
  geom_line(aes(Diameter,Ic))+
  geom_point(aes(Diameter,Ic))+
  geom_linerange(data=rdf2,aes(x=Diameter, ymax=Ic, ymin=0),linetype="dashed")+
  geom_point(data=rdf2,aes(Diameter,Ic,color=Ic),size=4.0)+
  scale_x_log10(expand=c(0,0),position="bottom",breaks=c(0.001,0.01,1,10,100,1000))+
  theme(axis.text=element_text(colour="black",size=14),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
        panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        panel.background=element_rect(fill="snow"),
        axis.ticks.length = unit(0.254, "cm"),
        axis.ticks = element_line(colour = "black", size=1), 
        axis.line = element_line(colour = "black"),
        legend.position = c(.05,.90),
        legend.direction = "vertical",
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        plot.margin = unit(c(1,0.3,1,0.3), "cm"))
p

p<-p+geom_vline(xintercept = 1.0, size=0.8,linetype="dotted",colour="black")+
  geom_vline(xintercept = 15.1,size=0.8,linetype="dotted",colour="black")+
  geom_rect(xmin = log(0.45), ymin = -Inf, xmax = log(0.5), ymax = +Inf, fill = "#8c510a",alpha=0.01,color=NA)+
  geom_rect(xmin = log(0.78), ymin = -Inf, xmax = log(0.87), ymax = +Inf, fill = "#d8d365",alpha=0.01,color=NA)+
  geom_rect(xmin = log(1.8), ymin = -Inf, xmax = log(2), ymax = +Inf, fill = "#f6e8c3",alpha=0.01,color=NA)+
  geom_rect(xmin = log(6.3), ymin = -Inf, xmax = log(7), ymax = +Inf, fill = "#01665e",alpha=0.01,color=NA)+
  geom_rect(xmin = log(11), ymin = -Inf, xmax = log(12.2), ymax = +Inf, fill = "#5ab4ac",alpha=0.01,color=NA)+
  geom_rect(xmin = log(21.2), ymin = -Inf, xmax = log(23.2), ymax = +Inf, fill = "#c7eae5",alpha=0.01,color=NA)+
  geom_ribbon(data=filter(inf_c,sple=="obs"),aes(x=s_um, ymin=mn, ymax=mx), fill="#feebe2", alpha=.2,inherit.aes = FALSE)
p











out_wlm_cat


lnd_wlm_cat <- lnd_dat %>% 
  filter(basin == "Willamette") %>% 
  select(starts_with("wsd")) 
  
  
  lnd_wlm_cat <- lnd_wlm %>% 
  select(starts_with("cat")) %>% 
  lnd_wlm_wsd <- lnd_wlm %>% 
  select(starts_with("wsd"))
  

lnd_wlm_cat <- lnd_wlm %>% 
  select





















# Tentative color palette for land uses (to be changed using colors corresponding
# to the national database)

my_colors <- c("#F564E3","#00BA38","#B79F00","#F8766D","#619CFF")

#Data:
dat_o <- read.csv("assets/data/processed/230202_yrb_wlm_resp_dat.csv",stringsAsFactors = TRUE)
lnd_o <- read.csv("assets/data/processed/230202_yrb_wlm_land_filtered_dat.csv",stringsAsFactors=TRUE)

#Let's first create a working data set, which right now is just
#a copy of the original lnd:

dat <- as_tibble(dat_o)
lnd <- as_tibble(lnd_o)

################################################################################
# Entropy analysis
################################################################################

# Let's start with a simple calculation of the Shannon's entropy as a proxy for 
# land use heterogeneity

# Making row-wise operations (https://dplyr.tidyverse.org/articles/rowwise.html)
lnd <- lnd %>% rowwise() %>% 
  mutate(hl = entropy(c(agrc,
                        frst,
                        shrb,
                        urbn,
                        wtnd),unit = "log")) %>% 
  mutate(hrl = hl/log(5)) %>% 
  mutate(ht = entropy(c(agrc_t,
                        frst_t,
                        shrb_t,
                        urbn_t,
                        wtnd_t),unit = "log")) %>% 
  mutate(hrt = ht/log(5)) 

# Let's compare basins in terms of their landscape heterogeneity:

lnd %>% select(basin,hrl,hrt) %>% 
  rename(Basin = basin,
         Local = hrl,
         Total = hrt) %>% 
  gather(key = "Extent",value = "Entropy",c(2:3)) %>% 
  ggplot(aes(Basin,Entropy,color = Basin))+
  geom_boxplot(alpha = 0.5)+
  ylab("Relative Shannon's Entropy")+
  facet_wrap(~Extent,ncol = 2)
  
# Despite the striking differences in landscape configuration, we do not observe
# a commensurate difference in landscape entropies.

# Information content analysis

# Using Shannon's entropy calculations, we can identify which land use types 
# either locally or at the watershed scale contribute with most of the information
# about spatial variability. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# Let's start with local analysis

# Willamette River Basin

# Local data set
lnd_el <- filter(lnd,basin == "Willamette") %>% #Notice the specification of the watershed of interest
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1000
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=ssz,replace = FALSE),]
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(iml))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  sr_list[[i]] <- ic_loc[3,]
  ub_list[[i]] <- ic_loc[4,]
  wt_list[[i]] <- ic_loc[5,]
  
}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

wlm_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "wlm" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(wlm_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Willamette River Basin (Local Drainage Area)")
p5

# Yakima River Basin

# Local data set
lnd_el <- filter(lnd,basin == "Yakima") %>% #Notice the specification of the watershed of interest
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1000
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 1000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=ssz,replace = FALSE),]
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(iml))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  sr_list[[i]] <- ic_loc[3,]
  ub_list[[i]] <- ic_loc[4,]
  wt_list[[i]] <- ic_loc[5,]
  
}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

yrb_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "yrb" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(yrb_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Yakima River Basin (Local Drainage Area)")
p5

# Overall (Local Information Contribution)

# Local data set
lnd_el <- lnd %>% #Notice that no watershed is specified in the analysis
  select(agrc,
         frst,
         shrb,
         urbn,
         wtnd)

# Creating a matrix for results

ncols = 4
nrows = 5
ssz = 1400
ic_loc <- matrix(1:nrows,nrows,ncols, 
                 dimnames = list(c("Agriculture","Forests","Shrublands","Urban","Wetlands"),
                                 c("Yjn_l","Hn_l","Hmaxn_l", "In_l")))

ag_list <- list()
fr_list <- list()
sr_list <- list()
ub_list <- list()
wt_list <- list()

# Number of iterations 
itn = 5000

for(i in 1:itn){
  if (i == itn +1){
    break
  }
  loc_im <- lnd_el[sample(nrow(lnd_el),size=ssz,replace = FALSE),]
  iml <- loc_im[,c(1:ncol(loc_im))]/sum(loc_im[,c(1:ncol(loc_im))])
  for(j in 1:ncol(iml)){
    yjn = sum(iml[,j])
    hn = entropy(iml[,j], unit = "log")
    hmaxn = log(nrow(iml))
    ic_loc[j,1]=yjn
    ic_loc[j,2]=hn
    ic_loc[j,3]=hmaxn
    ic_loc[j,4]=yjn%*%(hmaxn-hn)
  }
  ag_list[[i]] <- ic_loc[1,]
  fr_list[[i]] <- ic_loc[2,]
  sr_list[[i]] <- ic_loc[3,]
  ub_list[[i]] <- ic_loc[4,]
  wt_list[[i]] <- ic_loc[5,]
  
}
ag_l = as_tibble(do.call("rbind",ag_list))
ag_l <- ag_l %>% mutate(use="Agriculture")
fr_l = as_tibble(do.call("rbind",fr_list))
fr_l <- fr_l %>% mutate(use = "Forests")
sr_l = as_tibble(do.call("rbind",sr_list))
sr_l <- sr_l %>% mutate(use = "Shurblands")
ub_l = as_tibble(do.call("rbind",ub_list))
ub_l <- ub_l %>% mutate(use = "Urban")
wt_l = as_tibble(do.call("rbind",wt_list))
wt_l <- wt_l %>% mutate(use = "Wetlands")

ywb_local_im <- rbind(ag_l,fr_l,sr_l,ub_l,wt_l) # Notice the "ywb" prefix in the name
# of the output file

# Let's check the results with a box-plot 

p5 <- ggplot(ywb_local_im,aes(x = reorder(use,-In_l), y = In_l, fill = use, color = use))+
  geom_boxplot(alpha = 0.5)+
  scale_color_manual(values = my_colors)+
  scale_fill_manual(values = my_colors)+
  labs(x="Land use",y ="Information Contribution")+
  theme(legend.position = "none")+
  ggtitle("Information contribution across River Basins (Local Drainage Area)")
p5
