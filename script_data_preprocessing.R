###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PRE-PROCESSING
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero (Modified from Kyongho Son)
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

librarian::shelf(sp,sf,raster,rgdal,rasterVis,
                 rgeos,lattice,grid,spatstat,
                 plotKML,fasterize,egg,nhdplusTools,
                 nhdR,colorspace,stars,pals,foreign,
                 tidyverse,readr)
#Note: Verify that the sf package was sucessfully installed. Otherwise try install.packages("sf)
#and answer "no" to the following promt:

#Do you want to install from sources the package which needs compilation? (Yes/no/cancel) 

set.seed(2703)

#########################################################################################################
# Import / Export of assets

# Import: Repository path to raw data
raw_data <- "https://raw.githubusercontent.com/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/shapes/"

# Loading NHD data

# Local import
assets_data <- "../1-swf-knowledge.base/assets/data/raw/pre-processing/model_inputs" 

############ loading NHD Data----
##model_inputs: model input folder

# Willamette
require(sf)
nhd_CR_stream<-st_read(paste(assets_data,"shapes/nhd_CR_stream_sub8.shp",sep = "/"))
tmp<-st_zm(nhd_CR_stream)
nhd_CR_poly<-tmp[,"COMID"]

## reading model inputs: substrate concentrations
require(readr)
stream_annDO<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DO.csv",sep="/"),show_col_types = FALSE)
stream_annno3<-read.csv(paste(assets_data,"nhd_CR_stream_no3.csv",sep="/"),header=T,sep=',',skip=0)
stream_annDOC<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DOC.csv",sep="/"),show_col_types = FALSE)
stream_nexss<-read_csv(paste(assets_data,"nexss_inputs.csv",sep="/"),show_col_types = FALSE)


## merging the model input data with NHDPLUS stream reach shapefiles
nhd_CR_stream_resp=merge(nhd_CR_poly,stream_annDO,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annDOC,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_annno3,by="COMID")
nhd_CR_stream_resp=merge(nhd_CR_stream_resp,stream_nexss,by.x="COMID",by.y="comid_nhd")

# 
# figures<-"ESS-DIVE/figures"

jpeg("Stream DOC_CRB_annual_DOC.jpeg", width = 6, height = 6, units = 'in', res = 300) 
par(cex.main=1.5,cex.axis=1.5) 
plot(nhd_CR_stream_resp[,"Stream DOC"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.reds(10),reset=FALSE)
# plot(st_geometry(nhd_CR_stream_resp),add=T)

title("(a) Stream DOC (mg/l)",line=-24, adj = 0.2)


dev.off()
