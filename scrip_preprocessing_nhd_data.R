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
                 tidyverse)
set.seed(2703)

#########################################################################################################
# Import / Export of assets

# Import: Repository path to raw data
raw_data <- "https://raw.githubusercontent.com/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/shapes/"

# Loading NHD data

# Local import
# assets_data <- "../2-swf-analytical.engine/shapes" It does not work


require(sf)
# nhd_yrb_stream <- st_read(assets_data,"nhd_CR_stream_sub8.shp") it does not work

# Apparently files have to be rigth outside in the working directory

nhd_yrb_stream <- st_read("nhd_CR_stream_sub8.shp")

tmp<-st_zm(nhd_yrb_stream)
nhd_yrb_poly<-tmp[,"COMID"]
nhd_yrb_stream<-data.frame(nhd_yrb_poly)
nhd_yrb_stream$COMID<-NULL                          


