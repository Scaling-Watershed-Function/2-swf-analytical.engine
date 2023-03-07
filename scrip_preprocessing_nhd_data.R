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
raw_data <- "https://raw.githubusercontent.com/Scaling-Watershed-Function/1-swf-knowledge.base/main/assets/data/raw/"

# Loading NHD data

# Local import
assets_data <- "..\1-swf-knowledge.base\assets\data\raw\shapes"

require(sf)
nhd_yrb_stream <- st_read(assets_data,"nhd_CR_stream_sub8.shp")
