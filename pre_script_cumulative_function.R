###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# CUMULATIVE CALCULATIONS
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md
gc()

rm()

librarian::shelf(tidyverse)# To manipulate ggplot objects

#Data:

# Local import and export paths

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"
assets_figs <- "../1-swf-knowledge.base/assets/plots"

#header info (data dictionary)

heading_dat <- read_csv(paste(processed_data,"guerrero_etal_swf_dd.csv", sep = '/'),
                        show_col_types = FALSE)

#values
bgc_dat_c8 <- read_csv(paste(processed_data,"230505_bgc_dat_c7_entropy.csv", sep = "/"),
                       show_col_types = FALSE) %>% 
  mutate(totco2g_day = 10^logtotco2g_m2_day*stream_area_m2)


################# Finding upstream reaches ########################################3
findSrc <- function(endNode, ret = NULL) {
  w <- which(to_node %in% endNode)
  
  if (length(w) == 0) return(c(ret, endNode))
  else {
    return(findSrc(from_node[w], c(ret, endNode)))
  }
}


######### YAKIMA RIVER BASIN ############################################
yrb_dat <- filter(bgc_dat_c8, basin == "yakima")

to_node<-yrb_dat$to_node
from_node<-yrb_dat$from_node

tmp_comid_resp<-matrix(0,ncol=2,nrow=nrow(yrb_dat))

for (i in 1:nrow(yrb_dat)){
  j_id<-findSrc(yrb_dat$from_node[i],yrb_dat$to_node[i])
  ## select the downstream reach
  tmp<-j_id[1]
  
  tmp_comid<-yrb_dat[yrb_dat$from_node==j_id[1],]
  tmp_comid<-tmp_comid$comid
  
  if(length(j_id)==1){
    ## no upstream
    tmp1_resp<-yrb_dat[yrb_dat$from_node==j_id[1],]
  }
  
  else {
    
    tmp1_resp= subset(yrb_dat, from_node %in% c(j_id))
  }
  
  tmp_comid_resp[i,]<-cbind(tmp_comid[1],sum(tmp1_resp$totco2g_day))
  
  print(i)
  
}

yrb_acc_rsp <- as_tibble(unlist(tmp_comid_resp)) %>% 
  rename(comid = V1,
         acc_totco2g_day = V2) %>% 
  distinct(comid, .keep_all = TRUE) 

yrb_acc_dat <- merge(yrb_dat,
                     yrb_acc_rsp,
                     by = "comid",
                     all.x = TRUE)



yrb_acc_dat_t <- yrb_acc_dat %>% 
  group_by(stream_order) %>% 
  mutate(acc_totco2g_day = if_else(is.na(acc_totco2g_day) & stream_order == 1,
                                   totco2g_day,
                                   if_else(is.na(acc_totco2g_day) & stream_order>1,
                                           median(acc_totco2g_day, na.rm = TRUE),
                                           acc_totco2g_day)),
         acc_totco2g_km2_day = acc_totco2g_day/wshd_area_km2,
         acc_totco2g_ntw_day = acc_totco2g_day*wshd_stream_dens)



#################################### Willamette River ##########################

wrb_dat <- filter(bgc_dat_c8, basin == "willamette")

to_node<-wrb_dat$to_node
from_node<-wrb_dat$from_node

tmp_comid_resp<-matrix(0,ncol=2,nrow=nrow(wrb_dat))

for (i in 1:nrow(wrb_dat)){
  j_id<-findSrc(wrb_dat$from_node[i],wrb_dat$to_node[i])
  ## select the downstream reach
  tmp<-j_id[1]
  
  tmp_comid<-wrb_dat[wrb_dat$from_node==j_id[1],]
  tmp_comid<-tmp_comid$comid
  
  if(length(j_id)==1){
    ## no upstream
    tmp1_resp<-wrb_dat[wrb_dat$from_node==j_id[1],]
  }
  
  else {
    
    tmp1_resp= subset(wrb_dat, from_node %in% c(j_id))
  }
  
  tmp_comid_resp[i,]<-cbind(tmp_comid[1],sum(tmp1_resp$totco2g_day))
  
  print(i)
  
}

wrb_acc_rsp <- as_tibble(unlist(tmp_comid_resp)) %>% 
  rename(comid = V1,
         acc_totco2g_day = V2) %>% 
  distinct(comid, .keep_all = TRUE) 

wrb_acc_dat <- merge(wrb_dat,
                     wrb_acc_rsp,
                     by = "comid",
                     all.x = TRUE)

summary(filter(wrb_acc_dat, is.na(acc_totco2g_day)==TRUE))

# Mostly first order streams with no upstream connections result in NA values 
# the cumulative estimation needs to be updated


wrb_acc_dat_t <- wrb_acc_dat %>% 
  group_by(stream_order) %>% 
  mutate(acc_totco2g_day = if_else(is.na(acc_totco2g_day) & stream_order == 1,
                                   totco2g_day,
                                   if_else(is.na(acc_totco2g_day) & stream_order>1,
                                           median(acc_totco2g_day, na.rm = TRUE),
                                           acc_totco2g_day)),
         acc_totco2g_km2_day = acc_totco2g_day/wshd_area_km2,
         acc_totco2g_ntw_day = acc_totco2g_day*wshd_stream_dens)


bgc_dat_c10 <- rbind(yrb_acc_dat_t,
                     wrb_acc_dat_t)

write.csv(bgc_dat_c10,paste(processed_data,"230507_bgc_dat_c10.csv", sep = '/'),
          row.names = FALSE)

################################## IN PROGRESS ##################################

# define function to find source nodes
findSrc <- function(endNode, ret = NULL) {
  w <- which(to_node %in% endNode)
  
  if (length(w) == 0) return(c(ret, endNode))
  else {
    return(findSrc(from_node[w], c(ret, endNode)))
  }
}

# define function to process data for a single basin
process_basin <- function(basin) {
  bgc_dat_c8 <- read_csv(paste(processed_data,"230505_bgc_dat_c7_entropy.csv", sep = "/"),
                         show_col_types = FALSE) %>% 
    mutate(totco2g_day = 10^logtotco2g_m2_day*stream_area_m2)
  
  
  to_node <- bgc_dat_c8$to_node
  from_node <- bgc_dat_c8$from_node
  
  tmp_comid_resp <- matrix(0, ncol = 2, nrow = nrow(bgc_dat_c8))
  
  for (i in 1:nrow(bgc_dat_c8)) {
    j_id <- findSrc(bgc_dat_c8$from_node[i], bgc_dat_c8$to_node[i])
    tmp_comid <- bgc_dat_c8[bgc_dat_c8$from_node == j_id[1], "comid"]
    
    if (length(j_id) == 1) {
      tmp1_resp <- bgc_dat_c8[bgc_dat_c8$from_node == j_id[1], "totco2g_day"]
    } else {
      tmp1_resp <- bgc_dat_c8[from_node %in% j_id, "totco2g_day"] %>% sum()
    }
    
    tmp_comid_resp[i, ] <- c(tmp_comid[1], tmp1_resp)
  }
  
  acc_rsp <- as_tibble(tmp_comid_resp) %>% 
    rename(comid = V1, acc_totco2g_day = V2) %>% 
    distinct(comid, .keep_all = TRUE)
  
  acc_dat <- merge(bgc_dat_c8, acc_rsp, by = "comid", all.x = TRUE) %>% 
    group_by(stream_order) %>% 
    mutate(acc_totco2g_day = if_else(
      is.na(acc_totco2g_day) & stream_order == 1,
      totco2g_day,
      if_else(
        is.na(acc_totco2g_day) & stream_order > 1,
        median(acc_totco2g_day, na.rm = TRUE),
        acc_totco2g_day
      )
    ), 
    acc_totco2g_km2_day = acc_totco2g_day / wshd_area_km2,
    acc_totco2g_ntw_day = acc_totco2g_day * wshd_stream_dens)
  
  return(acc_dat)
}

# define vector of basin names
basins <- c("yakima", "willamette")

# process data for each basin
all_acc_dat <- lapply(basins, process_basin) %>% bind_rows()































