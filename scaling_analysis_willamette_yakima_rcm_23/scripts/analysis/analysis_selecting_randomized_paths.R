################################################################################
# SCALING WATERSHED FUNCTION: Randomized Paths for Allometric Constraint Analysis
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign,
                 data.table)

# Local Import-Export
shapes_data <- "../../1-swf-knowledge.base/datasets/raw_data/nsi_ssn_network/data"
local_data <- "./data"
source("./source/function_randomized_paths_allometry.R")

pnw_rivers_dat <- st_transform(st_read(paste(shapes_data,
                                             "nsi_network_ywrb.shp",sep = "/")),4326)

rcm_23_model_output_dat <- read_csv(paste(local_data,"rcm_23_model_output_data.csv", sep = '/'),
                                    show_col_types = FALSE)


# Let's take a look at the data

summary(rcm_23_model_output_dat)

#Let's also create a dataset that allows for mapping some of these variables:

pnw_rivers_map <- pnw_rivers_dat %>% 
  select(COMID) %>% 
  rename(comid = COMID) %>% 
  merge(.,
        rcm_23_model_output_dat,
        by = "comid", 
        all.x = TRUE)

# Stream orders rank from 1 - 7, let's take a look at the map to identify reaches
# with the maximum order

leaflet(pnw_rivers_map) %>% 
  addPolylines(weight = 2) %>%
  addPolylines(data = filter(pnw_rivers_map,stream_order==7),
               color = "#EDEF5C",
               opacity = 1,
               weight = 7) %>%
  addPolylines(data = filter(pnw_rivers_map,stream_order==6),
               color = "#BEE05E",
               opacity = 1,
               weight = 6) %>%
  addPolylines(data = filter(pnw_rivers_map,stream_order==5),
               color = "#78C86F",
               opacity = 1,
               weight = 5) %>% 
  addPolylines(data = filter(pnw_rivers_map,stream_order==4),
               color = "#08A47F",
               opacity = 1,
               weight = 4) %>% 
  addPolylines(data = filter(pnw_rivers_map,stream_order==3),
               color = "#028090",
               opacity = 1,
               weight = 3) %>% 
  addPolylines(data = filter(pnw_rivers_map,stream_order==2),
               color = "#176B88",
               opacity = 1,
               weight = 2) %>% 
  addPolylines(data = filter(pnw_rivers_map,stream_order==1),
               color = "#2E4F79",
               opacity = 1,
               weight = 0.5) %>% 
  addProviderTiles("Esri.WorldImagery")


# Randomized reaches for allometric constraint
sample_size = 20
data <- rcm_23_model_output_dat %>% 
  select(basin, 
         comid,
         tocomid,
         stream_order)

resulting_paths <- traceDownstream(data, sample_size)

# Assuming you have the resulting_paths dataframe:
filtered_df <- extractFilteredPathOrderCombinations(resulting_paths)


# Building a larger dataset by iteratively identifying non-overlapping paths and 
# storing the data
n_iterations <- 20  # Change this to the desired number of iterations

# Pre-allocate a list of the size we know we'll need
all_results <- vector("list", n_iterations)

for (i in 1:n_iterations) {
  resulting_paths <- traceDownstream(data, sample_size)
  filtered_df <- extractFilteredPathOrderCombinations(resulting_paths)
  all_results[[i]] <- filtered_df
}

# Combine all the results using rbindlist (faster than rbind)
compiled_results <- rbindlist(all_results)

length(unique(compiled_results$comid))

# Sort by basin
setorder(compiled_results, basin)

print(compiled_results)

summary(compiled_results)

write.csv(compiled_results,paste(local_data,"randomized_paths_allometric_constraint.csv",sep = '/'),
          row.names = FALSE)

pnw_rivers_paths <- pnw_rivers_dat %>% 
  select(COMID) %>% 
  rename(comid = COMID) %>% 
  merge(.,
        compiled_results,
        by = "comid", 
        all.y = TRUE)

# Stream orders rank from 1 - 7, let's take a look at the map to identify reaches
# with the maximum order

leaflet(pnw_rivers_paths) %>% 
  addPolylines(weight = 0.5,
               color = "gray",
               opacity = 0.5) %>%
  addPolylines(data = filter(pnw_rivers_paths,stream_order==7),
               color = "#EDEF5C",
               opacity = 1,
               weight = 7) %>%
  addPolylines(data = filter(pnw_rivers_paths,stream_order==6),
               color = "#BEE05E",
               opacity = 1,
               weight = 6) %>%
  addPolylines(data = filter(pnw_rivers_paths,stream_order==5),
               color = "#78C86F",
               opacity = 1,
               weight = 5) %>% 
  addPolylines(data = filter(pnw_rivers_paths,stream_order==4),
               color = "#08A47F",
               opacity = 1,
               weight = 4) %>% 
  addPolylines(data = filter(pnw_rivers_paths,stream_order==3),
               color = "#028090",
               opacity = 1,
               weight = 3) %>% 
  addPolylines(data = filter(pnw_rivers_paths,stream_order==2),
               color = "#176B88",
               opacity = 1,
               weight = 2) %>% 
  addPolylines(data = filter(pnw_rivers_paths,stream_order==1),
               color = "#2E4F79",
               opacity = 1,
               weight = 0.5) #%>% 
# addProviderTiles("Esri.WorldImagery")

################################################################################

# # TEST
# 
# # Randomized segments for allometric constraint analysis
# source("./source/test_function_randomized_paths.R")
# n = 20
# data <- rcm_23_model_output_dat %>% 
#   select(basin, 
#          comid,
#          tocomid,
#          stream_order)
# 
# select_segments <- sampleUnconnectedReaches(data)
# filtered_df <- extractFilteredPathOrderCombinations(select_segments)
# 
# 
# n_iterations <- 30 # Change this to the desired number of iterations
# 
# all_results <- vector("list", n_iterations)
# 
# # Track all sampled comids across iterations and their directly connected segments
# all_sampled_comids <- integer(0)
# 
# for (i in 1:n_iterations) {
#   select_segments <- sampleUnconnectedReaches(data)
#   filtered_df <- select_segments
#   all_results[[i]] <- filtered_df
#   
#   # Update the list of all sampled comids and their directly connected segments
#   current_sampled_comids <- unique(as.integer(filtered_df$comid))
#   connected_comids <- unique(c(as.integer(data[data$comid %in% current_sampled_comids,]$tocomid),
#                                as.integer(data[data$tocomid %in% current_sampled_comids,]$comid)))
#   all_sampled_comids <- unique(c(all_sampled_comids, current_sampled_comids, connected_comids))
# }
# 
# compiled_results <- rbindlist(all_results)
# setorder(compiled_results, basin)
# print(compiled_results)
# 
# 
# 
# selected_reaches <- pnw_rivers_dat %>% 
#   select(COMID) %>% 
#   rename(comid = COMID) %>% 
#   merge(.,
#         compiled_results,
#         by = "comid", 
#         all.y = TRUE) %>% 
#   merge(.,
#         rcm_23_model_output_dat,
#         by = "comid",
#         all.x = TRUE)
# 
# # Mapping selected reaches
# 
# p <- ggplot(data = selected_reaches,
#             aes(x = wshd_area_km2,
#                 y = totco2_o2g_m2_day))+
#   geom_point()+
#   scale_x_log10()+
#   scale_y_log10()+
#   facet_wrap(~basin.x, ncol = 2)
# p
# 
# 
# 
# length(unique(selected_reaches$comid))
# 
# leaflet(pnw_rivers_map) %>% 
#   addPolylines(weight = 0.5,
#                color = "gray",
#                opacity = 0.5) %>%
#   addPolylines(data = filter(selected_reaches,stream_order.x==6),
#                color = "#BEE05E",
#                opacity = 1,
#                weight = 6) %>%
#   addPolylines(data = filter(selected_reaches,stream_order.x==5),
#                color = "#78C86F",
#                opacity = 1,
#                weight = 5) %>% 
#   addPolylines(data = filter(selected_reaches,stream_order.x==4),
#                color = "#08A47F",
#                opacity = 1,
#                weight = 4) %>% 
#   addPolylines(data = filter(selected_reaches,stream_order.x==3),
#                color = "#028090",
#                opacity = 1,
#                weight = 3) %>% 
#   addPolylines(data = filter(selected_reaches,stream_order.x==2),
#                color = "#176B88",
#                opacity = 1,
#                weight = 2) %>% 
#   addPolylines(data = filter(selected_reaches,stream_order.x==1),
#                color = "#2E4F79",
#                opacity = 1,
#                weight = 0.5)
# 
# 
# 
# 
# 
# 
# 
