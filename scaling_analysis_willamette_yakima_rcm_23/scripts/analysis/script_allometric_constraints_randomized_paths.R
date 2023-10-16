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
source("./source/script_randomized_paths_allometric_constraints.R")


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
n_iterations <- 500  # Change this to the desired number of iterations

# Pre-allocate a list of the size we know we'll need
all_results <- vector("list", n_iterations)

for (i in 1:n_iterations) {
  resulting_paths <- traceDownstream(data, sample_size)
  filtered_df <- extractFilteredPathOrderCombinations(resulting_paths)
  all_results[[i]] <- filtered_df
}

# Combine all the results using rbindlist (faster than rbind)
compiled_results <- rbindlist(all_results)

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




