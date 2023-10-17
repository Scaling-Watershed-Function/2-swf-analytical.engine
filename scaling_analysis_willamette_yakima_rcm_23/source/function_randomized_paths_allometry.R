################################################################################
# Randomization of stream reaches across changing flowpaths for estimation of 
# Allometric constraints
################################################################################

library(data.table)
library(librarian)

traceDownstream <- function(data, n) {
  
  # Convert data to data.table if it's not already
  if(!inherits(data, "data.table")) {
    setDT(data)
  }
  
  unique_basins <- unique(data$basin)
  all_paths_list <- vector("list", length(unique_basins))
  
  path_counter <- 0 # Initialize a path counter for unique path numbers here
  
  for (j in seq_along(unique_basins)) {
    
    basin = unique_basins[j]
    basin_data <- data[basin == basin]
    max_order_minus_one <- max(basin_data$stream_order) - 1
    
    # Get n random order 1 streams for the basin
    order1_streams <- basin_data[stream_order == 1]
    if(nrow(order1_streams) < n) {
      stop("Number of first order streams is less than n for basin: ", basin)
    }
    random_order1_streams <- order1_streams[sample(.N, n)]
    
    paths_list <- vector("list", n)
    
    for(i in 1:n) {
      
      path_counter <- path_counter + 1 # Increment the counter for each path
      
      current_comid <- random_order1_streams$comid[i]
      path <- list()
      path_used_comids <- integer()
      
      while(TRUE) {
        
        current_stream <- basin_data[comid == current_comid]
        
        # If this comid has been used for this path, there's no downstream, or we reach max_order_minus_one, break
        if (current_stream$comid %in% path_used_comids || nrow(current_stream) == 0 || current_stream$stream_order == max_order_minus_one) {
          break
        }
        
        path <- rbindlist(list(path, current_stream))
        path_used_comids <- c(path_used_comids, current_stream$comid)
        current_comid <- current_stream$tocomid
      }
      
      path[, path_number := path_counter] # Assign the unique path number
      
      # Add path to paths list
      paths_list[[i]] <- path
    }
    
    all_paths_list[[j]] <- rbindlist(paths_list)
  }
  
  return(rbindlist(all_paths_list))
}

extractFilteredPathOrderCombinations <- function(paths) {
  # Get unique basins and path numbers
  unique_basins <- unique(paths$basin)
  result <- list()
  
  for (basin in unique_basins) {
    basin_data <- paths[basin == paths$basin]
    unique_path_numbers <- unique(basin_data$path_number)
    
    used_orders <- integer(0)
    for (path_num in unique_path_numbers) {
      available_orders <- setdiff(unique(basin_data$stream_order), used_orders)
      
      # If no available orders, break out of this loop
      if (length(available_orders) == 0) break
      
      # Choose the smallest available stream order
      chosen_order <- min(available_orders)
      used_orders <- c(used_orders, chosen_order)
      
      path_data <- basin_data[path_number == path_num & stream_order == chosen_order]
      result <- append(result, list(path_data))
    }
  }
  result_df <- rbindlist(result)
  
  # Sorting the result by basin
  setorder(result_df, basin)
  
  # Extract one row for each unique combination of basin, path_number, and stream_order
  unique_rows <- unique(result_df, by = c("basin", "path_number", "stream_order"))
  
  return(unique_rows)
}
