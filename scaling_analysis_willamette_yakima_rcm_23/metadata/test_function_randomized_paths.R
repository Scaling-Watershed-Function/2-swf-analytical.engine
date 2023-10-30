################################################################################
# Randomization of stream reaches across changing flowpaths for estimation of 
# Allometric constraints
################################################################################

librarian::shelf(data.table)

traceDownstream <- function(data, n) {
  # Convert data to data.table if it's not already
  if(!inherits(data, "data.table")) {
    setDT(data)
  }
  
  # Set key for faster subsetting
  setkey(data, comid)
  
  unique_basins <- unique(data$basin)
  all_paths_list <- vector("list", length(unique_basins))
  
  for (j in seq_along(unique_basins)) {
    basin = unique_basins[j]
    basin_data <- data[basin == basin]
    max_order_minus_one <- max(basin_data$stream_order) - 1
    
    # Use a data.table for excluded_comids for faster exclusion checks
    excluded_comids <- data.table(comid = integer(0))
    
    paths_list <- vector("list", max_order_minus_one)
    
    for(order in 1:max_order_minus_one) {
      # Filter streams of the current order excluding the excluded comids
      order_streams <- basin_data[!(comid %in% excluded_comids$comid) & stream_order == order]
      
      if(nrow(order_streams) < n) {
        stop(paste("Number of order", order, "streams is less than n for basin:", basin))
      }
      
      sampled_streams <- order_streams[sample(.N, n)]
      paths_list[[order]] <- sampled_streams
      
      # Add all downstream segments' comids to the excluded_comids list
      for(sampled_comid in sampled_streams$comid) {
        while(TRUE) {
          current_stream <- basin_data[comid == sampled_comid]
          if(nrow(current_stream) == 0 || current_stream$stream_order == max_order_minus_one) {
            break
          }
          excluded_comids <- rbind(excluded_comids, data.table(comid = current_stream$tocomid))
          sampled_comid <- current_stream$tocomid
        }
      }
    }
    
    all_paths_list[[j]] <- rbindlist(paths_list)
  }
  
  return(rbindlist(all_paths_list))
}


extractUniqueBasinOrderCombinations <- function(paths) {
  # Sorting the result by basin and stream order
  setorder(paths, basin, stream_order)
  
  # Extract one row for each unique combination of basin and stream_order
  unique_rows <- unique(paths, by = c("basin", "stream_order"))
  
  return(unique_rows)
}