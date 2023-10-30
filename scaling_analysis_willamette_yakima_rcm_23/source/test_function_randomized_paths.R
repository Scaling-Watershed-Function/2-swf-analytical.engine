################################################################################
# Randomization of stream reaches across changing flowpaths for estimation of 
# Allometric constraints
################################################################################

librarian::shelf(data.table)

traceDownstream <- function(data, n, overall_excluded_comids = integer(0)) {
  if(!inherits(data, "data.table")) {
    setDT(data)
  }
  
  data$comid <- as.integer(data$comid)
  data$stream_order <- as.integer(data$stream_order)
  setkey(data, comid)
  
  unique_basins <- unique(data$basin)
  all_paths_list <- vector("list", length(unique_basins))
  
  for (j in seq_along(unique_basins)) {
    basin = unique_basins[j]
    basin_data <- data[basin == basin]
    max_order_minus_one <- max(basin_data$stream_order) - 1
    
    excluded_comids <- unique(c(overall_excluded_comids, integer(0)))
    
    paths_list <- vector("list", max_order_minus_one)
    
    for(order in 1:max_order_minus_one) {
      # Debugging code
      print(paste("Is comid %in% excluded_comids a logical vector?", is.logical(basin_data$comid %in% excluded_comids)))
      print(paste("Type of stream_order:", class(basin_data$stream_order[1])))
      print(paste("Type of order:", class(order)))
      
      is_not_in_excluded <- !(basin_data$comid %in% excluded_comids)
      is_order_match <- basin_data$stream_order == order
      order_streams <- basin_data[is_not_in_excluded & is_order_match]
      
      if(nrow(order_streams) < n) {
        n = nrow(order_streams)
      }
      
      sampled_streams <- order_streams[sample(.N, n)]
      paths_list[[order]] <- sampled_streams
      
      downstream_comids <- unlist(lapply(sampled_streams$comid, getAllDownstreamComids, data = basin_data))
      upstream_comids <- unlist(lapply(sampled_streams$comid, getAllUpstreamComids, data = basin_data))
      
      excluded_comids <- unique(c(excluded_comids, downstream_comids, upstream_comids))
    }
    
    all_paths_list[[j]] <- rbindlist(paths_list)
  }
  
  result <- rbindlist(all_paths_list)
  setorder(result, basin)
  return(result)
}

getAllDownstreamComids <- function(comid, data) {
  downstream <- c()
  while(TRUE) {
    next_segment <- data[tocomid == comid]
    if(nrow(next_segment) == 0) {
      break
    }
    downstream <- c(downstream, next_segment$comid)
    comid <- next_segment$comid
  }
  return(downstream)
}

getAllUpstreamComids <- function(comid, data_table) {
  upstream <- c()
  current_tocomid <- comid
  
  while(TRUE) {
    # Debugging code
    print(paste("Length of current_tocomid:", length(current_tocomid)))
    print(paste("Value of current_tocomid:", current_tocomid))
    
    contributing_segments <- data_table[data_table$tocomid %in% current_tocomid,]
    if(nrow(contributing_segments) == 0) {
      break
    }
    upstream <- c(upstream, contributing_segments$comid)
    current_tocomid <- unique(as.numeric(contributing_segments$comid))
  }
  return(upstream)
}








extractFilteredPathOrderCombinations <- function(paths) {
  # Get unique basins
  unique_basins <- unique(paths$basin)
  result <- list()
  
  for (basin in unique_basins) {
    basin_data <- paths[basin == paths$basin]
    max_order <- max(basin_data$stream_order)
    used_orders <- integer(0)
    
    for (order in 1:max_order) {
      available_orders <- setdiff(unique(basin_data$stream_order), used_orders)
      
      # If no available orders, break out of this loop
      if (length(available_orders) == 0) break
      
      # Choose the smallest available stream order
      chosen_order <- min(available_orders)
      used_orders <- c(used_orders, chosen_order)
      
      path_data <- basin_data[stream_order == chosen_order]
      result <- append(result, list(path_data))
    }
  }
  
  result_df <- rbindlist(result)
  
  # Sorting the result by basin
  setorder(result_df, basin)
  
  # Extract one row for each unique combination of basin and stream_order
  unique_rows <- unique(result_df, by = c("basin", "stream_order"))
  
  return(unique_rows)
}

sampleUnconnectedReaches <- function(data_table) {
  # Sort by stream_order in descending order
  data_table <- data_table[order(-data_table$stream_order), ]
  
  
  # Initialize the list for sampled comids
  sampled_comids <- c()
  
  # Placeholder list for excluded comids (those that are upstream of sampled reaches)
  excluded_comids <- c()
  
  for(i in 1:nrow(data_table)) {
    current_comid <- data_table$comid[i]
    
    # If the current comid is neither sampled nor excluded
    if(!(current_comid %in% sampled_comids) && !(current_comid %in% excluded_comids)) {
      # Add current comid to sampled list
      sampled_comids <- c(sampled_comids, current_comid)
      
      # Find all upstream comids for current comid
      upstream_comids <- getAllUpstreamComids(current_comid, data_table)
      
      # Add upstream comids to the excluded list
      excluded_comids <- c(excluded_comids, upstream_comids)
    }
  }
  
  # Return sampled comids
  return(data_table[data_table$comid %in% sampled_comids,])
}

getAllUpstreamComids <- function(comid, data_table) {
  upstream <- c()
  current_tocomid <- comid
  
  while(TRUE) {
    contributing_segments <- data_table[which(data_table$tocomid %in% current_tocomid), ]
    
    if(nrow(contributing_segments) == 0) {
      break
    }
    
    upstream <- c(upstream, contributing_segments$comid)
    current_tocomid <- contributing_segments$comid
  }
  
  return(upstream)
}


