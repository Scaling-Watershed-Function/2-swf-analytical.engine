

# Helper function to get all downstream nodes for a given node
get_downstream_nodes <- function(df, node) {
  downstream_nodes <- df$FromNode[df$ToNode == node]
  all_downstream_nodes <- downstream_nodes
  for (down_node in downstream_nodes) {
    all_downstream_nodes <- c(all_downstream_nodes, get_downstream_nodes(df, down_node))
  }
  return(all_downstream_nodes)
}

cumulative_function <- function(data, variable, operation = "sum") {
  nhd_data <- data.frame(data)
  new_colname <- paste("accm", variable, sep="_")
  
  nhd_data[, new_colname] <- numeric(nrow(nhd_data))
  
  for (node in nhd_data$FromNode) {
    downstream_nodes <- get_downstream_nodes(nhd_data, node)
    nodes_to_operate <- c(node, downstream_nodes)
    if (operation == "sum") {
      nhd_data[node, new_colname] <- sum(nhd_data[nodes_to_operate, variable])
    } else if (operation == "avg") {
      nhd_data[node, new_colname] <- mean(nhd_data[nodes_to_operate, variable])
    } else if (operation == "sd") {
      nhd_data[node, new_colname] <- sd(nhd_data[nodes_to_operate, variable])
    } else {
      stop("Invalid operation specified. Choose from 'sum', 'avg', or 'sd'.")
    }
  }
  
  return(nhd_data)
}

# Sample dataframe
nhd_YRB_stream_resp <- data.frame(
  FromNode = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  ToNode = c(5, 5, 6, 6, 9, 8, 8, 10, 10, 11),
  comid = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
  totco2g_day_fill = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55)
)



# Example usages:
result_sum <- cumulative_function(nhd_YRB_stream_resp, "totco2g_day_fill", "sum")
print(result_sum)

result_avg <- cumulative_function(nhd_YRB_stream_resp, "totco2g_day_fill", "avg")
print(result_avg)

result_sd <- cumulative_function(nhd_YRB_stream_resp, "totco2g_day_fill", "sd")
print(result_sd)




