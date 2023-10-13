# Cumulative Function across River Network

cumulative_function_pipe <- function(data, variable_name, operation = "sum") {
  
  current_column <- data[[variable_name]]
  n <- length(current_column)
  
  accm_data <- numeric(n)  # Initialize the accumulator
  
  # Calculate the cumulative value for each node
  for (node in 1:n) {
    incoming_nodes <- which(data$ToNode == node)
    
    if (length(incoming_nodes) == 0) {
      # If no incoming nodes, just use the value for the current node
      accm_data[node] <- current_column[node]
    } else {
      # If there are incoming nodes, aggregate their accumulated values
      if (operation == "sum") {
        accm_data[node] <- sum(accm_data[incoming_nodes]) + current_column[node]
      } else if (operation == "mean") {
        all_values <- c(current_column[node], accm_data[incoming_nodes])
        accm_data[node] <- mean(all_values)
      } else if (operation == "sd") {
        all_values <- c(current_column[node], accm_data[incoming_nodes])
        accm_data[node] <- sd(all_values)
      }
    }
  }
  
  return(accm_data)
}


