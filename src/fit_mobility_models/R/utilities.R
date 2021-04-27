prepare_movement_data <- function(location_data, movement_source) {
  
  conn <- movement_source
  conn <- as.movement_matrix(conn)
  colnames(conn) <-  location_data$location
  rownames(conn) <-  location_data$location
  
  conn
  
}

p_stay_average <- function(observed_movement) {
  
  out <- diag(observed_movement) / rowSums(observed_movement)
  out <- mean(out)
  
}