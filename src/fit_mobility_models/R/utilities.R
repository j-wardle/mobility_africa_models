fit_gravity_model <- function(location_data, movement_matrix) {
  
  location_data <- as.location_dataframe(location_data)
  
  conn <- movement_matrix
  conn <- as.movement_matrix(conn)
  colnames(conn) = location_data$location
  rownames(conn) = location_data$location
  
  gravity_model <- movement(conn ~ location_data, flux_model = gravity())
  
}