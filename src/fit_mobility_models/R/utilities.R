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

prob_predictions_to_numbers <- function(x) {
  
  imap(x, function(predictions, name) {
    
    if(isTRUE(startsWith(name, "france"))) {
      
      if(isTRUE(grepl("adm3", name))) {
        location_data <- france_location_data
      } else {
        location_data <- france_adm2_location_data
      }
    }
    
    if(isTRUE(startsWith(name, "portugal"))) {
      
      if(isTRUE(grepl("adm2", name))) {
        location_data <- portugal_location_data
      } else {
        location_data <- portugal_adm1_location_data
      }
    }
    
    conn_numbers <- predictions * location_data$population
    conn_numbers <- as.movement_matrix(round(conn_numbers))
    
    conn_numbers
  }) 
}
