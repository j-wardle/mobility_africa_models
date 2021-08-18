
# Load data ---------------------------------------------------------------

## Read in location data
# Smallest adm units
portugal_location_data <- readRDS("portugal_location_data.rds")
portugal_location_data <- as.location_dataframe(portugal_location_data)
france_location_data <- readRDS("france_location_data.rds")
france_location_data <- as.location_dataframe(france_location_data)

# Aggregated adm units
portugal_adm1_location_data <- readRDS("portugal_adm1_location_data.rds")
portugal_adm1_location_data <- as.location_dataframe(portugal_adm1_location_data)
france_adm2_location_data <- readRDS("france_adm2_location_data.rds")
france_adm2_location_data <- as.location_dataframe(france_adm2_location_data)

## Read in scaled movement data
scaled_matrix <- readRDS("scaled_matrix.rds")
aggregated_scaled_matrix <- readRDS("aggregated_scaled_matrix.rds")



# Fit gravity models ------------------------------------------------------

## Fit gravity model to Portugal data (adm2 :- the adm unit for which we have raw movement data)

prtl_conn <- prepare_movement_data(portugal_location_data,
                                   scaled_matrix[["portugal"]])
  
gravity_portugal_adm2 <- movement(prtl_conn ~ portugal_location_data, flux_model = gravity())

## Fit gravity model to France data (adm3 :- the adm unit for which we have raw movement data)

fra_conn <- prepare_movement_data(france_location_data,
                                  scaled_matrix[["france"]])

gravity_france_adm3 <- movement(fra_conn ~ france_location_data, flux_model = gravity())

## Save model coefficients

models <- list(gravity_portugal_adm2,
            gravity_france_adm3)
names(models) <- c("gravity_portugal_adm2", "gravity_france_adm3")

coefficients <- map_dfr(models, function(m) {
  
  data.frame(theta = m[["coefficients"]][[1]],
             alpha = m[["coefficients"]][[2]],
             beta = m[["coefficients"]][[3]],
             gamma = m[["coefficients"]][[4]])
  
}, .id = "model")

saveRDS(coefficients, "model_coefficients.rds")





# Predict flows from gravity fits -----------------------------------------

## 1) Predict flows for the same spatial unit we fitted to (and in same countries)

portugal_predict_adm2 <- predict(gravity_portugal_adm2, portugal_location_data,
                                 flux_model = gravity(), symmetric = FALSE)

france_predict_adm3 <- predict(gravity_france_adm3, france_location_data,
                               flux_model = gravity(), symmetric = FALSE)

## 2) Predict flows for the same spatial unit in the other country

portugal_predict_adm2_alt <- predict(gravity_france_adm3, portugal_location_data,
                                 flux_model = gravity(), symmetric = FALSE)

france_predict_adm3_alt <- predict(gravity_portugal_adm2, france_location_data,
                               flux_model = gravity(), symmetric = FALSE)

## Collate  predicted flows

predicted_movements <- map(ls(pattern = "_predict_adm"), function(prediction) {
  
  get(prediction)[["movement_matrix"]]
  
})

names(predicted_movements) <- ls(pattern = "_predict_adm")



# Populate diagonals of predicted matrices --------------------------------
# The gravity model does not predict numbers of 'non-commuters' (diagonals of the movement matrix)
# We use two different methods to infer the size of these diagonals

## Method 1
# Use observed data as diagonals then normalise so that the row sum is equal to the patch population

method1_predictions <- imap(predicted_movements, function(prediction, name) {
  
  if(isTRUE(startsWith(name, "france"))) {
    
    observed_movement <- fra_conn
    
  }
  
  if(isTRUE(startsWith(name, "portugal"))) {
    
    observed_movement <- prtl_conn
    
  }
  
  p_stay <- diag(observed_movement) / rowSums(observed_movement)
  
  conn_method1 <- (1 - p_stay) * prediction / rowSums(prediction)
  diag(conn_method1) <- p_stay
  conn_method1 <- as.movement_matrix(conn_method1)
  
  if (!(isTRUE(sum(rowSums(conn_method1)) == nrow(conn_method1)))) {
    warning("Row sums of probability matrix do not equal 1")
  }
  
  conn_method1
})

saveRDS(method1_predictions, "gravity_matrix1_normalised.rds")


## Method 2
# Use national average of p_stay as matrix diagonals,
# where p_stay is the probability a person does not commute from their patch.
# Use the same p_stay across all patches for a given matrix

# first calculate average p_stay in different movement matrices

fra_p_stay <- p_stay_average(fra_conn)
prtl_p_stay <- p_stay_average(prtl_conn)

# now estimate the movement matrices

method2_predictions <- imap(predicted_movements, function(prediction, name) {
  
  if(isTRUE(startsWith(name, "france"))) {
    
    if(isTRUE(grepl("alt", name))) {
      p_stay_avg <- prtl_p_stay
    } else {
      p_stay_avg <- fra_p_stay
    }
  }
  
  if(isTRUE(startsWith(name, "portugal"))) {
    
    if(isTRUE(grepl("alt", name))) {
      p_stay_avg <- fra_p_stay
    } else {
      p_stay_avg <- prtl_p_stay
    }
  }
  
  conn_method2 <- (1 - p_stay_avg) * prediction / rowSums(prediction)
  diag(conn_method2) <- p_stay_avg
  conn_method2 <- as.movement_matrix(conn_method2)
  
  if (!(isTRUE(sum(rowSums(conn_method2)) == nrow(conn_method2)))) {
    warning("Row sums of probability matrix do not equal 1")
  }
  
  conn_method2
})

saveRDS(method2_predictions, "gravity_matrix2_normalised.rds")


# For comparison of the predictions from different methods, can get the absolute movement numbers 
# based on probability matrices and population sizes.

method1_numbers <- prob_predictions_to_numbers(method1_predictions)
method2_numbers <- prob_predictions_to_numbers(method2_predictions)

saveRDS(method1_numbers, "gravity_matrix1_numbers.rds")
saveRDS(method2_numbers, "gravity_matrix2_numbers.rds")



######################################
## OPPOSITE AGGREGATION APPROACH ----
## Fit to aggregated data, then predict high resolution movements
######################################

# Fit gravity models ------------------------------------------------------

## Fit gravity model to Portugal data (adm1 :- the aggregated raw movement data)

prtl_aggr_conn <- prepare_movement_data(portugal_adm1_location_data,
                                   aggregated_scaled_matrix[["portugal"]])

gravity_portugal_adm1 <- movement(prtl_aggr_conn ~ portugal_adm1_location_data, flux_model = gravity())

## Fit gravity model to France data (adm2 :- the aggregated raw movement data)

fra_aggr_conn <- prepare_movement_data(france_adm2_location_data,
                                       aggregated_scaled_matrix[["france"]])

gravity_france_adm2 <- movement(fra_aggr_conn ~ france_adm2_location_data, flux_model = gravity())

## Save model coefficients

alt_aggr_models <- list(gravity_portugal_adm1,
                        gravity_france_adm2)
names(alt_aggr_models) <- c("gravity_portugal_adm1", "gravity_france_adm2")

alt_aggr_coefficients <- map_dfr(alt_aggr_models, function(m) {
  
  data.frame(theta = m[["coefficients"]][[1]],
             alpha = m[["coefficients"]][[2]],
             beta = m[["coefficients"]][[3]],
             gamma = m[["coefficients"]][[4]])
  
}, .id = "model")

saveRDS(alt_aggr_coefficients, "alt_aggr_model_coefficients.rds")


# Predict flows from gravity fits -----------------------------------------

# skip these lo res predictions for now.
# ## 1) Predict flows for the same spatial unit we fitted to (and in same countries)
# 
# portugal_predict_adm1_aggr2 <- predict(gravity_portugal_adm1, portugal_adm1_location_data,
#                                  flux_model = gravity(), symmetric = FALSE)
# 
# france_predict_adm2_aggr2 <- predict(gravity_france_adm2, france_adm2_location_data,
#                                flux_model = gravity(), symmetric = FALSE)
# 
# ## 2) Predict flows for the same spatial unit in the other country
# 
# portugal_predict_adm1_aggr2_alt <- predict(gravity_france_adm2, portugal_adm1_location_data,
#                                      flux_model = gravity(), symmetric = FALSE)
# 
# france_predict_adm2_aggr2_alt <- predict(gravity_portugal_adm1, france_adm2_location_data,
#                                    flux_model = gravity(), symmetric = FALSE)

## 3) Predict flows for the higher res spatial units (for the same countries as we fitted to)

portugal_predict_adm2_aggr2 <- predict(gravity_portugal_adm1, portugal_location_data,
                                 flux_model = gravity(), symmetric = FALSE)

france_predict_adm3_aggr2 <- predict(gravity_france_adm2, france_location_data,
                               flux_model = gravity(), symmetric = FALSE)

## 4) Predict flows for the higher res spatial units (in the country we did not fit to)

portugal_predict_adm2_aggr2_alt <- predict(gravity_france_adm2, portugal_location_data,
                                     flux_model = gravity(), symmetric = FALSE)

france_predict_adm3_aggr2_alt <- predict(gravity_portugal_adm1, france_location_data,
                                   flux_model = gravity(), symmetric = FALSE)

## Collate predicted flows

predicted_movements_lo_res_fit <- map(ls(pattern = "aggr2"), function(prediction) {
  
  get(prediction)[["movement_matrix"]]
  
})

names(predicted_movements_lo_res_fit) <- ls(pattern = c("aggr2"))



# Populate diagonals of predicted matrices --------------------------------
# The gravity model does not predict numbers of 'non-commuters' (diagonals of the movement matrix)
# We use two different methods to infer the size of these diagonals

## Method 1
# Use observed data as diagonals then normalise so that the row sum is equal to the patch population

# Use the formatted lo res data from above (prtl_conn and fra_conn)

# now estimate the movement matrices

lo_res_fit_predictions_mthd1 <- imap(predicted_movements_lo_res_fit, function(prediction, name) {
  
  if(isTRUE(startsWith(name, "france"))) {
    
    if(isTRUE(grepl("adm3", name))) {
      observed_movement <- fra_conn
    } else {
      observed_movement <- fra_aggr_conn
    }
  }
  
  if(isTRUE(startsWith(name, "portugal"))) {
    
    if(isTRUE(grepl("adm2", name))) {
      observed_movement <- prtl_conn
    } else {
      observed_movement <- prtl_aggr_conn
    }
  }
  
  p_stay <- diag(observed_movement) / rowSums(observed_movement)
  
  # conn_method1 <- prediction
  # diag(conn_method1) <-
  # conn_method1 <- round(conn_method1)
  # conn_method1 <- as.movement_matrix(conn_method1)
  # conn_method1_probability <- conn_method1 / rowSums(conn_method1)
  # conn_method1_probability <- as.movement_matrix(conn_method1_probability)
  
  conn_method1 <- (1 - p_stay) * prediction / rowSums(prediction)
  diag(conn_method1) <- p_stay
  conn_method1 <- as.movement_matrix(conn_method1)
  
  if (!(isTRUE(sum(rowSums(conn_method1)) == nrow(conn_method1)))) {
    warning("Row sums of probability matrix do not equal 1")
  }
  
  conn_method1
})

saveRDS(lo_res_fit_predictions_mthd1, "lo_res_gravity_matrix1_normalised.rds")


## Method 2
# Use national average of p_stay as matrix diagonals,
# where p_stay is the probability a person does not commute from their patch.
# Use the same p_stay across all patches for a given matrix

# first calculate average p_stay in different movement matrices

fra_p_stay <- p_stay_average(fra_conn)
fra_aggr_p_stay <- p_stay_average(fra_aggr_conn)
prtl_p_stay <- p_stay_average(prtl_conn)
prtl_aggr_p_stay <- p_stay_average(prtl_aggr_conn)

# now estimate the movement matrices

lo_res_fit_predictions_mthd2 <- imap(predicted_movements_lo_res_fit, function(prediction, name) {
  
  if(isTRUE(startsWith(name, "france"))) {
    
    if(isTRUE(grepl("alt", name))) {
      p_stay_avg <- prtl_p_stay
    } else {
      p_stay_avg <- fra_p_stay
    }
  }
  
  if(isTRUE(startsWith(name, "portugal"))) {
    
    if(isTRUE(grepl("alt", name))) {
      p_stay_avg <- fra_p_stay
    } else {
      p_stay_avg <- prtl_p_stay
    }
  }
  
  conn_method2 <- (1 - p_stay_avg) * prediction / rowSums(prediction)
  diag(conn_method2) <- p_stay_avg
  conn_method2 <- as.movement_matrix(conn_method2)
  
  if (!(isTRUE(sum(rowSums(conn_method2)) == nrow(conn_method2)))) {
    warning("Row sums of probability matrix do not equal 1")
  }
  
  conn_method2
})

saveRDS(lo_res_fit_predictions_mthd2, "lo_res_gravity_matrix2_normalised.rds")


# For comparison of the predictions from different methods, can get the absolute movement numbers 
# based on probability matrices and population sizes.

lo_res_fit_mthd1_numbers <- prob_predictions_to_numbers(lo_res_fit_predictions_mthd1)
lo_res_fit_mthd2_numbers <- prob_predictions_to_numbers(lo_res_fit_predictions_mthd2)

saveRDS(lo_res_fit_mthd1_numbers, "lo_res_gravity_matrix1_numbers.rds")
saveRDS(lo_res_fit_mthd2_numbers, "lo_res_gravity_matrix2_numbers.rds")
