
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



# Fit gravity models ------------------------------------------------------

## Fit gravity model to Portugal data (adm2 :- the adm unit for which we have raw movement data)

prtl_conn <- scaled_matrix[["portugal"]]
prtl_conn <- as.movement_matrix(prtl_conn)
colnames(prtl_conn) <-  portugal_location_data$location
rownames(prtl_conn) <-  portugal_location_data$location

gravity_portugal_adm2 <- movement(prtl_conn ~ portugal_location_data, flux_model = gravity())

## Fit gravity model to France data (adm3 :- the adm unit for which we have raw movement data)

fra_conn <- scaled_matrix[["france"]]
fra_conn <- as.movement_matrix(fra_conn)
colnames(fra_conn) <-  france_location_data$location
rownames(fra_conn) <-  france_location_data$location

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

## 3) Predict flows for the aggregated spatial units (for the same countries as we fitted to)

portugal_predict_adm1 <- predict(gravity_portugal_adm2, portugal_adm1_location_data,
                                 flux_model = gravity(), symmetric = FALSE)

france_predict_adm2 <- predict(gravity_france_adm3, france_adm2_location_data,
                               flux_model = gravity(), symmetric = FALSE)

## 4) Predict flows for the aggregated spatial units (in the country we did not fit to)

portugal_predict_adm1_alt <- predict(gravity_france_adm3, portugal_adm1_location_data,
                                 flux_model = gravity(), symmetric = FALSE)

france_predict_adm2_alt <- predict(gravity_portugal_adm2, france_adm2_location_data,
                               flux_model = gravity(), symmetric = FALSE)

## Collate  predicted flows

predicted_movements <- map(ls(pattern = "_predict_adm"), function(prediction) {
  
  get(prediction)[["movement_matrix"]]
  
})

names(predicted_movements) <- ls(pattern = "_predict_adm")


### For epidemic model we will three versions of the mobility matrix
# One uses the raw commuting data
# The others use predicted flows from gravity model. Gravity model does not predicting 'non-commuters'.
# Therefore we infer diagonals in two different ways as follows.

## Method 1
# Use raw observations along diagonals

conn_method1 <- conn_predict
diag(conn_method1) <- diag(conn)
conn_method1 <- round(conn_method1)
conn_method1 <- as.movement_matrix(conn_method1)
conn_method1_probability <- conn_method1 / rowSums(conn_method1)
conn_method1_probability <- as.movement_matrix(conn_method1_probability)

conn_mthd1_pm <- conn_predict_pm
diag(conn_mthd1_pm) <- diag(conn)
conn_mthd1_pm <- round(conn_mthd1_pm)
conn_mthd1_pm <- as.movement_matrix(conn_mthd1_pm)
conn_mthd1_pm_prob <- conn_mthd1_pm / rowSums(conn_mthd1_pm)
conn_mthd1_pm_prob <- as.movement_matrix(conn_mthd1_pm_prob)

saveRDS(conn_method1_probability, "data/prtgl_gravity_matrix1.rds")
saveRDS(conn_mthd1_pm_prob, "data/prtgl_gravity_matrix1_pm.rds")

## Method 2
# Use national average of p_stay as diagonals
# Use the same p_stay in all patches

p_stay <- diag(conn) / rowSums(conn)
p_stay_avg <- mean(p_stay) # 0.296

conn_method2 <- (1 - p_stay_avg) * conn_predict / rowSums(conn_predict)
diag(conn_method2) <- p_stay_avg

# set the numbers moving in am and pm
conn_method2 <- conn_method2 * location_data$population
conn_method2_pm <- t(conn_method2)

# now rescale to probabilities
conn_mthd2_prob <- conn_method2 / rowSums(conn_method2)
conn_mthd2_pm_prob <- conn_method2_pm / rowSums(conn_method2_pm)

# convert to movement matrix objects
conn_mthd2_prob <- as.movement_matrix(conn_mthd2_prob)
conn_mthd2_pm_prob <- as.movement_matrix(conn_mthd2_pm_prob)

saveRDS(conn_mthd2_prob, "data/prtgl_gravity_matrix2.rds")
saveRDS(conn_mthd2_pm_prob, "data/prtgl_gravity_matrix2_pm.rds")



# For comparison of the predictions from different methods, can create absolute movement numbers 
# based on probabilities and population sizes.

# Method 1 Numbers

conn_method1_numbers <- conn_method1_probability * location_data$population
conn_method1_numbers <- as.movement_matrix(round(conn_method1_numbers))

# Method 2 Numbers

conn_method2_numbers <- conn_method2 * location_data$population
conn_method2_numbers <- as.movement_matrix(round(conn_method2_numbers))


heatmap(conn[1:50, 1:50], Colv = NA, Rowv = NA)
heatmap(conn_method1_numbers[1:50, 1:50], Colv = NA, Rowv = NA)
heatmap(conn_method2_numbers[1:50, 1:50], Colv = NA, Rowv = NA)



## Predicting adm1 flows with the adm2 gravity model
adm1_location_data <- readRDS("~/mobility_africa/mobility_africa2/data/portugal_adm1_location_data.rds")
adm1_norm_scld_matrix <- readRDS("~/mobility_africa/mobility_africa2/data/prtgl_adm1_norm_scld_matrix.rds")

gravity_adm1_predict <- predict(gravity_portugal, adm1_location_data, flux_model = gravity(), symmetric = FALSE)
adm1_conn_predict <- gravity_adm1_predict$movement_matrix


# Use national average of p_stay as diagonals
# Use the same p_stay in all patches

p_stay <- diag(conn) / rowSums(conn)
p_stay_avg <- mean(p_stay) # 0.296

adm1_conn_method2 <- (1 - p_stay_avg) * adm1_conn_predict / rowSums(adm1_conn_predict)
diag(adm1_conn_method2) <- p_stay_avg

# set the numbers moving in am and pm
adm1_conn_method2 <- adm1_conn_method2 * adm1_location_data$population
adm1_conn_method2_pm <- t(adm1_conn_method2)

# now rescale to probabilities
adm1_conn_mthd2_prob <- adm1_conn_method2 / rowSums(adm1_conn_method2)
adm1_conn_mthd2_pm_prob <- adm1_conn_method2_pm / rowSums(adm1_conn_method2_pm)

# convert to movement matrix objects
adm1_conn_mthd2_prob <- as.movement_matrix(adm1_conn_mthd2_prob)
adm1_conn_mthd2_pm_prob <- as.movement_matrix(adm1_conn_mthd2_pm_prob)

saveRDS(adm1_conn_mthd2_prob, "data/prtgl_adm1_gravity_matrix2.rds")
saveRDS(adm1_conn_mthd2_pm_prob, "data/prtgl_adm1_gravity_matrix2_pm.rds")




############
## FRANCE ##
############

fra_location_data <- readRDS("data/france_location_data_2007.rds")
fra_location_data <- as.location_dataframe(fra_location_data)

## generate predicted flows for France from gravity model fitted to Portugal data
gravity_france_predict <- predict(gravity_portugal, fra_location_data,
                                  flux_model = gravity(), symmetric = FALSE)
conn_france_predict <- gravity_france_predict$movement_matrix


gravity_france_predict_pm <- predict(gravity_portugal_pm, fra_location_data,
                                     flux_model = gravity(), symmetric = FALSE)
conn_predict_pm <- gravity_france_predict_pm$movement_matrix

# note these predicted flow matrices are the transpose of each other


## Gravity model does not predicting 'non-commuters'.
## Therefore we must infer diagonals for France movement where we imagine we do not have observed data.

## Method 2 (of the gravity predictions used in portugal)
# Use national average of p_stay in Portugal as the diagonals
# Use the same p_stay in all patches

p_stay <- diag(conn) / rowSums(conn)
p_stay_avg <- mean(p_stay) # 0.296

fra_conn_method2 <- (1 - p_stay_avg) * conn_france_predict / rowSums(conn_france_predict)
diag(fra_conn_method2) <- p_stay_avg

# set the numbers moving in am and pm
fra_conn_method2 <- fra_conn_method2 * fra_location_data$population
fra_conn_method2_pm <- t(fra_conn_method2)

# now rescale to probabilities
fra_conn_mthd2_prob <- fra_conn_method2 / rowSums(fra_conn_method2)
fra_conn_mthd2_pm_prob <- fra_conn_method2_pm / rowSums(fra_conn_method2_pm)

# convert to movement matrix objects
fra_conn_mthd2_prob <- as.movement_matrix(fra_conn_mthd2_prob)
fra_conn_mthd2_pm_prob <- as.movement_matrix(fra_conn_mthd2_pm_prob)

saveRDS(fra_conn_mthd2_prob, "data/fra_gravity_matrix2.rds")
saveRDS(fra_conn_mthd2_pm_prob, "data/fra_gravity_matrix2_pm.rds")




#####################################################
## Additional code for using other mobility models ##
#####################################################

# radiation_portugal_predict <- predict(radiation_portugal, location_data, flux_model = radiationWithSelection(), symmetric = FALSE)
# gravity_portugal_predict <- predict(gravity_portugal, location_data, flux_model = gravity(), symmetric = FALSE)
# gravity_distance_portugal_predict <- predict(gravity_distance_portugal, location_data, flux_model = gravityWithDistance(), symmetric = FALSE)
# u_selection_portugal_predict <- predict(u_selection_portugal, location_data, flux_model = uniformSelection(), symmetric = FALSE)



# Miscellaneous code for exploring the model predictions.
# TO DO: review this more systematically

# ## Explore the predictions
# ## Compare total outgoing flows with population of patch
# a <- cbind(location_data, pred_commuters = rowSums(conn_predict)) %>% 
#   mutate(too_big = ifelse(pred_commuters > population, 1, 0),
#          non_commuters = diag(conn))
# 
# sum(a$too_big)
# # 51 patches have outgoing flows that are greater than their population size...
# # how do we handle this? what is the denominator when we convert to probabilities?
# 
# sum(a$too_big[which(a$non_commuters == 0)])
# # 26 of the patches that are too big come from the patches with zero non-movers
# 
# hist(a$non_commuters[which(a$too_big == 1)])
# 
# hist(a$population[which(a$non_commuters == 0)])
# 
# hist(a$population)
# 
# 
# a$possible_total <- a$pred_commuters + a$non_commuters
# a$total_pcnt_diff <- 100 *(a$possible_total - a$population) / a$population
# 
# 
# ###NEXT: explore model fit
# # compare observed vs predicted
# # residuals vs predicted etc
# 
# diag(conn) <- 0
# conn <- as.data.frame(conn)
# conn$origin <- rownames(conn)
# 
# conn <- conn %>%
#   tidyr::pivot_longer(cols = !origin,
#                       names_to = "destination",
#                       values_to = "observed")
# 
# conn_predict <- as.data.frame(conn_predict)
# conn_predict$origin <- rownames(conn_predict)
# 
# conn_predict <- conn_predict %>%
#   tidyr::pivot_longer(cols = !origin,
#                       names_to = "destination",
#                       values_to = "predicted")
# 
# conn$predicted <- conn_predict$predicted
# conn$residual <- conn$predicted - conn$observed
# 
# conn$log_obs <- log(conn$observed)
# conn$log_pred <- log(conn$predicted)
# 
# ggplot(conn) +
#   geom_point(aes(x = observed, y = predicted)) +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   coord_fixed()
# 
# ggplot(conn, aes(x = log_obs, y = log_pred)) +
#   geom_point() +
#   # geom_smooth(method='lm', colour = "blue") +
#   geom_abline(intercept = 0, slope = 1, colour = "red") +
#   coord_fixed()
# 
# 
# ggplot(conn) +
#   geom_point(aes(x = predicted, y = residual)) +
#   geom_hline(yintercept = 0, colour = "red") +
#   coord_fixed()
# 
# ggplot(conn, aes(x = log_pred, y = residual)) +
#   geom_point() +
#   # geom_smooth(method='lm', colour = "blue") +
#   geom_hline(yintercept = 0, colour = "red")