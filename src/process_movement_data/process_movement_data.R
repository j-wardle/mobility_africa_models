## Read in raw mobile phone commuting data
## Tidy and convert to a form that is usable for models
#########################################################

locations <- c("portugal", "france")



# Load and prepare data ---------------------------------------------------


## Import raw commuting data
## Commuter data from https://doi.org/10.1371/journal.pcbi.1003716
## Non-commuter data provided by authors of the same study
## Message Jack to get access to commuter data

# Load required datasets

commuters <- map(locations, function(country) {
  read.delim(paste0("task_data/", country, "/od_matrix_mobilephones.txt")) %>%
    clean_names()
}
)
names(commuters) <- locations

non_commuters <- map(locations, function(country) {
  read.table(paste0("task_data/", country, "/selfloops.txt")) %>% 
    rename(origin = V1,
           dest = V2,
           commuters = V3)
}
)
names(non_commuters) <- locations

id_names <- map(locations, function(country) {
  read.table(paste0("task_data/", country, "/id_list.txt")) %>% 
    rename(id = V1,
           name = V2)
}
)
names(id_names) <- locations

location_data <- map(locations, function(country) {
  readRDS(paste0(country, "_location_data.rds"))
}
)
names(location_data) <- locations


## Some initial tidying of the data

# Re-fromat French id_names
id_names[["france"]]$name <- iconv(id_names[["france"]]$name, from = "UTF-8", to = "ASCII//TRANSLIT")
id_names[["france"]]$name <- gsub(" ", "_", toupper(id_names[["france"]]$name))

# French commuter data has an entry for people travelling from Bordeaux to Bordeaux. Delete.
commuters[["france"]] <- filter(commuters[["france"]], !(origin == 109 & dest == 109))


## Now convert to matrices

all_users_wide <- map(locations, function(country) {
  
  out <- bind_rows(commuters[[country]], non_commuters[[country]]) %>% 
    left_join(id_names[[country]],
              by = c("origin" = "id")) %>% 
    rename(origin_name = name) %>% 
    left_join(id_names[[country]],
              by = c("dest" = "id")) %>% 
    rename(dest_name = name) %>% 
    arrange(origin_name, dest_name)
  
  out <- pivot_wider(out,
              id_cols = origin_name,
              names_from = dest_name,
              values_from = commuters) %>% 
    select(origin_name, sort(names(.)))
  
  out
  
})
names(all_users_wide) <- locations



# Movement matrices from raw data -----------------------------------------



movement_matrix <- imap(all_users_wide, function(country, country_name) {
  
  out <- as.matrix(subset(country, select = -c(origin_name)))
  
  rownames(out) <- country$origin_name
  
  out <- out[
    which(rownames(out) %in% location_data[[country_name]][["location"]]),
    which(colnames(out) %in% location_data[[country_name]][["location"]])]
  
  out[is.na(out)] <- 0
  
  out
  
})

saveRDS(movement_matrix, "raw_matrix.rds")


## Get matrix of probabilities
norm_movement <- map(movement_matrix, function(country) {
  
  country / (rowSums(country))
  
})

saveRDS(norm_movement, "normalised_matrix.rds")


## Create corresponding 'evening' flows, ie return from work
eve_norm_movement <- map(movement_matrix, function(country) {
  
  out <- t(country)
  out <- out / (rowSums(out))
  
})

saveRDS(eve_norm_movement, "normalised_matrix_eve.rds")



# Movement matrices from scaling the raw data -----------------------------

## Since sampling rate across patches is not necessarily equal, we should scale the data

scaled_matrix <- imap(movement_matrix, function(country_matrix, country_name) {
  
  pop <- location_data[[country_name]]$population
  patch_scale_factor <- pop / rowSums(country_matrix)
  scaled_matrix <- country_matrix * patch_scale_factor
  
})

saveRDS(scaled_matrix, "scaled_matrix.rds")

## Convert to a matrix of probabilities

norm_scaled_movement <- map(scaled_matrix, function(country) {
  
  country / (rowSums(country))
  
})

saveRDS(norm_scaled_movement, "normalised_scaled_matrix.rds")

## Create corresponding 'evening' flows, ie return from work

eve_norm_scaled_movement <- map(scaled_matrix, function(country) {
  
  out <- t(country)
  out <- out / (rowSums(out))
  
})

saveRDS(eve_norm_scaled_movement, "normalised_scaled_matrix_eve.rds")




# Movement matrices for aggregated data -----------------------------------



## Here we aggregate the Portugal data from adm2 to adm1, and France data from adm3 to adm2.

adm_lookup <- map(locations, function(country) {
  readRDS(paste0(country, "_adm_lookup.rds"))
}
)
names(adm_lookup) <- locations

# Smallest Prtgl unit is adm2, whereas smallest France unit is adm3.
# Rename columns in adm lookup for consistency
adm_lookup$portugal <- rename(adm_lookup$portugal,
                              lrg_spatial_unit = n1,
                              sml_spatial_unit = n2)
adm_lookup$france <- rename(adm_lookup$france,
                              lrg_spatial_unit = n2,
                              sml_spatial_unit = n3)

# Aggregate the movement data
aggregated_movement <- imap(scaled_matrix, function(country_matrix, country_name) {
  
  long_movement <- as.data.frame(country_matrix)
  long_movement$origin <- rownames(long_movement)
  
  long_movement <- long_movement %>% 
    pivot_longer(cols = 1:(ncol(long_movement) - 1),
                 names_to = "dest",
                 values_to = "flow") %>% 
    select(origin, everything())
  
  adm_lookup[[country_name]] <- distinct(adm_lookup[[country_name]])
  
  aggregated_movement <- left_join(long_movement, adm_lookup[[country_name]],
                                   by = c("origin" = "sml_spatial_unit")) %>%
    rename(aggregate_origin = lrg_spatial_unit)
  aggregated_movement <- left_join(aggregated_movement, adm_lookup[[country_name]],
                                   by = c("dest" = "sml_spatial_unit")) %>%
    rename(aggregate_dest = lrg_spatial_unit)
  
  aggregated_movement <- aggregated_movement %>%
    group_by(aggregate_origin, aggregate_dest) %>%
    summarise(aggregate_flows = sum(flow)) %>%
    pivot_wider(id_cols = aggregate_origin,
                names_from = aggregate_dest,
                values_from = aggregate_flows) %>%
    select(aggregate_origin, sort(names(.)))

})


aggregated_movement_matrix <- map(aggregated_movement, function(country) {
  
  out <- as.matrix(subset(country,
                          select = -c(aggregate_origin)))
  rownames(out) <- country$aggregate_origin
  out
  
})


saveRDS(aggregated_movement_matrix, "aggregated_scaled_matrix.rds")


## Convert to a matrix of probabilities

aggr_norm_scaled_movement <- map(aggregated_movement_matrix, function(country_matrix) {
  
  country_matrix / (rowSums(country_matrix))
  
})

saveRDS(aggr_norm_scaled_movement, "aggregated_norm_scaled_matrix.rds")


## Create corresponding 'evening' flows, ie return from work

aggr_norm_scaled_movement_eve <- map(aggregated_movement_matrix, function(country_matrix) {
  
  out <- t(country_matrix)
  out <- out / rowSums(out)
  
})

saveRDS(aggr_norm_scaled_movement_eve, "aggregated_norm_scaled_matrix_eve.rds")