## Load packages and function
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(purrr)

locations <- c("portugal", "france", "spain")


## Import raw data for Portugal
## Commuter data from https://doi.org/10.1371/journal.pcbi.1003716
## Non-commuter data accessed from authors of the same study
commuters <- map(locations, function(country) {
  read.delim(paste0("data/mobile_data/", country, "/od_matrix_mobilephones.txt")) %>%
    clean_names()
}
) 

non_commuters <- map(locations, function(country) {
  read.table(paste0("data/mobile_data/", country, "/selfloops.txt")) %>% 
    rename(origin = V1,
           dest = V2,
           commuters = V3)
}
)


id_names <- map(locations, function(country) {
  read.table(paste0("data/mobile_data/", country, "/id_list.txt")) %>% 
    rename(id = V1,
           name = V2)
}
)



locations <- readRDS("data/portugal_location_data.rds") # Portugal location data from weighted centroids

## Convert data to matrix
all_users <- bind_rows(commuters, non_commuters) %>% 
  left_join(id_names,
            by = c("origin" = "id")) %>% 
  rename(origin_name = name) %>% 
  left_join(id_names,
            by = c("dest" = "id")) %>% 
  rename(dest_name = name) %>% 
  arrange(origin_name, dest_name)

all_users_wide <- pivot_wider(all_users,
                              id_cols = origin_name,
                              names_from = dest_name,
                              values_from = commuters,
                              values_fn = length) %>% 
  select(origin_name, sort(names(.)))

movement_matrix <- as.matrix(subset(all_users_wide,
                                    select = -c(origin_name)))

rownames(movement_matrix) <- all_users_wide$origin_name

movement_matrix <- movement_matrix[which(rownames(movement_matrix) %in% locations[["location"]]),
                                   which(colnames(movement_matrix) %in% locations[["location"]])]

movement_matrix[is.na(movement_matrix)] <- 0

saveRDS(movement_matrix, "data/prtgl_raw_matrix.rds")


## Convert to a matrix of probabilities
norm_movement <- movement_matrix / (rowSums(movement_matrix))
saveRDS(norm_movement, "data/prtgl_norm_matrix.rds")


## Create corresponding 'evening' flows, ie return from work
eve_movement_matrix <- t(movement_matrix)
eve_norm_movement <- eve_movement_matrix / (rowSums(eve_movement_matrix))
saveRDS(eve_norm_movement, "data/prtgl_eve_norm_matrix.rds")



## Since sampling rate in each patch was not equal, we should scale the data

pop <- locations$pop
patch_scale_factor <- pop / rowSums(movement_matrix)
scaled_matrix <- movement_matrix * patch_scale_factor

saveRDS(scaled_matrix, "data/prtgl_scld_matrix.rds")

## Convert to a matrix of probabilities
norm_scaled_movement <- scaled_matrix / (rowSums(scaled_matrix))
saveRDS(norm_scaled_movement, "data/prtgl_norm_scld_matrix.rds")

## Create corresponding 'evening' flows, ie return from work
eve_scaled_matrix <- t(scaled_matrix)
eve_norm_scaled_movement <- eve_scaled_matrix / (rowSums(eve_scaled_matrix))
saveRDS(eve_norm_scaled_movement, "data/prtgl_eve_norm_scld_matrix.rds")


### Also aggregate movements from adm2 level to adm1

scaled_matrix <- readRDS("data/prtgl_scld_matrix.rds")
adm_lookup <- readRDS("data/adm_lookup.rds")

long_movement <- as.data.frame(scaled_matrix)
long_movement$origin <- rownames(long_movement)

long_movement <- long_movement %>% 
  pivot_longer(cols = ABRANTES:VOUZELA,
               names_to = "dest",
               values_to = "flow") %>% 
  select(origin, everything())

adm1_movement <- left_join(long_movement, adm_lookup, by = c("origin" = "adm2")) %>%
  rename(adm1_origin = adm1)
adm1_movement <- left_join(adm1_movement, adm_lookup, by = c("dest" = "adm2")) %>%
  rename(adm1_dest = adm1)

adm1_movement <- adm1_movement %>% 
  group_by(adm1_origin, adm1_dest) %>% 
  summarise(adm1_flows = sum(flow))


adm1_movement_wide <- pivot_wider(adm1_movement,
                                  id_cols = adm1_origin,
                                  names_from = adm1_dest,
                                  values_from = adm1_flows) %>% 
  select(adm1_origin, sort(names(.)))


adm1_movement_matrix <- as.matrix(subset(adm1_movement_wide,
                                         select = -c(adm1_origin)))

rownames(adm1_movement_matrix) <- adm1_movement_wide$adm1_origin

saveRDS(adm1_movement_matrix, "data/prtgl_adm1_scld_matrix.rds")


## Convert to a matrix of probabilities
norm_scaled_adm1_matrix <- adm1_movement_matrix / (rowSums(adm1_movement_matrix))
saveRDS(norm_scaled_adm1_matrix, "data/prtgl_adm1_norm_scld_matrix.rds")

## Create corresponding 'evening' flows, ie return from work
eve_adm1_movement_matrix <- t(adm1_movement_matrix)
eve_norm_scaled_adm1_matrix <- eve_adm1_movement_matrix / (rowSums(eve_adm1_movement_matrix))
saveRDS(eve_norm_scaled_adm1_matrix, "data/prtgl_adm1_eve_norm_scld_matrix.rds")








#######################################
## Some plots to explore distribution of data
#######################################

ggplot(as.data.frame(patch_scale_factor)) +
  geom_histogram(aes(x = patch_scale_factor))


ggplot(as.data.frame(diag(norm_scaled_movement))) +
  geom_histogram(aes(x = `diag(norm_scaled_movement)`)) +
  xlab("Probability of staying in patch")


## Create dataframe with populations, scale_factors and non-commuting numbers

exploring_data <- as.data.frame(cbind(patch_scale_factor, pop, diag(movement_matrix))) %>% 
  rename(matrix_diag = V3)

ggplot(exploring_data) +
  geom_point(aes(x = pop, y = patch_scale_factor))

ggplot(exploring_data) +
  geom_point(aes(x = pop, y = patch_scale_factor)) +
  xlim(c(0, 150000))

## Look at patches where no raw data on numbers staying 
exploring_data %>% 
  filter(matrix_diag == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = patch_scale_factor))

exploring_data %>% 
  filter(matrix_diag == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pop))




##### FRANCE

france_location_data <- readRDS("data/france_location_data.rds")

# select france data

commuters <- commuters[[2]]
non_commuters <- non_commuters[[2]]
id_names <- id_names[[2]]
location_data <- france_location_data

id_names$name <- iconv(id_names$name, from = "UTF-8", to = "ASCII//TRANSLIT")
id_names$name <- gsub(" ", "_", toupper(id_names$name))

# commuter data has an entry for people travelling from bordeaux to bordeaux. delete this

commuters <- filter(commuters, !(origin == 109 & dest == 109))


## Convert data to matrix
all_users <- bind_rows(commuters, non_commuters) %>% 
  left_join(id_names,
            by = c("origin" = "id")) %>% 
  rename(origin_name = name) %>% 
  left_join(id_names,
            by = c("dest" = "id")) %>% 
  rename(dest_name = name) %>% 
  arrange(origin_name, dest_name)

all_users_wide <- pivot_wider(all_users,
                              id_cols = origin_name,
                              names_from = dest_name,
                              values_from = commuters) %>% 
  select(origin_name, sort(names(.)))

movement_matrix <- as.matrix(subset(all_users_wide,
                                    select = -c(origin_name)))

rownames(movement_matrix) <- all_users_wide$origin_name

movement_matrix <- movement_matrix[which(rownames(movement_matrix) %in% location_data[["location"]]),
                                   which(colnames(movement_matrix) %in% location_data[["location"]])]

movement_matrix[is.na(movement_matrix)] <- 0

saveRDS(movement_matrix, "data/fra_raw_matrix.rds")


## Convert to a matrix of probabilities
norm_movement <- movement_matrix / (rowSums(movement_matrix))
saveRDS(norm_movement, "data/fra_norm_matrix.rds")


## Create corresponding 'evening' flows, ie return from work
eve_movement_matrix <- t(movement_matrix)
eve_norm_movement <- eve_movement_matrix / (rowSums(eve_movement_matrix))
saveRDS(eve_norm_movement, "data/fra_eve_norm_matrix.rds")



## Since sampling rate in each patch was not equal, we should scale the data

pop <- location_data$pop
patch_scale_factor <- pop / rowSums(movement_matrix)
scaled_matrix <- movement_matrix * patch_scale_factor

saveRDS(scaled_matrix, "data/fra_scld_matrix.rds")

## Convert to a matrix of probabilities
norm_scaled_movement <- scaled_matrix / (rowSums(scaled_matrix))
saveRDS(norm_scaled_movement, "data/fra_norm_scld_matrix.rds")

## Create corresponding 'evening' flows, ie return from work
eve_scaled_matrix <- t(scaled_matrix)
eve_norm_scaled_movement <- eve_scaled_matrix / (rowSums(eve_scaled_matrix))
saveRDS(eve_norm_scaled_movement, "data/fra_eve_norm_scld_matrix.rds")


#################################
## Repeat for 2007 France data ##
#################################

france_location_data <- readRDS("data/france_location_data_2007.rds")

# select france data

commuters <- commuters[[2]]
non_commuters <- non_commuters[[2]]
id_names <- id_names[[2]]
location_data <- france_location_data

id_names$name <- iconv(id_names$name, from = "UTF-8", to = "ASCII//TRANSLIT")
id_names$name <- gsub(" ", "_", toupper(id_names$name))

# commuter data has an entry for people travelling from bordeaux to bordeaux. delete this

commuters <- filter(commuters, !(origin == 109 & dest == 109))


## Convert data to matrix
all_users <- bind_rows(commuters, non_commuters) %>% 
  left_join(id_names,
            by = c("origin" = "id")) %>% 
  rename(origin_name = name) %>% 
  left_join(id_names,
            by = c("dest" = "id")) %>% 
  rename(dest_name = name) %>% 
  arrange(origin_name, dest_name)

all_users_wide <- pivot_wider(all_users,
                              id_cols = origin_name,
                              names_from = dest_name,
                              values_from = commuters) %>% 
  select(origin_name, sort(names(.)))

movement_matrix <- as.matrix(subset(all_users_wide,
                                    select = -c(origin_name)))

rownames(movement_matrix) <- all_users_wide$origin_name

movement_matrix <- movement_matrix[which(rownames(movement_matrix) %in% location_data[["location"]]),
                                   which(colnames(movement_matrix) %in% location_data[["location"]])]

movement_matrix[is.na(movement_matrix)] <- 0

saveRDS(movement_matrix, "data/fra2007_raw_matrix.rds")


## Convert to a matrix of probabilities
norm_movement <- movement_matrix / (rowSums(movement_matrix))
saveRDS(norm_movement, "data/fra2007_norm_matrix.rds")


## Create corresponding 'evening' flows, ie return from work
eve_movement_matrix <- t(movement_matrix)
eve_norm_movement <- eve_movement_matrix / (rowSums(eve_movement_matrix))
saveRDS(eve_norm_movement, "data/fra2007_eve_norm_matrix.rds")



## Since sampling rate in each patch was not equal, we should scale the data

pop <- location_data$population
patch_scale_factor <- pop / rowSums(movement_matrix)
scaled_matrix <- movement_matrix * patch_scale_factor

saveRDS(scaled_matrix, "data/fra2007_scld_matrix.rds")

## Convert to a matrix of probabilities
norm_scaled_movement <- scaled_matrix / (rowSums(scaled_matrix))
saveRDS(norm_scaled_movement, "data/fra2007_norm_scld_matrix.rds")

## Create corresponding 'evening' flows, ie return from work
eve_scaled_matrix <- t(scaled_matrix)
eve_norm_scaled_movement <- eve_scaled_matrix / (rowSums(eve_scaled_matrix))
saveRDS(eve_norm_scaled_movement, "data/fra2007_eve_norm_scld_matrix.rds")
