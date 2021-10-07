library(SpatialEpi)
library(rgdal)
library(broom)
library(ggplot2)
library(dplyr)

collated_time_to_first_case_path1 <- readRDS("~/mobility_africa/mobility_africa_models/draft/first_case_figures/20210902-182612-9463a800/collated_time_to_first_case_path1.rds")
# update filenames to extract from the proper source location (not the draft folders)
france_location_data <- readRDS("~/mobility_africa/mobility_africa_models/draft/first_case_figures/20210902-182612-9463a800/france_location_data.rds")
portugal_location_data <- readRDS("~/mobility_africa/mobility_africa_models/draft/first_case_figures/20210902-182612-9463a800/portugal_location_data.rds")

# Load in the gravity movement and raw movement predictions
gravity_move <- readRDS("~/mobility_africa/mobility_africa_models/draft/fit_mobility_models/20210903-103232-3984890f/gravity_matrix2_numbers.rds")
observed_move <- readRDS("~/mobility_africa/mobility_africa_models/draft/fit_mobility_models/20210903-103232-3984890f/scaled_matrix.rds")


setwd("C:/Users/jw2519/Documents/mobility_africa/data_for_centroids/France/")
france_spdf <- readOGR(dsn = "gadm36_FRA_3.shp", use_iconv = TRUE, encoding = "UTF-8")
summary(france_spdf)
head(france_spdf@data)
plot(france_spdf)

france_fortified <- tidy(france_spdf, region = "NAME_3")

ggplot() +
  geom_polygon(data = france_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 

# Re-add location names to first case timing data

fra_scen4 <- filter(collated_time_to_first_case_path1, model == "fra_g2_alt")
fra_raw <- filter(collated_time_to_first_case_path1, model == "fra_raw")
france_location_data$id <- 1:nrow(france_location_data)

fra_scen4 <- left_join(fra_scen4, france_location_data, by = c("patch" = "id")) %>% 
  select(!c(population, x, y))

fra_scen4$diff <- fra_scen4$mean - fra_raw$mean

fra_scen4 <- fra_scen4 %>% 
  group_by(seed) %>% 
  mutate(rel_diff = diff / max(mean),
         last_spread = max(mean))

# add info on movement from seed
observed_move_fra <- observed_move[["france"]]
gravity_move_fra <- gravity_move[["france_predict_adm3_alt"]]
# rows are origin, columns are destination

move_from_brest <- observed_move_fra["BREST", ]
move_from_paris <- observed_move_fra["PARIS", ]
move_from_seed <- c(move_from_brest, move_from_paris)
fra_scen4$flow_from_seed <- as.vector(move_from_seed)

gravity_from_brest <- gravity_move_fra["BREST", ]
gravity_from_paris <- gravity_move_fra["PARIS", ]
gravity_from_seed <- c(gravity_from_brest, gravity_from_paris)
fra_scen4$gravity_from_seed <- as.vector(gravity_from_seed)

fra_scen4$flow_diff <- fra_scen4$gravity_from_seed - fra_scen4$flow_from_seed


### Update the naming of the adm units in tidied spatial dataframe

france_fortified$id <- iconv(france_fortified$id, to = "ASCII//TRANSLIT")
france_fortified$id <- gsub(" ", "_", toupper(france_fortified$id))

# Tidy up naming of locations
france_fortified <- france_fortified %>%
  mutate(id = replace(id, id == "CHATEAU-CHINON_(VILLE)", "CHATEAU-CHINON(VILLE)"),
         id = stringr::str_replace(id, "LE_", "LE"),
         id = stringr::str_replace(id, "LA_", "LA"),
         id = stringr::str_replace(id, "LES_", "LES"))

france_fortified$id2 <- ifelse(startsWith(france_fortified$id, "PARIS,_"), "PARIS", france_fortified$id)


# Join the spatial dataframe to first timing dataframe

fra_scen4_map <- left_join(fra_scen4, france_fortified, by = c("location" = "id2"), keep = TRUE) %>% 
  select(-(id)) %>% 
  rename(id = id2)

fra_scen4_map %>% 
  filter(seed == "bre") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  coord_quickmap() +
  theme_void()

# Find the adm units that are missing from my plot
adm_data <- unique(fra_scen4$location)
adm_shp <- unique(france_fortified$id2)
setdiff(adm_shp, adm_data)
# [1] "AJACCIO"        "ARCACHON"       "BASTIA"         "CALVI"          "CORTE"          "FOUGERES-VITRE"
# [7] "LAC_LEMAN"      "SARTENE"   


# plot of times to first case in each area
time_to_spread_map_fra <-
  fra_scen4_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  facet_wrap(~seed) +
  ggtitle("Time to first case (using gravity model)") +
  theme_void()

time_to_spread_map_fra


# plot of the difference between model with gravity movement and that using raw movement data
time_diff_map_fra <-
  fra_scen4_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = diff), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  facet_wrap(~seed) +
  ggtitle("Time difference of first case (gravity model - raw data model)") +
  theme_void()

time_diff_map_fra


# plot of relative differences between model with gravity movement and that using raw movement data
rel_diff_map_fra <-
  fra_scen4_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff), color = "white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  facet_wrap(~seed) +
  ggtitle("Relative difference of first case (gravity model - raw data model)") +
  theme_void()

rel_diff_map_fra


fra_scen4_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff), color = "white", size = 0.1) +
  scale_fill_viridis_b() +
  coord_quickmap() +
  facet_wrap(~seed) +
  ggtitle("Relative difference of first case (gravity model - raw data model)") +
  theme_void()

# Measure time in weeks

fra_scen4_map$week <- fra_scen4_map$mean %/% 7

fra_scen4_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = week), color="white", size = 0.1) +
  scale_fill_viridis_b(n.breaks = 13) +
  facet_wrap(~seed, nrow = 1) +
  coord_quickmap() +
  ggtitle("Time difference of first case (gravity model - raw data model)") +
  theme_void()


## COMPARE TIME DIFFERENCES TO THE AMOUNT OF MOVEMENT IN PATCH

fra_scen4 %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  facet_wrap(~seed)


fra_scen4 %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = flow_diff/flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  facet_wrap(~seed)


fra_scen4 %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = gravity_from_seed/flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  facet_wrap(~seed)





## PORTUGAL MAPS

setwd("C:/Users/jw2519/Documents/mobility_africa/data_for_centroids/Portugal/")
port_spdf <- readOGR(dsn = "gadm36_PRT_2.shp", use_iconv = TRUE, encoding = "UTF-8")
summary(port_spdf)
head(port_spdf@data)
plot(port_spdf)


port_fortified <- tidy(port_spdf, region = "NAME_2")

ggplot() +
  geom_polygon(data = port_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 

# Re-add location names to first case timing data

port_scen4 <- filter(collated_time_to_first_case_path1, model == "prtl_g2_alt")
port_raw <- filter(collated_time_to_first_case_path1, model == "prtl_raw")
portugal_location_data$id <- 1:nrow(portugal_location_data)

port_scen4 <- left_join(port_scen4, portugal_location_data, by = c("patch" = "id")) %>% 
  select(!c(population, x, y))

port_scen4$diff <- port_scen4$mean - port_raw$mean

port_scen4 <- port_scen4 %>% 
  group_by(seed) %>% 
  mutate(rel_diff = diff / max(mean),
         last_spread = max(mean))

# add info on movement from seed
observed_move_prt <- observed_move[["portugal"]]
gravity_move_prt <- gravity_move[["portugal_predict_adm2_alt"]]
# rows are origin, columns are destination

move_from_mdd <- observed_move_prt["MIRANDA_DO_DOURO", ]
move_from_lsbn <- observed_move_prt["LISBOA", ]
move_from_seed_prt <- c(move_from_lsbn, move_from_mdd)
port_scen4$flow_from_seed <- as.vector(move_from_seed_prt)

gravity_from_mdd <- gravity_move_prt["MIRANDA_DO_DOURO", ]
gravity_from_lsbn <- gravity_move_prt["LISBOA", ]
gravity_from_seed_prt <- c(gravity_from_lsbn, gravity_from_mdd)
port_scen4$gravity_from_seed <- as.vector(gravity_from_seed_prt)

port_scen4$flow_diff <- port_scen4$gravity_from_seed - port_scen4$flow_from_seed

### Update the naming of the adm units in tidied spatial dataframe

port_fortified$id <- iconv(port_fortified$id, to = "ASCII//TRANSLIT")
port_fortified$id <- gsub(" ", "_", toupper(port_fortified$id))

# There are two duplicates

port_fortified <- port_fortified %>%
  filter(id %in% port_scen4$location) %>% 
  filter(!(id == "CALHETA")) %>%   # two municipalities on islands of Madeira and Azores
  filter(!(id == "LAGOA" & long < -25)) # remove municipality on Azores

# Join the spatial dataframe to first timing dataframe

port_scen4_map <- left_join(port_scen4, port_fortified, by = c("location" = "id"), keep = TRUE) 


# Create maps for different seeding locations

port_scen4_map %>% 
  filter(seed == "mdd") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  coord_quickmap() +
  theme_void()

# plot of times to first case in each area
time_to_spread_map_m <-
  port_scen4_map %>% 
  filter(seed == "mdd") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Time to first case (using gravity model)") +
  theme_void()

time_to_spread_map_m

time_to_spread_map_l <-
  port_scen4_map %>% 
  filter(seed == "lsbn") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  # ggtitle("Time to first case (using gravity model)") +
  theme_void()

time_to_spread_map_l

# plot of the difference between model with gravity movement and that using raw movement data
time_diff_map_m <-
  port_scen4_map %>% 
  filter(seed == "mdd") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = diff), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("Time difference of first case\n(gravity model - raw data model)") +
  theme_void()

time_diff_map_m


time_diff_map_l <-
  port_scen4_map %>% 
  filter(seed == "lsbn") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = diff), color="white", size = 0.1) +
  scale_fill_viridis_c() +
  coord_quickmap() +
  # ggtitle("Time difference of first case (gravity model - raw data model)") +
  theme_void()

time_diff_map_l
