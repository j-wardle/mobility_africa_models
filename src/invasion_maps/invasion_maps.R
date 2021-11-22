if (! is.null(dev.list())) dev.off()

collated_time_to_first_case_path1 <- readRDS("collated_time_to_first_case_path1.rds")

portugal_location_data <- readRDS("portugal_location_data.rds")
france_location_data <- readRDS("france_location_data.rds")

# Load in the gravity movement and raw movement predictions
gravity_move <- readRDS("gravity_matrix2_numbers.rds")
observed_move <- readRDS("scaled_matrix.rds")

# Load France shapefile
france_spdf <- readOGR(dsn = "task_data/gadm36_FRA_3.shp",
                       use_iconv = TRUE, encoding = "UTF-8")

# path <- getwd()
# france_spdf <- read_sf(dsn = paste0(path, "/task_data/"), layer = "gadm36_FRA_3.shp",
#                        use_iconv = TRUE, encoding = "UTF-8")
summary(france_spdf)
head(france_spdf@data)
plot(france_spdf)

france_fortified <- tidy(france_spdf, region = "NAME_3")

# ggplot() +
#   geom_polygon(data = france_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void() 

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
         id = replace(id, id == "ARCACHON", "BORDEAUX"),
         id = replace(id, id == "FOUGERES-VITRE", "FOUGERES"),
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



## PORTUGAL MAPS

port_spdf <- readOGR("task_data/gadm36_PRT_2.shp", use_iconv = TRUE, encoding = "UTF-8")
summary(port_spdf)
head(port_spdf@data)
plot(port_spdf)

port_fortified <- tidy(port_spdf, region = "NAME_2")

# ggplot() +
#   geom_polygon(data = port_fortified, aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void() 

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



# PLOT FRANCE AND PORTUGAL TOGETHER (SAME SCALE)

both_scen4_map <- bind_rows(fra_scen4_map, port_scen4_map)

both_scen4_map <- both_scen4_map %>%
  mutate(rel_diff_bins = cut(rel_diff,
                             breaks = seq(0.5, -0.5, -0.1),
                             right = FALSE))

both_scen4_map$rel_diff_bins <- fct_rev(both_scen4_map$rel_diff_bins)

# levels(both_scen4_map$rel_diff_bins) <- c("0.4 < d <= 0.5", "0.3 < d <= 0.4",
#                                           "0.2 < d <= 0.3", "0.1 < d <= 0.2",
#                                           "0.0 < d <= 0.1", "-0.1 < d <= 0.0",
#                                           "-0.2 < d <= -0.1", "-0.3 < d <= -0.2",
#                                           "-0.4 < d <= -0.3", "-0.5 < d <= -0.4")

levels(both_scen4_map$rel_diff_bins) <- c("0.4 < d \u2264 0.5", "0.3 < d \u2264 0.4",
                                          "0.2 < d \u2264 0.3", "0.1 < d \u2264 0.2",
                                          "0.0 < d \u2264 0.1", "-0.1 < d \u2264 0.0",
                                          "-0.2 < d \u2264 -0.1", "-0.3 < d \u2264 -0.2",
                                          "-0.4 < d \u2264 -0.3", "-0.5 < d \u2264 -0.4")

both_scen4_map$seed <- as.factor(both_scen4_map$seed)
both_scen4_map$seed <- factor(both_scen4_map$seed, 
                              levels = c("bre", "prs", "mdd", "lsbn"),
                              labels = c("Brest", "Paris", "Miranda do Douro", "Lisboa"))


plot_france_same_scale <- both_scen4_map %>%
  filter(seed == "Brest" | seed == "Paris") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff_bins), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdBu", drop = "FALSE", name = "Relative difference (d)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  theme_void() +
  theme(
    text = element_text(size = 9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    plot.margin=grid::unit(c(0,0,0,0), "mm")
  )

ggsave("figures/fra_map_rel_diff.png", plot_france_same_scale)

knitr::plot_crop("figures/fra_map_rel_diff.png")

plot_portugal_same_scale <- both_scen4_map %>%
  filter(seed == "Miranda do Douro" | seed == "Lisboa") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff_bins), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdBu", drop = "FALSE", name = "Relative difference (d)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  theme_void() +
  theme(
    text = element_text(size = 9),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    plot.margin=grid::unit(c(0,0,0,0), "mm")
  )

ggsave("figures/prt_map_rel_diff.png", plot_portugal_same_scale)

knitr::plot_crop("figures/prt_map_rel_diff.png")

p <- plot_france_same_scale + plot_portugal_same_scale +
  plot_layout(guides = 'collect')

ggsave("figures/fra_prt_joint_map_rel_diff.png", p)






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

if (! is.null(dev.list())) dev.off()