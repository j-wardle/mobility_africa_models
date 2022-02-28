if (! is.null(dev.list())) dev.off()

time_to_first_case <- readRDS("collated_time_to_first_case_path1.rds")

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

# TIME TO FIRST CASE PROCESSING ----------------------------------------------

# extract the country names and model type to be more clear
# https://stackoverflow.com/questions/12297859/remove-all-text-before-colon

time_to_first_case <- time_to_first_case %>% 
  mutate(country = word(model, 1, sep ="\\_"),
         model = regmatches(model,
                            gregexpr("(?<=_).*", model, perl=TRUE)
         )
  )

time_to_first_case$model <- as.character(time_to_first_case$model)
time_to_first_case <- time_to_first_case %>% 
  arrange(country, model)

time_to_first_case <- time_to_first_case %>% 
  mutate(seed = replace(seed, seed == "bre", "BREST"),
         seed = replace(seed, seed == "prs", "PARIS"),
         seed = replace(seed, seed == "mdd", "MIRANDA_DO_DOURO"),
         seed = replace(seed, seed == "lsbn", "LISBOA"),
         country = replace(country, country == "fra", "france"),
         country = replace(country, country == "prtl", "portugal"),)

if (scenario_number == 4) {
  
  model1 <- "raw"
  model2 <- "g2_alt"
  
}

if (scenario_number == 1) {
  
  model1 <- "raw"
  model2 <- "g1"
  
}

if (scenario_number == 3) {
  
  model1 <- "raw"
  model2 <- "g2"
  
}

if (scenario_number == 2) {
  
  model1 <- "raw"
  model2 <- "g1_alt"
  
}

if (scenario_number == 5) {
  
  model1 <- "raw"
  model2 <- "r2"
  
}

# Re-add location names to first case timing data

fra_scenario <- filter(time_to_first_case, model == model2 & country == "france")
fra_raw <- filter(time_to_first_case, model == model1 & country == "france")
france_location_data$id <- 1:nrow(france_location_data)

fra_scenario <- left_join(fra_scenario, france_location_data, by = c("patch" = "id")) %>% 
  select(!c(population, x, y))

fra_scenario$diff <- fra_scenario$mean - fra_raw$mean

fra_scenario <- fra_scenario %>% 
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
fra_scenario$flow_from_seed <- as.vector(move_from_seed)

gravity_from_brest <- gravity_move_fra["BREST", ]
gravity_from_paris <- gravity_move_fra["PARIS", ]
gravity_from_seed <- c(gravity_from_brest, gravity_from_paris)
fra_scenario$gravity_from_seed <- as.vector(gravity_from_seed)

fra_scenario$flow_diff <- fra_scenario$gravity_from_seed - fra_scenario$flow_from_seed


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

fra_scenario_map <- left_join(fra_scenario, france_fortified, by = c("location" = "id2"), keep = TRUE) %>% 
  select(-(id)) %>% 
  rename(id = id2)

fra_scenario_map %>% 
  filter(seed == "bre") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  coord_quickmap() +
  theme_void()

# Find the adm units that are missing from my plot
adm_data <- unique(fra_scenario$location)
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

port_scenario <- filter(time_to_first_case, model == model2 & country == "portugal")
port_raw <- filter(time_to_first_case, model == model1 & country == "portugal")
portugal_location_data$id <- 1:nrow(portugal_location_data)

port_scenario <- left_join(port_scenario, portugal_location_data, by = c("patch" = "id")) %>% 
  select(!c(population, x, y))

port_scenario$diff <- port_scenario$mean - port_raw$mean

port_scenario <- port_scenario %>% 
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
port_scenario$flow_from_seed <- as.vector(move_from_seed_prt)

gravity_from_mdd <- gravity_move_prt["MIRANDA_DO_DOURO", ]
gravity_from_lsbn <- gravity_move_prt["LISBOA", ]
gravity_from_seed_prt <- c(gravity_from_lsbn, gravity_from_mdd)
port_scenario$gravity_from_seed <- as.vector(gravity_from_seed_prt)

port_scenario$flow_diff <- port_scenario$gravity_from_seed - port_scenario$flow_from_seed

### Update the naming of the adm units in tidied spatial dataframe

port_fortified$id <- iconv(port_fortified$id, to = "ASCII//TRANSLIT")
port_fortified$id <- gsub(" ", "_", toupper(port_fortified$id))

# There are two duplicates

port_fortified <- port_fortified %>%
  filter(id %in% port_scenario$location) %>% 
  filter(!(id == "CALHETA")) %>%   # two municipalities on islands of Madeira and Azores
  filter(!(id == "LAGOA" & long < -25)) # remove municipality on Azores

# Join the spatial dataframe to first timing dataframe

port_scenario_map <- left_join(port_scenario, port_fortified, by = c("location" = "id"), keep = TRUE) 



# PLOT FRANCE AND PORTUGAL TOGETHER (SAME SCALE)

both_scen_map <- bind_rows(fra_scenario_map, port_scenario_map)

both_scen_map <- both_scen_map %>%
  mutate(rel_diff_bins = cut(rel_diff,
                             breaks = seq(0.5, -0.5, -0.1),
                             right = FALSE))

both_scen_map$rel_diff_bins <- fct_rev(both_scen_map$rel_diff_bins)

# levels(both_scen_map$rel_diff_bins) <- c("0.4 < d <= 0.5", "0.3 < d <= 0.4",
#                                           "0.2 < d <= 0.3", "0.1 < d <= 0.2",
#                                           "0.0 < d <= 0.1", "-0.1 < d <= 0.0",
#                                           "-0.2 < d <= -0.1", "-0.3 < d <= -0.2",
#                                           "-0.4 < d <= -0.3", "-0.5 < d <= -0.4")

# levels(both_scen_map$rel_diff_bins) <- c("0.4 < d \u2264 0.5 (slower spread)", "0.3 < d \u2264 0.4",
#                                           "0.2 < d \u2264 0.3", "0.1 < d \u2264 0.2",
#                                           "0.0 < d \u2264 0.1", "-0.1 < d \u2264 0.0",
#                                           "-0.2 < d \u2264 -0.1", "-0.3 < d \u2264 -0.2",
#                                           "-0.4 < d \u2264 -0.3", "-0.5 < d \u2264 -0.4 (faster spread)")

levels(both_scen_map$rel_diff_bins) <- c("0.4 < d \u2264 0.5", "0.3 < d \u2264 0.4",
                                          "0.2 < d \u2264 0.3", "0.1 < d \u2264 0.2",
                                          "0.0 < d \u2264 0.1", "-0.1 < d \u2264 0.0",
                                          "-0.2 < d \u2264 -0.1", "-0.3 < d \u2264 -0.2",
                                          "-0.4 < d \u2264 -0.3", "-0.5 < d \u2264 -0.4")


both_scen_map$rel_diff_bins <- fct_rev(both_scen_map$rel_diff_bins)


both_scen_map$seed <- as.factor(both_scen_map$seed)
both_scen_map$seed <- factor(both_scen_map$seed, 
                              levels = c("BREST", "PARIS", "MIRANDA_DO_DOURO", "LISBOA"),
                              labels = c("Brest", "Paris", "Miranda do Douro", "Lisboa"))



both_scen_map <- both_scen_map %>% 
  mutate(seed_long = case_when(
    seed == "Brest" ~ -4.48,
    seed == "Paris" ~ 2.34,
    seed == "Lisboa" ~ -9.15,
    seed == "Miranda do Douro" ~ -6.27),
    seed_lat = case_when(
      seed == "Brest" ~ 48.41,
      seed == "Paris" ~ 48.86,
      seed == "Lisboa" ~ 38.75,
      seed == "Miranda do Douro" ~ 41.49
    ))


plot_france_same_scale <- both_scen_map %>%
  filter(seed == "Brest" | seed == "Paris") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff_bins), color = "white", size = 0.1) +
  geom_point(aes(x = seed_long, y = seed_lat), color = "red", fill = "red",
             shape = 24) +
  scale_fill_brewer(palette = "RdBu", drop = "FALSE",
                    direction = -1,
                    name = "Relative difference (d)") +
  xlab("Relative error in\ninvasion time") +
  ylab("Time to first case using predicted mobility (days)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  # theme_void() +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    text = element_text(size = 11),
    axis.text = element_text(size = 14, colour = "white"),
    axis.title.x = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "white"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "none" #,
    # legend.direction = "horizontal",
    # plot.margin=grid::unit(c(0,0,0,0), "mm")
  )

plot_france_same_scale

ggsave("figures/fra_map_rel_diff.png", plot_france_same_scale,
       width = 6, height = 6, units = "in", dpi = 150)

discrete_legend <- both_scen_map %>%
  filter(seed == "Brest" | seed == "Paris") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff_bins), color = "white", size = 0.1) +
  scale_fill_brewer(palette = "RdBu", drop = "FALSE", direction = -1,
                    name = "Relative difference (d)",
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top",
                      label.position = "bottom",
                      label.hjust = 0.5,
                      label.vjust = 0.5,
                      label.theme = element_text(angle = 80),
                      nrow = 1
                    )) +
  xlab("Dummy text\ndummy text") +
  ylab("Time to first case using predicted mobility (days)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  # theme_void() +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    # text = element_text(size = 11),
    axis.text = element_text(size = 14, colour = "white"),
    axis.title = element_text(size = 16, colour = "white"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    # legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom" ,
    legend.direction = "horizontal"
  )

ggsave("figures/discrete_legend.png", discrete_legend,
       width = 6, height = 6, units = "in", dpi = 150)

# Create a continuous legend to be extracted

cont_legend <- both_scen_map
cont_legend$dummy_legend_scale <- runif(nrow(cont_legend), min = -0.5, max = 0.5)
cont_legend$dummy_legend_scale <- round(cont_legend$dummy_legend_scale, 2)
  
continuous_legend <- cont_legend %>%
  filter(seed == "Brest" | seed == "Paris") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = dummy_legend_scale), color = "white", size = 0.1) +
  scale_fill_distiller(palette = "RdBu", direction = -1,
                       name = "Relative difference"#,
                       # guide = guide_colourbar(
                       #   direction = "horizontal",
                       #   title.position = "top",
                       #   label.position = "bottom",
                       #   label.hjust = 0.5,
                       #   label.vjust = 0.5,
                       #   label.theme = element_text(angle = 80),
                       #   nrow = 1
                       # )
  ) +
  xlab("Dummy text\ndummy text") +
  ylab("Time to first case using predicted mobility (days)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  # theme_void() +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    # text = element_text(size = 11),
    axis.text = element_text(size = 14, colour = "white"),
    axis.title = element_text(size = 16, colour = "white"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    # legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom" ,
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))
  
ggsave("figures/continuous_legend.png", continuous_legend,
       width = 6, height = 6, units = "in", dpi = 150)


# knitr::plot_crop("figures/fra_map_rel_diff.png")

plot_portugal_same_scale <- both_scen_map %>%
  filter(seed == "Miranda do Douro" | seed == "Lisboa") %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = rel_diff_bins), color = "white", size = 0.1) +
  geom_point(aes(x = seed_long, y = seed_lat), color = "red", fill = "red",
             shape = 24) +
  scale_fill_brewer(palette = "RdBu", drop = "FALSE",
                    direction = -1,
                    name = "Relative difference (d)") +
  xlab("Relative error in\ninvasion time") +
  ylab("Time to first case using predicted mobility (days)") +
  coord_quickmap() +
  facet_wrap(~seed, nrow = 2) +
  # ggtitle("Relative difference of first case (gravity model - raw data model)") +
  # theme_void() +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    text = element_text(size = 11),
    axis.text = element_text(size = 14, colour = "white"),
    axis.title.x = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "white"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "none" #,
    # legend.direction = "horizontal",
    # plot.margin=grid::unit(c(0,0,0,0), "mm")
  )

ggsave("figures/prt_map_rel_diff.png", plot_portugal_same_scale,
       width = 6, height = 6, units = "in", dpi = 150)

# knitr::plot_crop("figures/prt_map_rel_diff.png")

p <- plot_france_same_scale + plot_portugal_same_scale +
  plot_layout(guides = 'collect')

ggsave("figures/fra_prt_joint_map_rel_diff.png", p)






## COMPARE TIME DIFFERENCES TO THE AMOUNT OF MOVEMENT IN PATCH

fra_scenario %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  facet_wrap(~seed)


fra_scenario %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = flow_diff/flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  facet_wrap(~seed)


fra_scenario %>%
  filter(!(seed == "bre" & location == "BREST"),
         !(seed == "prs" & location == "PARIS")) %>% 
  ggplot(aes(x = rel_diff, y = gravity_from_seed/flow_from_seed)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  # scale_y_log10() +
  facet_wrap(~seed)

if (! is.null(dev.list())) dev.off()