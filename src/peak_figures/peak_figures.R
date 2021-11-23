if (! is.null(dev.list())) dev.off()

## Create folder to save figures
dir.create("figures")

## Load metric data

time_to_peak_path1 <- readRDS("collated_time_to_peak_path1.rds")
time_to_peak_path2 <- readRDS("collated_time_to_peak_path2.rds")

time_to_peak <- bind_rows(time_to_peak_path1, time_to_peak_path2,
                                .id = "pathogen")

## Load location data
portugal_location_data <- readRDS("portugal_location_data.rds")
france_location_data<- readRDS("france_location_data.rds")


# TIME TO PEAK PROCESSING ----------------------------------------------

# extract the country names and model type to be more clear
# https://stackoverflow.com/questions/12297859/remove-all-text-before-colon

time_to_peak <- time_to_peak %>% 
  mutate(country = word(model, 1, sep ="\\_"),
         model = regmatches(model,
                            gregexpr("(?<=_).*", model, perl=TRUE)
         )
  )

time_to_peak$model <- as.character(time_to_peak$model)
time_to_peak <- time_to_peak %>% 
  arrange(country, model)

time_to_peak <- time_to_peak %>% 
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

time_to_peak <- time_to_peak %>% 
  filter(model == model1 | model == model2)


# Filter based on country

if (country == "france") {
  
  time_to_peak <- time_to_peak %>% 
    filter(seed == "BREST" | seed == "PARIS")
  
  location_names <- rep(france_location_data$location, 8)
  
}

if (country == "portugal") {
  
  time_to_peak <- time_to_peak %>% 
    filter(seed == "MIRANDA_DO_DOURO" | seed == "LISBOA")
  
  location_names <- rep(portugal_location_data$location, 8)
}


# add location names

time_to_peak$patch_name <- location_names


# Join location coordinates for patch and relevant seed location

france_location_data$country <- "FRANCE"
portugal_location_data$country <- "PORTUGAL"
location_data <- bind_rows(france_location_data, portugal_location_data)

peak <- left_join(time_to_peak, location_data,
                         by = c("patch_name" = "location")) %>% 
  select(-population) %>% 
  rename(patch_x = x,
         patch_y = y)

peak <- left_join(peak, location_data,
                         by = c("seed" = "location")) %>% 
  select(-population) %>% 
  rename(seed_x = x,
         seed_y = y)

## Calculate distances from seed to patch

peak <- peak %>% 
  rowwise() %>%
  mutate(distance = geosphere::distm(c(patch_x, patch_y), c(seed_x, seed_y)) / 1000
  )

peak <- peak %>%
  group_by(seed) %>%
  mutate(scaled_distance = distance / max(distance))

## Tidy variable names

peak <- rename(peak, median = `0.5`)
peak$model <- as.factor(peak$model)
peak$seed <- factor(peak$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))
peak$`95%CrI` <- peak$`0.975` - peak$`0.025`
peak$standardised_variation <- peak$`95%CrI` / peak$median


### Create scatter plot
###x-axis : time to first case with observed mobility data
### y-axis : time to first case with predicted mobility data

peak_scatter <-
  peak %>% 
  pivot_wider(id_cols = c(patch, seed, model, pathogen, scaled_distance),
              names_from = model,
              values_from = median) %>% 
  filter(pathogen == 1) %>% 
  ggplot() +
  geom_point(aes_string(x = model1, y = model2, colour = "scaled_distance"), size = 0.8) +
  scale_colour_viridis_c(name = "Scaled distance") +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to peak using\nobserved mobility (days)") +
  ylab("Time to peak using predicted mobility (days)") +
  scale_x_continuous(limits = c(120, 220),
                     breaks = seq(120, 220, 20)) +
  scale_y_continuous(limits = c(120, 220),
                     breaks = seq(120, 220, 20)) +
  coord_fixed() +
  theme_classic() +
  # facet_grid(pathogen ~ seed) +
  facet_wrap(~ seed, nrow = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 7),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  stat_cor(aes_string(x = model1, y = model2, label = "..rr.label.."),
           color = "red", geom = "text", label.x = 200, label.y = 130, size = 3)

ggsave("figures/peak_scatter.png", peak_scatter,
       height = 7.4, units = "in")

knitr::plot_crop("figures/peak_scatter.png")

if (! is.null(dev.list())) dev.off()