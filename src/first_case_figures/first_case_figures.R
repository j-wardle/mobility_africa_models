if (! is.null(dev.list())) dev.off()

## Create folder to save figures
dir.create("figures")

## Load metric data

time_to_first_case_path1 <- readRDS("collated_time_to_first_case_path1.rds")
time_to_first_case_path2 <- readRDS("collated_time_to_first_case_path2.rds")

time_to_first_case <- bind_rows(time_to_first_case_path1, time_to_first_case_path2,
                                .id = "pathogen")

## Load location data
portugal_location_data <- readRDS("portugal_location_data.rds")
france_location_data<- readRDS("france_location_data.rds")


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

time_to_first_case <- time_to_first_case %>% 
  filter(model == model1 | model == model2)


# Filter based on country

if (country == "france") {

time_to_first_case <- time_to_first_case %>% 
  filter(seed == "BREST" | seed == "PARIS")

location_names <- rep(france_location_data$location, 8)

}

if (country == "portugal") {
  
  time_to_first_case <- time_to_first_case %>% 
    filter(seed == "MIRANDA_DO_DOURO" | seed == "LISBOA")
  
  location_names <- rep(portugal_location_data$location, 8)
}

# add location names

time_to_first_case$patch_name <- location_names

# Join location coordinates for patch and relevant seed location

france_location_data$country <- "FRANCE"
portugal_location_data$country <- "PORTUGAL"
location_data <- bind_rows(france_location_data, portugal_location_data)

first_cases <- left_join(time_to_first_case, location_data,
                         by = c("patch_name" = "location")) %>% 
  select(-population) %>% 
  rename(patch_x = x,
         patch_y = y)

first_cases <- left_join(first_cases, location_data,
                         by = c("seed" = "location")) %>% 
  select(-population) %>% 
  rename(seed_x = x,
         seed_y = y)

## Caluclate distances from seed to patch

first_cases <- first_cases %>% 
  rowwise() %>%
  mutate(distance = geosphere::distm(c(patch_x, patch_y), c(seed_x, seed_y)) / 1000
  )

first_cases <- first_cases %>%
  group_by(seed) %>%
  mutate(scaled_distance = distance / max(distance))

## Tidy variable names

first_cases <- rename(first_cases, median = `0.5`)
first_cases$model <- as.factor(first_cases$model)
first_cases$seed <- factor(first_cases$seed,
                           levels = c("BREST", "PARIS", "MIRANDA_DO_DOURO", "LISBOA"),
                           labels = c("Brest", "Paris", "Miranda do Douro", "Lisboa"))
first_cases$`95%CrI` <- first_cases$`0.975` - first_cases$`0.025`
first_cases$standardised_variation <- first_cases$`95%CrI` / first_cases$median


### Create scatter plot
###x-axis : time to first case with observed mobility data
### y-axis : time to first case with predicted mobility data

first_cases_plot <- first_cases %>% 
  pivot_wider(id_cols = c(patch, seed, model, pathogen, scaled_distance),
              names_from = model,
              values_from = median) %>% 
  filter(pathogen == 1)


first_cases_scatter <-
    ggplot(first_cases_plot) +
    geom_point(aes_string(x = model1, y = model2, colour = "scaled_distance"), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled distance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using\nobserved mobility (days)") +
    ylab("Time to first case using predicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    theme_classic() +
    # facet_grid(pathogen ~ seed) +
    facet_wrap(~ seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          # axis.text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    stat_cor(aes_string(x = model1, y = model2, label = "..rr.label.."),
             color = "red", geom = "text", label.x = 75, label.y = 10, size = 3)

ggsave("figures/first_cases_scatter.png", first_cases_scatter,
       height = 7.4, units = "in")

knitr::plot_crop("figures/first_cases_scatter.png")

if (! is.null(dev.list())) dev.off()