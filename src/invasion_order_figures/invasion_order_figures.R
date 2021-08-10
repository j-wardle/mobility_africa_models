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
  
  time_to_first_case <- time_to_first_case %>% 
    filter(model == "raw" | model == "g2_alt")
  
}


# for now, filter so that we only show france and brest

time_to_first_case <- time_to_first_case %>% 
  filter(seed == "BREST" | seed == "PARIS")


# add location names

location_names <- rep(france_location_data$location, 8)

time_to_first_case$patch_name <- location_names

first_cases <- time_to_first_case

## Tidy variable names

first_cases <- rename(first_cases, median = `0.5`)
first_cases$model <- as.factor(first_cases$model)
first_cases$seed <- factor(first_cases$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))
first_cases$`95%CrI` <- first_cases$`0.975` - first_cases$`0.025`
first_cases$standardised_variation <- first_cases$`95%CrI` / first_cases$median

# find number of patches

total_patches <- length(unique(first_cases$patch_name))

# compare patch predictions at each increment

path1_first_cases <- first_cases %>% 
  ungroup() %>% 
  filter(pathogen == 1)

# Pathogen 1, Paris
path1_order_prs <- map_dfr(1:(length(unique(path1_first_cases$patch)) - 1), function(patch_order) {
  
  compare_patches(path1_first_cases,
                  simulation1 = "raw",
                  simulation2 = "g2_alt",
                  seed_name = "PARIS",
                  n = patch_order)
  
}, .id = "invasion_order")


path1_order_prs$pathogen <- 1
path1_order_prs$seed <- "PARIS"

# Pathogen1, Brest
path1_order_bre <- map_dfr(1:(length(unique(path1_first_cases$patch)) - 1), function(patch_order) {
  
  compare_patches(path1_first_cases,
                  simulation1 = "raw",
                  simulation2 = "g2_alt",
                  seed_name = "BREST",
                  n = patch_order)
  
}, .id = "invasion_order")

path1_order_bre$pathogen <- 1
path1_order_bre$seed <- "BREST"

## PATHOGEN 2
path2_first_cases <- first_cases %>% 
  ungroup() %>% 
  filter(pathogen == 2)

# Pathogen 2, Paris
path2_order_prs <- map_dfr(1:(length(unique(path2_first_cases$patch)) - 1), function(patch_order) {
  
  compare_patches(path2_first_cases,
                  simulation1 = "raw",
                  simulation2 = "g2_alt",
                  seed_name = "PARIS",
                  n = patch_order)
  
}, .id = "invasion_order")


path2_order_prs$pathogen <- 2
path2_order_prs$seed <- "PARIS"

# Pathogen 2, Brest
path2_order_bre <- map_dfr(1:(length(unique(path2_first_cases$patch)) - 1), function(patch_order) {
  
  compare_patches(path2_first_cases,
                  simulation1 = "raw",
                  simulation2 = "g2_alt",
                  seed_name = "BREST",
                  n = patch_order)
  
}, .id = "invasion_order")

path2_order_bre$pathogen <- 2
path2_order_bre$seed <- "BREST"


# Join the different dataframes

order_success <- bind_rows(path1_order_prs,
                           path1_order_bre,
                           path2_order_prs,
                           path2_order_bre)

order_success$pathogen <- as.factor(order_success$pathogen)

# Plot the order prediction success data

order_success_plot <- 
  ggplot(order_success) +
  geom_line(aes(x = n, y = match_prop, colour = seed, linetype = pathogen)) +
  xlab("Number of patches infected") +
  ylab("Proportion") +
  labs(colour = "Seed location",
       linetype = "Pathogen") +
  theme_minimal()

# Save plot image file
ggsave("figures/france_invasion_order.png", order_success_plot, scale = 0.5)

# Save ggplot object for use in creating panel
saveRDS(order_success_plot, file = "figures/france_invasion_order.rds")
