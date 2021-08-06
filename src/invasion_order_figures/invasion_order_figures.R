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

test_seq <- 1:10

path1_first_cases <- first_cases %>% 
  ungroup() %>% 
  filter(pathogen == 1)

test <- map(1:10, function(patch_order) {
  
  compare_patches(path1_first_cases,
                  simulation1 = "raw",
                  simulation2 = "g2_alt",
                  seed_name = "PARIS",
                  n = patch_order)
  
})





# COMPARING FIRST 20 PATCHES ----------------------------------------------
seed_names <- seed[1:4]
names(seed_names) <- seed[1:4]

initial_patches_obs <- map(seed_names, function(x) {
  first_cases %>% 
    filter(model == "raw" & seed == x & patch_name != x) %>% 
    slice_min(median, n = 20) %>% 
    arrange(median)
})

# for each model, for each seed, how many of the first 20 are in initial_patches_obs
names(adm_small_models) <- adm_small_models

initial_matches <- map_dfr(adm_small_models, function(x) {
  
  map_dfr(seed_names, function(y) {
    
    initial_model <- first_cases %>% 
      filter(model == x & seed == y & patch_name != y) %>% 
      slice_min(median, n = 20) %>%
      arrange(median)
    
    length(initial_model$patch_name)
    
    # match_numb <- length(initial_model$patch_name[initial_model$patch_name %in% initial_patches_obs[[y]]$patch_name])
    
    # match_prop <- match_numb / length(initial_patches_obs[[y]]$patch_name)
    
  }, .id = "seed")
  
  
}, .id = "model")

initial_matches <- initial_matches %>% 
  pivot_longer(cols = "BREST":"MIRANDA_DO_DOURO",
               names_to = "seed",
               values_to = "prop")

initial_matches$seed <- factor(initial_matches$seed,
                               levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO")
)

m1 <- ggplot(initial_matches, aes(x = seed, y = prop)) +
  geom_bar(aes(fill = model), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(name = "Scenario",
                       labels = c("1", "2", "3", "4")) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  xlab("Seed location") +
  ylab("Proportion") +
  theme_classic()
m1

ggsave("figures/first_patches.png", m1, scale = 0.5)
