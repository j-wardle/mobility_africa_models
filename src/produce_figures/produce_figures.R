## Create folder to save figures
dir.create("figures")

## Load metric data
collated_time_to_peak <- readRDS("collated_time_to_peak.rds")
collated_time_to_first_case <- readRDS("collated_time_to_first_case.rds")

## Load location data
portugal_location_data <- readRDS("portugal_location_data.rds")
portugal_aggr_location_data <- readRDS("portugal_aggr_location_data.rds")
france_location_data<- readRDS("france_location_data.rds")
france_aggr_location_data <- readRDS("france_aggr_location_data.rds")

## Figure 1: time_to_peak with raw data

# extract the country names and model type to be more clear
# https://stackoverflow.com/questions/12297859/remove-all-text-before-colon

collated_time_to_peak <- collated_time_to_peak %>% 
  mutate(country = word(model, 1, sep ="\\_"),
         model = regmatches(model,
                            gregexpr("(?<=_).*", model, perl=TRUE)
                            )
         )

collated_time_to_peak$model <- as.character(collated_time_to_peak$model)
collated_time_to_peak <- collated_time_to_peak %>% 
  arrange(country, model)

collated_time_to_peak %>% 
  group_by(country, model) %>% 
  summarise(n = n()/2) # check number of entries per grouping

# create a vector of the appropriate location names
# TO DO: come up with a better, non-manual way of doing this
location_names <- c(
  rep(france_location_data$location, 2),
  rep(france_aggr_location_data$location, 4),
  rep(france_location_data$location, 4),
  rep(france_aggr_location_data$location, 4),
  rep(france_location_data$location, 4),
  rep(france_aggr_location_data$location, 2),
  rep(portugal_location_data$location, 2),
  rep(portugal_aggr_location_data$location, 4),
  rep(portugal_location_data$location, 4),
  rep(portugal_aggr_location_data$location, 4),
  rep(portugal_location_data$location, 4),
  rep(portugal_aggr_location_data$location, 2)
)

collated_time_to_peak$patch_name <- location_names

collated_time_to_peak <- collated_time_to_peak %>% 
  mutate(seed = replace(seed, seed == "bre", "BREST"),
         seed = replace(seed, seed == "prs", "PARIS"),
         seed = replace(seed, seed == "mdd", "MIRANDA_DO_DOURO"),
         seed = replace(seed, seed == "lsbn", "LISBOA"),
         country = replace(country, country == "fra", "france"),
         country = replace(country, country == "prtl", "portugal"),)


## extract different model names
model_types <- rep(unique(collated_time_to_peak$model), each = 4)
countries <- rep(rep(unique(collated_time_to_peak$country), each = 2), 10)
seed <- rep(unique(collated_time_to_peak$seed), 10)

# messed up by collating the results too early
# re_split
results <- pmap_dfr(list(model_types, countries, seed), function(m, c, s) {
  
  out <- filter(collated_time_to_peak, model == m & country == c & seed == s)
  central_point <- s
  
  if(isTRUE((grepl("aggr", m, fixed = TRUE)))) {
    location_data <- readRDS(paste0(c, "_aggr_location_data.rds"))
    
    if(s == "BREST") central_point <- "FINISTERE"
    if(s == "MIRANDA_DO_DOURO") central_point <- "BRAGANCA"
    
  } else {
    location_data <- readRDS(paste0(c, "_location_data.rds"))
  }
  
  location_data$seed_x <- location_data$x[location_data$location == central_point]
  location_data$seed_y <- location_data$y[location_data$location == central_point]
  
  location_data <- location_data %>%
    rowwise() %>% 
    mutate(distance = geosphere::distm(c(x, y), c(seed_x, seed_y)) / 1000
    )
  
  out$distance <- location_data$distance
  out
  
})

results <- rename(results, median = `0.5`)
results$model <- as.factor(results$model)

results3 <- results3 %>% 
  group_by(country, patch) %>% 
  mutate(time_diff = median - median[model == "mrnd"])

