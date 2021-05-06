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


# TIME TO PEAK FIGURES ----------------------------------------------------

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
# re_split data: TO DO - better way to do this
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
results$seed <- factor(results$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))

## Generate plots

# First plot the peaks observed from the raw data

model.labs <- c("Raw data", "Raw aggregated data")
names(model.labs) <- c("raw", "raw_aggr")

peak_time_fig1 <- 
  results %>% 
  filter(model == "raw" | model == "raw_aggr") %>%
  ggplot() +
  geom_point(aes(x = distance, y = median), size = 0.9) +
  # ylim(c(-10, 10)) +
  xlab("Distance from seed (km)") +
  ylab("Time to peak (days)") +
  theme_classic() +
  facet_grid(seed ~ model,
             labeller = labeller(model = model.labs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

ggsave("figures/peak_time_fig1.pdf")

## Now plot the differences in peak times for the mobility models

adm_small_models <- unique(model_types)[!grepl("aggr", unique(model_types), fixed = TRUE)]
adm_big_models <- unique(model_types)[grepl("aggr", unique(model_types), fixed = TRUE)]

plots_adm_small <- map(adm_small_models, function(m) {
  
  results %>% 
    filter(model == "raw" | model == m) %>%
    group_by(country, patch) %>% 
    mutate(time_diff = median - median[model == "raw"]) %>%
    filter(model == m) %>% 
    ggplot() +
    geom_point(aes(x = distance, y = time_diff), size = 0.9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Distance from seed (km)") +
    ylab("peak from mobility model - peak from raw data (days)") +
    ggtitle(m) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  
})

pdf("figures/small_models_peak_difference.pdf")
plots_adm_small
dev.off()


plots_adm_big <- map(adm_big_models, function(m) {
  
  results %>% 
    filter(model == "raw_aggr" | model == m) %>%
    group_by(country, patch) %>% 
    mutate(time_diff = median - median[model == "raw_aggr"]) %>%
    filter(model == m) %>% 
    ggplot() +
    geom_point(aes(x = distance, y = time_diff), size = 0.9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Distance from seed (km)") +
    ylab("peak from mobility model - peak from raw data (days)") +
    ggtitle(m) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  
})

pdf("figures/big_models_peak_difference.pdf")
plots_adm_big
dev.off()



## Now plot time with raw data (x-axis) against time with model (y-axis)

plots_adm_small_scatter <- map(adm_small_models, function(m) {
  
  model_col <- as.character(m)
  
  results %>% 
    filter(model == "raw" | model == m) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>% 
    # group_by(country, patch) %>% 
    # mutate(time_diff = median - median[model == "raw"]) %>%
    # filter(model == m) %>% 
    ggplot() +
    geom_point(aes(x = raw, y = model_col), size = 0.9) +
    geom_abline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak with raw data (days)") +
    ylab("Time to peak with mobility data (days)") +
    ggtitle(m) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  
})

pdf("figures/small_models_peak_difference.pdf")
plots_adm_small
dev.off()



# TIME TO FIRST CASE FIGURES ----------------------------------------------


# extract the country names and model type to be more clear
# https://stackoverflow.com/questions/12297859/remove-all-text-before-colon

collated_time_to_first_case <- collated_time_to_first_case %>% 
  mutate(country = word(model, 1, sep ="\\_"),
         model = regmatches(model,
                            gregexpr("(?<=_).*", model, perl=TRUE)
         )
  )

collated_time_to_first_case$model <- as.character(collated_time_to_first_case$model)
collated_time_to_first_case <- collated_time_to_first_case %>% 
  arrange(country, model)

collated_time_to_first_case$patch_name <- location_names

collated_time_to_first_case <- collated_time_to_first_case %>% 
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
# re_split data: TO DO - better way to do this
first_cases <- pmap_dfr(list(model_types, countries, seed), function(m, c, s) {
  
  out <- filter(collated_time_to_first_case, model == m & country == c & seed == s)
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

first_cases <- rename(first_cases, median = `0.5`)
first_cases$model <- as.factor(first_cases$model)
first_cases$seed <- factor(first_cases$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))

## Generate plots

# First plot the peaks observed from the raw data

model.labs <- c("Raw data", "Raw aggregated data")
names(model.labs) <- c("raw", "raw_aggr")

first_cases_fig1 <- 
  first_cases %>% 
  filter(model == "raw" | model == "raw_aggr") %>%
  ggplot() +
  geom_point(aes(x = distance, y = median), size = 0.9) +
  # ylim(c(-10, 10)) +
  xlab("Distance from seed (km)") +
  ylab("Time to first case (days)") +
  theme_classic() +
  facet_grid(seed ~ model,
             labeller = labeller(model = model.labs)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

ggsave("figures/first_case_fig1.pdf")

## Now plot the differences in peak times for the mobility models

first_cases_adm_small <- map(adm_small_models, function(m) {
  
  first_cases %>% 
    filter(model == "raw" | model == m) %>%
    group_by(country, patch) %>% 
    mutate(time_diff = median - median[model == "raw"]) %>%
    filter(model == m) %>% 
    ggplot() +
    geom_point(aes(x = distance, y = time_diff), size = 0.9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Distance from seed (km)") +
    ylab("first case from mobility model - first case from raw data (days)") +
    ggtitle(m) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  
})

pdf("figures/small_models_first_case.pdf")
first_cases_adm_small
dev.off()


first_cases_adm_big <- map(adm_big_models, function(m) {
  
  first_cases %>% 
    filter(model == "raw_aggr" | model == m) %>%
    group_by(country, patch) %>% 
    mutate(time_diff = median - median[model == "raw_aggr"]) %>%
    filter(model == m) %>% 
    ggplot() +
    geom_point(aes(x = distance, y = time_diff), size = 0.9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Distance from seed (km)") +
    ylab("first case from mobility model - first case from raw data (days)") +
    ggtitle(m) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  
})

pdf("figures/big_models_first_case.pdf")
first_cases_adm_big
dev.off()

