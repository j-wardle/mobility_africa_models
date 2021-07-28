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


# TIME TO PEAK PROCESSING ----------------------------------------------------

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

results <- results %>% 
  group_by(seed) %>% 
  mutate(scaled_distance = distance / max(distance))

results <- rename(results, median = `0.5`)
results$model <- as.factor(results$model)
results$seed <- factor(results$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))
results$`95%CrI` <- results$`0.975` - results$`0.025`
results$standardised_variation <- results$`95%CrI` / results$median

adm_small_models <- unique(model_types)[!grepl("aggr", unique(model_types), fixed = TRUE)]
adm_big_models <- unique(model_types)[grepl("aggr", unique(model_types), fixed = TRUE)]


# TIME TO FIRST CASE PROCESSING ----------------------------------------------

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

first_cases <- first_cases %>% 
  group_by(seed) %>% 
  mutate(scaled_distance = distance / max(distance))

first_cases <- rename(first_cases, median = `0.5`)
first_cases$model <- as.factor(first_cases$model)
first_cases$seed <- factor(first_cases$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))
first_cases$`95%CrI` <- first_cases$`0.975` - first_cases$`0.025`
first_cases$standardised_variation <- first_cases$`95%CrI` / first_cases$median



# SCATTER PLOTS -----------------------------------------------------------

## Scatter time vs time plots
adm_small_models <- adm_small_models[adm_small_models != "raw"]

results_small <- filter(results, model %in% adm_small_models | model == "raw")

##################
## TIME TO PEAK ##
##################

## Scatter of small units

min(results_small$median) #155
max(results_small$median) #211.5

small_scatters <- map(adm_small_models, function(model_name) {
  
  results %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak using\nobserved mobility (days)") +
    ylab("Time to peak using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 10)) +
    scale_y_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 20)) +
    ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45),
          strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
})


p1 <- wrap_plots(small_scatters) + plot_layout(guides = "collect")
p1
ggsave("figures/peak_scatter_small.png", p1,
       width = 10, height = 8.65, units = "in")

## Scatter of small units, 95%CrI

small_scatters_cri <- map(adm_small_models, function(model_name) {
  
  results %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = `95%CrI`) %>%
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Width of 95% CrI using\nobserved mobility (days)") +
    ylab("Width of 95% CrI using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(40, 110),
                       breaks = seq(50, 100, 10)) +
    scale_y_continuous(limits = c(40, 110),
                       breaks = seq(50, 100, 10)) +
    ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45),
          strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
})

p2 <- wrap_plots(small_scatters_cri) + plot_layout(guides = "collect")
p2
ggsave("figures/peak_scatter_small_cri.png", p2,
       width = 10, height = 8.65, units = "in")



## Scatters for aggregated spatial units

adm_big_models <- adm_big_models[adm_big_models != "raw_aggr"]

results_big <- filter(results, model %in% adm_big_models | model == "raw_aggr")

min(results_big$median) #159.5
max(results_big$median) #233

big_scatters <- map(adm_big_models, function(model_name) {
  
  results %>% 
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw_aggr, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak using\nobserved mobility (days)") +
    ylab("Time to peak using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(150, 240),
                       breaks = seq(150, 230, 10)) +
    scale_y_continuous(limits = c(150, 240),
                       breaks = seq(150, 230, 20)) +
    ggtitle(paste0("Scenario ", which(adm_big_models == model_name) + 4)) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45),
          strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
})


p2 <- wrap_plots(big_scatters) + plot_layout(guides = "collect")
p2
ggsave("figures/peak_scatter_big.png", p2,
       width = 10, height = 8.65, units = "in")

########################
########################
## Time to first case ##
########################
########################

first_cases_small <- filter(first_cases, model %in% adm_small_models | model == "raw")

## Scatter plot for small units

min(first_cases_small$median) #1
max(first_cases_small$median) #111

first_cases_scatter_small <- map(adm_small_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = median) %>% 
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using observed mobility (days)") +
    ylab("Time to first case using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5))
  
})

f1 <- wrap_plots(first_cases_scatter_small) + plot_layout(guides = "collect")
f1
# ggsave("figures/first_case_scatter_small.png", f1, scale = 2)
ggsave("figures/first_case_scatter_small.png", f1,
       width = 10, height = 8.65, units = "in")


## Scatter plot for small units, 95%CrI

first_cases_scatter_small_cri <- map(adm_small_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = `95%CrI`) %>%
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Width of 95% CrI using\nobserved mobility (days)") +
    ylab("Width of 95% CrI using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 130),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 130),
                       breaks = seq(0, 125, 25)) +
    ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          axis.text.x = element_text(angle = 45),
          strip.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5))
  
})

f2 <- wrap_plots(first_cases_scatter_small_cri) + plot_layout(guides = "collect")
f2
ggsave("figures/first_case_scatter_small_cri.png", f2,
       width = 10, height = 8.65, units = "in")




## First cases - big

first_cases_big <- filter(first_cases, model %in% adm_big_models | model == "raw_aggr")

min(first_cases_big$median) #1
max(first_cases_big$median) #111


first_cases_scatter_big <- map(adm_big_models, function(model_name) {

  first_cases %>%
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed, scaled_distance),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw_aggr, y = get(model_name), colour = scaled_distance), size = 0.8) +
    scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using observed mobility (days)") +
    ylab("Time to first case using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    ggtitle(paste0("Scenario ", which(adm_big_models == model_name) + 4)) +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5))

})

p4 <- wrap_plots(first_cases_scatter_big) + plot_layout(guides = "collect")
p4
ggsave("figures/first_case_scatter_big.png", p4,
       width = 10, height = 8.65, units = "in")



## SPEARMAN RANK TESTS -------

## Models at small spatial scale

## Time to first case

names(adm_small_models) <- adm_small_models
spearman_first_cases_small <- map_dfr(adm_small_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    group_by(seed) %>% 
    mutate(rho = cor.test(raw, get(model_name),  method = "spearman")[["estimate"]],
           p_value = cor.test(raw, get(model_name),  method = "spearman")[["p.value"]]) %>% 
    select(seed, rho, p_value)
  
}, .id = "model")

spearman_first_cases_small <- spearman_first_cases_small %>% 
  group_by(model, seed) %>% 
  summarise(rho = round(mean(rho), 3),
            p_value = round(mean(p_value), 5)) %>% 
  pivot_wider(id_cols = model,
              names_from = seed,
              values_from = rho)

write.csv(spearman_first_cases_small, "figures/spearman_first_case_small.csv")

## Time to epidemic peak

spearman_peak_small <- map_dfr(adm_small_models, function(model_name) {
  
  results %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    group_by(seed) %>% 
    mutate(rho = cor.test(raw, get(model_name),  method = "spearman")[["estimate"]],
           p_value = cor.test(raw, get(model_name),  method = "spearman")[["p.value"]]) %>% 
    select(seed, rho, p_value)
  
}, .id = "model")

spearman_peak_small <- spearman_peak_small %>% 
  group_by(model, seed) %>% 
  summarise(rho = round(mean(rho), 3),
            p_value = round(mean(p_value), 5)) %>% 
  pivot_wider(id_cols = model,
              names_from = seed,
              values_from = rho)

write.csv(spearman_peak_small, "figures/spearman_peak_small.csv")

## Models at aggregated spatial scale

## Time to first case

names(adm_big_models) <- adm_big_models
spearman_first_cases_big <- map_dfr(adm_big_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    group_by(seed) %>% 
    mutate(rho = cor.test(raw_aggr, get(model_name),  method = "spearman")[["estimate"]],
           p_value = cor.test(raw_aggr, get(model_name),  method = "spearman")[["p.value"]]) %>% 
    select(seed, rho, p_value)
  
}, .id = "model")

spearman_first_cases_big <- spearman_first_cases_big %>% 
  group_by(model, seed) %>% 
  summarise(rho = round(mean(rho), 3),
            p_value = round(mean(p_value), 5)) %>% 
  pivot_wider(id_cols = model,
              names_from = seed,
              values_from = rho)

write.csv(spearman_first_cases_big, "figures/spearman_first_case_big.csv")

## Time to epidemic peak

spearman_peak_big <- map_dfr(adm_big_models, function(model_name) {
  
  results %>% 
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    group_by(seed) %>% 
    mutate(rho = cor.test(raw_aggr, get(model_name),  method = "spearman")[["estimate"]],
           p_value = cor.test(raw_aggr, get(model_name),  method = "spearman")[["p.value"]]) %>% 
    select(seed, rho, p_value)
  
}, .id = "model")

spearman_peak_big <- spearman_peak_big %>% 
  group_by(model, seed) %>% 
  summarise(rho = round(mean(rho), 3),
            p_value = round(mean(p_value), 5)) #%>% 
# pivot_wider(id_cols = model,
#             names_from = seed,
#             values_from = rho)

write.csv(spearman_peak_big, "figures/spearman_peak_big.csv")



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



# MOBILITY SCATTER PLOTS --------------------------------------------------

## Mobility figures
obs_mob <- readRDS("scaled_matrix.rds")

if (spatial_res == "high") {
  pred_mob1 <- readRDS("gravity_matrix1_numbers.rds")
  pred_mob2 <- readRDS("gravity_matrix2_numbers.rds")
}
if (spatial_res == "low") {
  pred_mob1 <- readRDS("lo_res_gravity_matrix1_numbers.rds")
  pred_mob2 <- readRDS("lo_res_gravity_matrix2_numbers.rds")
}

palette <- ggpubr::get_palette("lancet", 5)

## FRANCE ##

france_mob <- obs_mob[["france"]]
diag(france_mob) <- 0
france_mob <- as.data.frame(france_mob)
france_mob$origin <- rownames(france_mob)

france_mob <- france_mob %>%
  pivot_longer(cols = !origin,
                names_to = "destination",
                values_to = "observed")

france_pred1 <- pred_mob1[grepl("france", names(pred_mob1)) & grepl("adm3", names(pred_mob1))]
france_pred2 <- pred_mob2[grepl("france", names(pred_mob2)) & grepl("adm3", names(pred_mob2))]

france_pred <- c(france_pred1, france_pred2)
names(france_pred) <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")

france_predictions <- map(france_pred, function(mod) {
  
  diag(mod) <- 0
  mod <- as.data.frame(mod)
  mod$origin <- rownames(mod)
  
  mod <- mod %>% 
    pivot_longer(cols = !origin,
                 names_to = "destination",
                 values_to = "predicted")
  
  mod$observed <- round(france_mob$observed)

  mod
  
})  

france_predictions <- bind_rows(france_predictions, .id = "scenario")
france_predictions$country <- "FRANCE"

## PORTUGAL ##

prt_mob <- obs_mob[["portugal"]]
diag(prt_mob) <- 0
prt_mob <- as.data.frame(prt_mob)
prt_mob$origin <- rownames(prt_mob)

prt_mob <- prt_mob %>%
  pivot_longer(cols = !origin,
               names_to = "destination",
               values_to = "observed")

prt_pred1 <- pred_mob1[grepl("portugal", names(pred_mob1)) & grepl("adm2", names(pred_mob1))]
prt_pred2 <- pred_mob2[grepl("portugal", names(pred_mob2)) & grepl("adm2", names(pred_mob2))]

prt_pred <- c(prt_pred1, prt_pred2)
names(prt_pred) <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")

prt_predictions <- map(prt_pred, function(mod) {
  
  diag(mod) <- 0
  mod <- as.data.frame(mod)
  mod$origin <- rownames(mod)
  
  mod <- mod %>% 
    pivot_longer(cols = !origin,
                 names_to = "destination",
                 values_to = "predicted")
  
  mod$observed <- round(prt_mob$observed)
  
  mod
  
})  

prt_predictions <- bind_rows(prt_predictions, .id = "scenario")
prt_predictions$country <- "PORTUGAL"


## JOINT PLOT ##

combined_preds <- bind_rows(france_predictions, prt_predictions)

combined_plot <- ggplot(combined_preds) +
  geom_point(aes(x = observed, y = predicted, colour = country),
             size = 0.8, shape = 1, alpha = 0.2) +
  scale_colour_manual(values = c(palette[1], palette[5])) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  coord_fixed() +
  xlab("Observed movement") +
  ylab("Predicted movement") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(country ~ scenario) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))
combined_plot

ggsave("figures/predicted_mob.png", combined_plot, scale  = 2)#,
       #width = 10, height = 8.65, units = "in"

combined_preds <- combined_preds %>% 
  mutate(overestimate = ifelse(predicted > observed, 1, 0))

combined_preds %>% 
  group_by(country, scenario) %>% 
  summarise(propn = sum(overestimate) / n())

## Overlay summary stats on the scatter plot

summary_overlay <- combined_preds %>% 
  mutate(obs_bins = findInterval(observed, c(0, 1, 10, 10e1, 10e2, 10e3, 10e4, 10e5))) 

summary_overlay <- summary_overlay %>% 
  group_by(country, scenario, obs_bins) %>% 
  mutate(bin_median = median(observed),
         bin_pred_mid = median(predicted),
         bin_pred_lo = quantile(predicted, 0.025),
         bin_pred_hi = quantile(predicted, 0.975),
         group_count = row_number())

## Scatter plot with added boxplots summarising pred values in each bin

bin_divides <- c(0, 1, 10, 10e1, 10e2, 10e3, 10e4, 10e5)

plot_with_bins <-
  ggplot(summary_overlay, aes(observed, predicted)) +
  geom_point(aes(colour = country),
             size = 0.8, shape = 1, alpha = 0.2) +
  scale_colour_manual(values = c(palette[1], palette[5])) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = bin_divides, colour = "grey", linetype = 2) +
  geom_errorbar(data = . %>% filter(group_count == 1),
                aes(x = bin_median, ymin = bin_pred_lo, ymax = bin_pred_hi),
                width = 0.5, size = 1) +
  geom_point(data = . %>% filter(group_count == 1),
             aes(x = bin_median, y = bin_pred_mid),
             size = 1.5) +
  coord_fixed() +
  xlab("Observed movement") +
  ylab("Predicted movement") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(country ~ scenario) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

ggsave("figures/predicted_mob_comb_box.png", plot_with_bins, scale  = 2)


## Aggregated scale

if (spatial_res == "high") {
  
  obs_mob_aggr <- readRDS("aggregated_scaled_matrix.rds")
  
  france_aggr <- obs_mob_aggr[["france"]]
  diag(france_aggr) <- 0
  france_aggr <- as.data.frame(france_aggr)
  france_aggr$origin <- rownames(france_aggr)
  
  france_aggr <- france_aggr %>%
    pivot_longer(cols = !origin,
                 names_to = "destination",
                 values_to = "observed")
  
  france_pred1_aggr <- pred_mob1[grepl("france", names(pred_mob1)) & grepl("adm2", names(pred_mob1))]
  france_pred2_aggr <- pred_mob2[grepl("france", names(pred_mob2)) & grepl("adm2", names(pred_mob2))]
  
  france_pred_aggr <- c(france_pred1_aggr, france_pred2_aggr)
  names(france_pred_aggr) <- c("Scenario 5", "Scenario 6", "Scenario 7", "Scenario 8")
  
  france_aggr_predictions <- map(france_pred_aggr, function(mod) {
    
    diag(mod) <- 0
    mod <- as.data.frame(mod)
    mod$origin <- rownames(mod)
    
    mod <- mod %>% 
      pivot_longer(cols = !origin,
                   names_to = "destination",
                   values_to = "predicted")
    
    mod$observed <- round(france_aggr$observed)
    
    mod
    
  })  
  
  prt_aggr <- obs_mob_aggr[["portugal"]]
  diag(prt_aggr) <- 0
  prt_aggr <- as.data.frame(prt_aggr)
  prt_aggr$origin <- rownames(prt_aggr)
  
  prt_aggr <- prt_aggr %>%
    pivot_longer(cols = !origin,
                 names_to = "destination",
                 values_to = "observed")
  
  prt_pred1_aggr <- pred_mob1[grepl("portugal", names(pred_mob1)) & grepl("adm1", names(pred_mob1))]
  prt_pred2_aggr <- pred_mob2[grepl("portugal", names(pred_mob2)) & grepl("adm1", names(pred_mob2))]
  
  prt_pred_aggr <- c(prt_pred1_aggr, prt_pred2_aggr)
  names(prt_pred_aggr) <- c("Scenario 5", "Scenario 6", "Scenario 7", "Scenario 8")
  
  prt_aggr_predictions <- map(prt_pred_aggr, function(mod) {
    
    diag(mod) <- 0
    mod <- as.data.frame(mod)
    mod$origin <- rownames(mod)
    
    mod <- mod %>% 
      pivot_longer(cols = !origin,
                   names_to = "destination",
                   values_to = "predicted")
    
    mod$observed <- round(prt_aggr$observed)
    
    mod
    
  })  
  
  
  france_aggr_predictions <- bind_rows(france_aggr_predictions, .id = "scenario")
  france_aggr_predictions$country <- "FRANCE"
  
  prt_aggr_predictions <- bind_rows(prt_aggr_predictions, .id = "scenario")
  prt_aggr_predictions$country <- "PORTUGAL"
  
  aggr_predictions <- bind_rows(france_aggr_predictions, prt_aggr_predictions)
  
  comb_aggr_plot <- ggplot(aggr_predictions) +
    geom_point(aes(x = observed, y = predicted, colour = country),
               size = 0.8, shape = 1, alpha = 0.5) +
    scale_colour_manual(values = c(palette[1], palette[5])) +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    coord_fixed() +
    xlab("Observed movement") +
    ylab("Predicted movement") +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    facet_grid(country ~ scenario) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16))
  comb_aggr_plot
  
  ggsave("figures/predicted_mob_aggr.png", comb_aggr_plot, scale  = 2)#,
  #width = 10, height = 8.65, units = "in"
  
  aggr_predictions$diff <- aggr_predictions$predicted - aggr_predictions$observed
  
  aggr_predictions <- aggr_predictions %>% 
    mutate(overestimate = ifelse(predicted > observed, 1, 0))
  
  aggr_predictions %>% 
    group_by(country, scenario) %>% 
    summarise(propn = sum(overestimate) / n())
  
  
  ## Scatter plot with added boxplots summarising pred values in each bin
  
  aggr_plot_with_box <-
    ggplot(aggr_predictions, aes(observed, predicted)) +
    geom_point(aes(colour = country),
               size = 0.8, shape = 1, alpha = 0.5) +
    scale_colour_manual(values = c(palette[1], palette[5])) +
    geom_abline(intercept = 0, slope = 1, colour = "red") +
    geom_boxplot(aes(group = cut(observed,
                                 breaks = c(0, 1, 10, 10e1, 10e2, 10e3, 10e4, 10e5), #set bins for boxplot
                                 include.lowest = TRUE),
                     alpha = 0.8), outlier.alpha = 0.1) +
    coord_fixed() +
    xlab("Observed movement") +
    ylab("Predicted movement") +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    facet_grid(country ~ scenario) +
    theme_classic() +
    theme(legend.position = "none",
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16))
  
  ggsave("figures/predicted_mob_comb_box_aggr.png", aggr_plot_with_box, scale  = 2)
  
}