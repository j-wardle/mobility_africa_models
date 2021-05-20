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
ggsave("figures/peak_scatter_small.png", p1, scale = 2)

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
ggsave("figures/peak_scatter_small_cri.png", p2, scale = 2)



## Scatters for aggregated spatial units

# adm_big_models <- adm_big_models[adm_big_models != "raw_aggr"]
# 
# results_big <- filter(results, model %in% adm_big_models | model == "raw_aggr")
# 
# min(results_big$median) #159.5
# max(results_big$median) #233
# 
# big_scatters <- map(adm_big_models, function(model_name) {
#   
#   results %>% 
#     filter(model == "raw_aggr" | model == model_name) %>%
#     pivot_wider(id_cols = c(patch, seed),
#                 names_from = model,
#                 values_from = median) %>%
#     ggplot() +
#     geom_point(aes(x = raw_aggr, y = get(model_name)), size = 0.8) +
#     geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
#     xlab("Time to peak with observed mobility (days)") +
#     ylab("Time to peak with\npredicted mobility (days)") +
#     scale_x_continuous(limits = c(150, 240),
#                        breaks = seq(150, 230, 20)) +
#     scale_y_continuous(limits = c(150, 240),
#                        breaks = seq(150, 230, 20)) +
#     ggtitle(model_name) +
#     coord_fixed() +
#     theme_classic() +
#     facet_wrap(~seed, nrow = 2) +
#     theme(panel.border = element_rect(colour = "black", fill = NA),
#           axis.text = element_text(size = 7))
#   
# })
# 
# p2 <- patchwork::wrap_plots(big_scatters)
# ggsave("figures/peak_scatter_big.png", p2, scale = 2)

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
ggsave("figures/first_case_scatter_small.png", f1, scale = 2)


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
ggsave("figures/first_case_scatter_small_cri.png", f2, scale = 2)




# ## First cases - big
# 
# first_cases_big <- filter(first_cases, model %in% adm_big_models | model == "raw_aggr")
# 
# min(first_cases_big$median) #1
# max(first_cases_big$median) #111
# 
# 
# first_cases_scatter_big <- map(adm_big_models, function(model_name) {
#   
#   first_cases %>% 
#     filter(model == "raw_aggr" | model == model_name) %>%
#     pivot_wider(id_cols = c(patch, seed),
#                 names_from = model,
#                 values_from = median) %>% 
#     ggplot() +
#     geom_point(aes(x = raw_aggr, y = get(model_name)), size = 0.8) +
#     geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
#     xlab("Time to first case using observed mobility (days)") +
#     ylab("Time to first case using\npredicted mobility (days)") +
#     scale_x_continuous(limits = c(0, 115),
#                        breaks = seq(0, 125, 25)) +
#     scale_y_continuous(limits = c(0, 115),
#                        breaks = seq(0, 125, 25)) +
#     coord_fixed() +
#     ggtitle(model_name) +
#     theme_classic() +
#     facet_wrap(~seed, nrow = 2) +
#     theme(panel.border = element_rect(colour = "black", fill = NA),
#           axis.text = element_text(size = 7))
#   
# })
# 
# p4 <- patchwork::wrap_plots(first_cases_scatter_big)
# p4
# ggsave("figures/first_case_scatter_big.png", p4, scale = 2)



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