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

time_to_first_case$patch_name <- location_names

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


### TO DO: REVIEW AND IMPLEMENT THIS BIT

# ## extract different model names
# model_types <- rep(unique(time_to_first_case$model), each = 4)
# countries <- rep(rep(unique(time_to_first_case$country), each = 2), 10)
# seed <- rep(unique(time_to_first_case$seed), 10)
# 
# # messed up by collating the results too early
# # re_split data: TO DO - better way to do this
# first_cases <- pmap_dfr(list(model_types, countries, seed), function(m, c, s) {
#   
#   out <- filter(collated_time_to_first_case, model == m & country == c & seed == s)
#   central_point <- s
#   
#   if(isTRUE((grepl("aggr", m, fixed = TRUE)))) {
#     location_data <- readRDS(paste0(c, "_aggr_location_data.rds"))
#     
#     if(s == "BREST") central_point <- "FINISTERE"
#     if(s == "MIRANDA_DO_DOURO") central_point <- "BRAGANCA"
#     
#   } else {
#     location_data <- readRDS(paste0(c, "_location_data.rds"))
#   }
#   
#   location_data$seed_x <- location_data$x[location_data$location == central_point]
#   location_data$seed_y <- location_data$y[location_data$location == central_point]
#   
#   location_data <- location_data %>%
#     rowwise() %>% 
#     mutate(distance = geosphere::distm(c(x, y), c(seed_x, seed_y)) / 1000
#     )
#   
#   out$distance <- location_data$distance
#   out
#   
# })
# 
# first_cases <- first_cases %>% 
#   group_by(seed) %>% 
#   mutate(scaled_distance = distance / max(distance))

first_cases <- rename(first_cases, median = `0.5`)
first_cases$model <- as.factor(first_cases$model)
first_cases$seed <- factor(first_cases$seed, levels = c("BREST", "PARIS", "LISBOA", "MIRANDA_DO_DOURO"))
first_cases$`95%CrI` <- first_cases$`0.975` - first_cases$`0.025`
first_cases$standardised_variation <- first_cases$`95%CrI` / first_cases$median




### Create scatter plot

### TO DO: Need to tidy this up
### TO Do: at the moment, pathogen 1 and pathogen 2 are the same...

a <- first_cases %>% 
  pivot_wider(id_cols = c(patch, seed, model, pathogen),
              names_from = model,
              values_from = median)
# first_cases_scatter <-
  first_cases %>% 
    pivot_wider(id_cols = c(patch, seed, model, pathogen),
                names_from = model,
                values_from = median) %>% 
    ggplot() +
    geom_point(aes(x = raw, y = g2_alt), size = 0.8) +
    # scale_colour_viridis_c(name = "Scaled\ndistance") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using observed mobility (days)") +
    ylab("Time to first case using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    theme_classic() +
    facet_grid(pathogen ~ seed) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7),
          plot.title = element_text(hjust = 0.5))
  




