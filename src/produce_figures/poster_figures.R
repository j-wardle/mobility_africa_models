## Time to peak

plot1 <-
  results %>% 
  filter(model == "raw" | model == "g2_alt") %>%
  group_by(country, patch) %>% 
  mutate(time_diff = median - median[model == "raw"]) %>%
  filter(model == "g2_alt") %>% 
  ggplot() +
  geom_point(aes(x = distance, y = time_diff), size = 0.9) +
  geom_hline(yintercept = 0, colour = "red", linetype = 2) +
  xlab("Distance from seed (km)") +
  ylab("Difference in time to epidemic peaks (days)") +
  scale_y_continuous(limits = c(-45, 40),
                     breaks = seq(-40, 40, 10)) +
  theme_classic() +
  facet_wrap(~seed, nrow = 1) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

ggsave("figures/peak_time_diff_g2_alt.png", plot1)


plot2 <-
  results %>% 
  filter(model == "raw_aggr" | model == "g2_aggr_alt") %>%
  group_by(country, patch) %>% 
  mutate(time_diff = median - median[model == "raw_aggr"]) %>%
  filter(model == "g2_aggr_alt") %>% 
  ggplot() +
  geom_point(aes(x = distance, y = time_diff), size = 0.9) +
  geom_hline(yintercept = 0, colour = "red", linetype = 2) +
  xlab("Distance from seed (km)") +
  ylab("Difference in time to epidemic peaks (days)") +
  scale_y_continuous(limits = c(-45, 40),
                     breaks = seq(-40, 40, 10)) +
  theme_classic() +
  facet_wrap(~seed, nrow = 1) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

ggsave("figures/peak_time_diff_g2_aggr_alt.png", plot2)


## Time to first case

plot3 <-
  first_cases %>% 
  filter(model == "raw" | model == "g2_alt") %>%
  group_by(country, patch) %>% 
  mutate(time_diff = median - median[model == "raw"]) %>%
  filter(model == "g2_alt") %>% 
  ggplot() +
  geom_point(aes(x = distance, y = time_diff), size = 0.9) +
  geom_hline(yintercept = 0, colour = "red", linetype = 2) +
  xlab("Distance from seed (km)") +
  ylab("Difference in time to first case (days)") +
  scale_y_continuous(limits = c(-65, 35),
                     breaks = seq(-60, 30, 10)) +
  theme_classic() +
  facet_wrap(~seed, nrow = 1) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

ggsave("figures/first_case_diff_g2_alt.png", plot3)
  
plot4 <-  
  first_cases %>% 
    filter(model == "raw_aggr" | model == "g2_aggr_alt") %>%
    group_by(country, patch) %>% 
    mutate(time_diff = median - median[model == "raw_aggr"]) %>%
    filter(model == "g2_aggr_alt") %>% 
    ggplot() +
    geom_point(aes(x = distance, y = time_diff), size = 0.9) +
    geom_hline(yintercept = 0, colour = "red", linetype = 2) +
    xlab("Distance from seed (km)") +
    ylab("Difference in time to first case (days)") +
    scale_y_continuous(limits = c(-65, 35),
                       breaks = seq(-60, 30, 10)) +
    theme_classic() +
    facet_wrap(~seed, nrow = 1) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12))

ggsave("figures/first_case_diff_g2_aggr_alt.png", plot4)




## Scatter time vs time plots
adm_small_models <- adm_small_models[adm_small_models != "raw"]

results_small <- filter(results, model %in% adm_small_models)

min(results_small$median) #155
max(results_small$median) #210

small_scatters <- map(adm_small_models, function(model_name) {
  
  results %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name)), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak with observed mobility (days)") +
    ylab("Time to peak with\npredicted mobility (days)") +
    scale_x_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 10)) +
    scale_y_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 20)) +
    ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7))
  
})


p1 <- patchwork::wrap_plots(small_scatters)
p1
p1_wide <- patchwork::wrap_plots(small_scatters, nrow = 1)
p1_wide
ggsave("figures/peak_scatter_small.png", p1, scale = 2)
ggsave("figures/peak_scatter_small_wide.png", p1_wide, scale = 1, width = 50, units = "cm")


adm_big_models <- adm_big_models[adm_big_models != "raw_aggr"]

results_big <- filter(results, model %in% adm_big_models | model == "raw_aggr")

min(results_big$median) #159.5
max(results_big$median) #233

big_scatters <- map(adm_big_models, function(model_name) {
  
  results %>% 
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw_aggr, y = get(model_name)), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak with observed mobility (days)") +
    ylab("Time to peak with\npredicted mobility (days)") +
    scale_x_continuous(limits = c(150, 240),
                       breaks = seq(150, 230, 20)) +
    scale_y_continuous(limits = c(150, 240),
                       breaks = seq(150, 230, 20)) +
    ggtitle(model_name) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7))
  
})

p2 <- patchwork::wrap_plots(big_scatters)
ggsave("figures/peak_scatter_big.png", p2, scale = 2)

## Time to first case - scatter plots

first_cases_small <- filter(first_cases, model %in% adm_small_models | model == "raw")

min(first_cases_small$median) #1
max(first_cases_small$median) #111


first_cases_scatter_small <- map(adm_small_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>% 
    ggplot() +
    geom_point(aes(x = raw, y = get(model_name)), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using observed mobility (days)") +
    ylab("Time to first case using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    ggtitle(model_name) +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7))
  
})

p3 <- patchwork::wrap_plots(first_cases_scatter_small)
p3
ggsave("figures/first_case_scatter_small.png", p3, scale = 2)


## First cases - big

first_cases_big <- filter(first_cases, model %in% adm_big_models | model == "raw_aggr")

min(first_cases_big$median) #1
max(first_cases_big$median) #111


first_cases_scatter_big <- map(adm_big_models, function(model_name) {
  
  first_cases %>% 
    filter(model == "raw_aggr" | model == model_name) %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>% 
    ggplot() +
    geom_point(aes(x = raw_aggr, y = get(model_name)), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to first case using observed mobility (days)") +
    ylab("Time to first case using\npredicted mobility (days)") +
    scale_x_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    scale_y_continuous(limits = c(0, 115),
                       breaks = seq(0, 125, 25)) +
    coord_fixed() +
    ggtitle(model_name) +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7))
  
})

p4 <- patchwork::wrap_plots(first_cases_scatter_big)
p4
ggsave("figures/first_case_scatter_big.png", p4, scale = 2)


## Scenario 4 plots -----

scen4_peaks <-
  results %>% 
    filter(model == "raw" | model == "g2_alt") %>%
    pivot_wider(id_cols = c(patch, seed),
                names_from = model,
                values_from = median) %>%
    ggplot() +
    geom_point(aes(x = raw, y = g2_alt), size = 0.8) +
    geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
    xlab("Time to peak with observed mobility (days)") +
    ylab("Time to peak with\npredicted mobility (days)") +
    scale_x_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 10)) +
    scale_y_continuous(limits = c(150, 220),
                       breaks = seq(150, 210, 20)) +
    # ggtitle(paste0("Scenario ", which(adm_small_models == model_name))) +
    coord_fixed() +
    theme_classic() +
    facet_wrap(~seed, nrow = 2) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 7))

ggsave("figures/peak_scatter_scen4.png", scen4_peaks, width = 20, units = "cm")


scen4_first <-
  first_cases %>% 
  filter(model == "raw" | model == "g2_alt") %>%
  pivot_wider(id_cols = c(patch, seed),
              names_from = model,
              values_from = median) %>% 
  ggplot() +
  geom_point(aes(x = raw, y = g2_alt), size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to first case with observed mobility (days)") +
  ylab("Time to first case with\npredicted mobility (days)") +
  scale_x_continuous(limits = c(0, 115),
                     breaks = seq(0, 125, 25)) +
  scale_y_continuous(limits = c(0, 115),
                     breaks = seq(0, 125, 25)) +
  coord_fixed() +
  # ggtitle(model_name) +
  theme_classic() +
  facet_wrap(~seed, nrow = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 7))
  
ggsave("figures/first_case_scatter_scen4.png", scen4_first, width = 20, units = "cm")



## Scenario 4 with aggregated data

scen4big_first_case <-
  first_cases %>% 
  filter(model == "raw_aggr" | model == "g2_aggr_alt") %>%
  pivot_wider(id_cols = c(patch, seed),
              names_from = model,
              values_from = median) %>% 
  ggplot() +
  geom_point(aes(x = raw_aggr, y = g2_aggr_alt), size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to first case with observed mobility (days)") +
  ylab("Time to first case using\npredicted mobility (days)") +
  scale_x_continuous(limits = c(0, 115),
                     breaks = seq(0, 125, 25)) +
  scale_y_continuous(limits = c(0, 115),
                     breaks = seq(0, 125, 25)) +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~seed, nrow = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 7))

ggsave("figures/first_case_scatter_scen4big.png", scen4big_first_case, width = 20, units = "cm")



scen4big_peaks <-
  results %>% 
  filter(model == "raw_aggr" | model == "g2_aggr_alt") %>%
  pivot_wider(id_cols = c(patch, seed),
              names_from = model,
              values_from = median) %>%
  ggplot() +
  geom_point(aes(x = raw_aggr, y = g2_aggr_alt), size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to peak with observed mobility (days)") +
  ylab("Time to peak with\npredicted mobility (days)") +
  scale_x_continuous(limits = c(150, 240),
                     breaks = seq(150, 230, 20)) +
  scale_y_continuous(limits = c(150, 240),
                     breaks = seq(150, 230, 20)) +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~seed, nrow = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_text(size = 7))

ggsave("figures/peak_scatter_scen4big.png", scen4big_peaks, width = 20, units = "cm")


## SPearman rank tests -------

##small models

## first case

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

## peak time
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

## big models

##first case

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

## peak time
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







