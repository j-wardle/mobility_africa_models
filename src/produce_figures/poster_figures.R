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

plot5 <- 
  results %>% 
  filter(model == "raw" | model == "g2_alt") %>%
  pivot_wider(id_cols = c(patch, seed),
              names_from = model,
              values_from = median) %>% 
  ggplot() +
  geom_point(aes(x = raw, y = g2_alt), size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to peak with observed mobility (days)") +
  ylab("Time to peak with predicted mobility (days)") +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~seed, nrow = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA))


plot6 <-
  first_cases %>% 
  filter(model == "raw" | model == "g2_alt") %>%
  pivot_wider(id_cols = c(patch, seed),
              names_from = model,
              values_from = median) %>% 
  ggplot() +
  geom_point(aes(x = raw, y = g2_alt), size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 2) +
  xlab("Time to first case using observed mobility (days)") +
  ylab("Time to first case using\npredicted mobility (days)") +
  scale_x_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 25)) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 25)) +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~seed, nrow = 1) +
  theme(panel.border = element_rect(colour = "black", fill = NA))

ggsave("figures/first_case_scatter_g2_alt.png", plot6)
