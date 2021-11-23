if (! is.null(dev.list())) dev.off()

# MOBILITY SCATTER PLOTS --------------------------------------------------

## Create folder to save figures
dir.create("figures")

## Load mobility matrices
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

# Filter out the predictions we don't need
scenario_to_plot <- paste0("Scenario ", scenario_number)

combined_preds <- bind_rows(france_predictions, prt_predictions) %>% 
  filter(scenario == scenario_to_plot)

# Create simple scatter plot of both countries

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
  facet_wrap(~ country, nrow = 1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))
combined_plot

ggsave("figures/comb_mob_predictions.png", combined_plot, scale  = 2)


# combined_preds <- combined_preds %>% 
#   mutate(overestimate = ifelse(predicted > observed, 1, 0))
# 
# combined_preds %>% 
#   group_by(country, scenario) %>% 
#   summarise(propn = sum(overestimate) / n())


## Create simple scatter plot of both countries with summary overlaid

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

combined_plot_with_bins <-
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
  facet_wrap(~ country, nrow = 2) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16))

combined_plot_with_bins

ggsave("figures/comb_mob_predictions_errorbar.png", combined_plot_with_bins, scale  = 2)



## Individual country plots

## FRANCE

france_plot_with_bins <-
  summary_overlay %>% 
  filter(country == "FRANCE") %>% 
  ggplot(aes(observed, predicted)) +
  geom_point(size = 0.8, shape = 1, alpha = 0.2, colour = palette[1]) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = 2) +
  geom_vline(xintercept = bin_divides, colour = "grey", linetype = 2) +
  geom_errorbar(data = . %>% filter(group_count == 1),
                aes(x = bin_median, ymin = bin_pred_lo, ymax = bin_pred_hi),
                width = 0.5, size = 1) +
  geom_point(data = . %>% filter(group_count == 1),
             aes(x = bin_median, y = bin_pred_mid),
             size = 1.5) +
  xlab("Observed movement") +
  ylab("Predicted movement") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # coord_fixed() +
  facet_wrap(~ country, nrow = 1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  stat_cor(aes_string(label = "..rr.label.."),
           color = "red", geom = "text", size = 3)

france_plot_with_bins

ggsave("figures/france_mob_predictions_errorbar.png", france_plot_with_bins) #, scale  = 2)

knitr::plot_crop("figures/france_mob_predictions_errorbar.png")

# Save ggplot object for use in creating panel
saveRDS(france_plot_with_bins, file = "figures/france_plot_with_bins.rds")

## PORTUGAL

portugal_plot_with_bins <-
  summary_overlay %>% 
  filter(country == "PORTUGAL") %>% 
  ggplot(aes(observed, predicted)) +
  geom_point(size = 0.8, shape = 1, alpha = 0.2, colour = palette[5]) +
  geom_abline(intercept = 0, slope = 1, colour = "red", linetype = 2) +
  geom_vline(xintercept = bin_divides, colour = "grey", linetype = 2) +
  geom_errorbar(data = . %>% filter(group_count == 1),
                aes(x = bin_median, ymin = bin_pred_lo, ymax = bin_pred_hi),
                width = 0.5, size = 1) +
  geom_point(data = . %>% filter(group_count == 1),
             aes(x = bin_median, y = bin_pred_mid),
             size = 1.5) +
  xlab("Observed movement") +
  ylab("Predicted movement") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # coord_fixed() +
  facet_wrap(~ country, nrow = 1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16)) +
  stat_cor(aes_string(label = "..rr.label.."),
           color = "red", geom = "text", size = 3)

  portugal_plot_with_bins

ggsave("figures/portugal_mob_predictions_errorbar.png", portugal_plot_with_bins) #, scale  = 2)

knitr::plot_crop("figures/portugal_mob_predictions_errorbar.png")

# Save ggplot object for use in creating panel
saveRDS(portugal_plot_with_bins, file = "figures/portugal_plot_with_bins.rds")

# # EXPERIMENTAL PLOT: all movement points, but highlight those originating in Paris
# 
# # france_plot_with_paris <-
#   summary_overlay %>% 
#   filter(country == "FRANCE") %>% 
#   ggplot(aes(observed, predicted)) +
#   geom_point(size = 0.8, shape = 1, alpha = 0.2) +
#   geom_abline(intercept = 0, slope = 1, colour = "red", linetype = 2) +
#   # geom_vline(xintercept = bin_divides, colour = "grey", linetype = 2) +
#   # geom_errorbar(data = . %>% filter(group_count == 1),
#   #               aes(x = bin_median, ymin = bin_pred_lo, ymax = bin_pred_hi),
#   #               width = 0.5, size = 1) +
#   geom_point(data = . %>% filter(origin == "PARIS"),
#              aes(x = observed, y = predicted),
#              colour = "red", size = 0.8, shape = 1) +
#   coord_fixed() +
#   xlab("Observed movement") +
#   ylab("Predicted movement") +
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   facet_wrap(~ country, nrow = 1) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         strip.text = element_text(size = 16))
#   
#   summary_overlay %>% 
#     filter(country == "FRANCE") %>% 
#     ggplot(aes(observed, predicted)) +
#     geom_point(size = 0.8, shape = 1, alpha = 0.2) +
#     geom_abline(intercept = 0, slope = 1, colour = "red", linetype = 2) +
#     # geom_vline(xintercept = bin_divides, colour = "grey", linetype = 2) +
#     # geom_errorbar(data = . %>% filter(group_count == 1),
#     #               aes(x = bin_median, ymin = bin_pred_lo, ymax = bin_pred_hi),
#     #               width = 0.5, size = 1) +
#     geom_point(data = . %>% filter(origin == "BREST"),
#                aes(x = observed, y = predicted),
#                colour = "red", size = 0.8, shape = 1) +
#     coord_fixed() +
#     xlab("Observed movement") +
#     ylab("Predicted movement") +
#     scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x))) +
#     scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x))) +
#     facet_wrap(~ country, nrow = 1) +
#     theme_classic() +
#     theme(legend.position = "none",
#           axis.text = element_text(size = 14),
#           axis.title = element_text(size = 16),
#           strip.text = element_text(size = 16))


if (! is.null(dev.list())) dev.off()