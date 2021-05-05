
## Create folder to save figures
dir.create("figures")

results2 <- tidyr::pivot_wider(results,
                               id_cols = patch,
                               names_from = model,
                               values_from = `0.5`)

results2$patch_name <- location_data$location
