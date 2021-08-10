compare_patches <- function(first_cases_data, simulation1, simulation2, seed_name, n) {
  
  sim1_patches <- first_cases_data %>% 
    filter(model == simulation1 & seed == seed_name & patch_name != seed_name) %>% 
    arrange(median, `95%CrI`) %>% 
    slice_head(n = n)
    # slice_min(median, n = n) %>%
    # arrange(median)
  
  sim2_patches <- first_cases_data %>%
    filter(model == simulation2 & seed == seed_name & patch_name != seed_name) %>%
    arrange(median, `95%CrI`) %>% 
    slice_head(n = n)
    # slice_min(median, n = n) %>%
    # arrange(median)
  
  match_numb <- length(sim2_patches$patch_name[sim2_patches$patch_name %in% sim1_patches$patch_name])

  match_prop <- match_numb / length(sim1_patches$patch_name)

  out <- data.frame(n = n,
                    sim1_patches = length(sim1_patches$patch_name),
                    sim2_patches = length(sim2_patches$patch_name),
                    match_prop = match_prop,
                    match_numb = match_numb
  )
  
  out
  
}
