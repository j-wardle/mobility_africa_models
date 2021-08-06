compare_patches <- function(first_cases_data, simulation1, simulation2, seed, n) {
  
  sim1_patches <- first_cases_data %>% 
    filter(model == simulation1 & seed == seed & patch_name != seed) %>% 
    slice_min(median, n = n) %>%
    arrange(median)
  
  sim2_patches <- first_cases_data %>% 
    filter(model == simulation2 & seed == seed & patch_name != seed) %>% 
    slice_min(median, n = n) %>%
    arrange(median)
  
  match_numb <- length(initial_model$patch_name[initial_model$patch_name %in% initial_patches_obs[[y]]$patch_name])
  
  match_prop <- match_numb / length(initial_patches_obs[[y]]$patch_name)
  
  match_prop
  
}