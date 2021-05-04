## Functions to estimate sim metrics at patch level

## 1) Times to epidemic peak in each patch

time_to_peak <- function(results_data) {
  
  results_all <- results_data
  
  results_all$population <- results_all$susceptible +
    results_all$exposed +
    results_all$infected +
    results_all$recovered
  results_all$scaled_infections <- 1000 * results_all$infected / results_all$population
  
  results_peak <- 
    results_all %>%
    filter(patch_epidemic_status == 1) %>% 
    group_by(sim, patch) %>%
    mutate(peak_rank_scaled = rank(-scaled_infections, ties.method = "first"),
           peak_rank = rank(-infected, ties.method = "first")) %>% 
    # filter(peak_rank_scaled == 1)
    filter(peak_rank == 1)
  
  peak_times <-
    results_peak %>%
    ungroup() %>% 
    group_by(patch) %>% 
    summarise(peak_time = quantile(time,
                                   c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975))
  
  peak_times <- peak_times %>%
    mutate(peak_time = round(peak_time, 1)) %>% 
    tidyr::pivot_wider(id_cols = patch,
                       names_from = q,
                       values_from = peak_time) %>% 
    select(patch, `0.5`, `0.025`, `0.975`)
  
  peak_times
  
}


## 2) Time to first infection

time_to_first_infection <- function(results_data) {
  
  first_case <- results_data %>%
    filter(seed_success == 1 & infected > 0) %>%
    group_by(sim, patch) %>% 
    slice_head()
  
  first_case <- first_case %>%
    ungroup() %>% 
    group_by(patch) %>% 
    summarise(time_of_case = quantile(time,
                                      c(0.025, 0.5, 0.975)),
              q = c(0.025, 0.5, 0.975))
  
  first_case <- first_case %>%
    mutate(time_of_case = round(time_of_case, 1)) %>% 
    tidyr::pivot_wider(id_cols = patch,
                       names_from = q,
                       values_from = time_of_case) %>% 
    select(patch, `0.5`, `0.025`, `0.975`)
  
  first_case
  
}