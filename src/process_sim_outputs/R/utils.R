## Functions for processing simulations

## 1) Function to determine seed_success

add_epidemic_status <- function(results_data, seed_location) {
  
  # First identify the patches with epidemics and the sims with successful seeding
  final_size <- filter(results_data, time == 300)
  
  final_size$population  <-  final_size$susceptible +
    final_size$exposed +
    final_size$infected +
    final_size$recovered
  final_size$epidemic_size  <-  1000 * final_size$recovered / final_size$population
  
  final_size$patch_epidemic_status <- ifelse(final_size$epidemic_size > 100, 1, 0) # using fixed threshold atm
  
  final_size <- final_size %>% 
    group_by(sim) %>%
    mutate(seed_success = patch_epidemic_status[patch == seed_location])
  
  # Now combine info on epidemic status to results_all for future filtering
  
  epidemic_status <- final_size %>% 
    select(sim, patch, seed_success, patch_epidemic_status)
  
  out <- left_join(results_data, epidemic_status, by = c("sim", "patch"))
  out
  
}