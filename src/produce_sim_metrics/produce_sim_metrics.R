## Script to estimate summary metrics for each set of simulated data

results_prcsd <- readRDS("results_prcsd.rds")

# Time to peak

peak_times <- time_to_peak(results_prcsd$sim_results)
peak_times$model <- results_prcsd$movement
peak_times$seed <- results_prcsd$seed_name

saveRDS(peak_times, "time_to_peak.rds")

# Time to first case

first_case <- time_to_first_infection(results_prcsd$sim_results)
first_case$model <- results_prcsd$movement
first_case$seed <- results_prcsd$seed_name

saveRDS(first_case, "first_case.rds")