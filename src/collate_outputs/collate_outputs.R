# infiles <- list.files(pattern = "*.rds") # use this in task development

collate_outputs <- function(infiles, pattern) {
  model_files <- grep(pattern, infiles, value = TRUE)
  model_outputs <- map(model_files, readRDS)
  
  bind_rows(model_outputs)
  
}


run_info <- orderly::orderly_run_info()
infiles <- run_info$depends$as

peak_times <- collate_outputs(infiles, "time_to_peak")
first_case <- collate_outputs(infiles, "first_case")
firstcase_allsims <- collate_outputs(infiles, "firstcase_allsims")



saveRDS(peak_times, "collated_time_to_peak.rds")
saveRDS(first_case, "collated_time_to_first_case.rds")
saveRDS(firstcase_allsims, "collated_first_case_all_sims.rds")