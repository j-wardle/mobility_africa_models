## Script for getting simulation results from cluster set up in correct format for figure generation

if (isTRUE(startsWith(filename, "prtl"))) {

## Read in results
  
prtl_results <- readRDS(paste0(folder, filename))
prtl_results <- rename(prtl_results, patch = home)
prtl_results$patch <- as.numeric(prtl_results$patch)

## Extract scenario info from filename (eg seed_name, movement type etc)
prtl_results_prcsd <- vector("list")
prtl_results_prcsd[["sim_results"]] <- prtl_results
prtl_results_prcsd[["seed_name"]] <- word(file_path_sans_ext(filename), -1, sep = fixed('_'))
prtl_results_prcsd[["movement"]] <- sub("_[^_]+$", "", file_path_sans_ext(filename))


## Identify the numerical value of the seed location

# load appropriate location data
  if (isTRUE((grepl("aggr", prtl_results_prcsd[["movement"]], fixed=TRUE)))) {
    location_data <- readRDS("portugal_adm1_location_data.rds")
  } else {
    location_data <- readRDS("portugal_location_data.rds")
  }
  
  # look up seed location
  if(prtl_results_prcsd[["seed_name"]] == "mdd" &
     isTRUE(grepl("aggr", prtl_results_prcsd[["movement"]], fixed=TRUE))) {
    seed_location <- which(location_data[,"location"] == "BRAGANCA")
  } else if (prtl_results_prcsd[["seed_name"]] == "mdd") {
    seed_location <- which(location_data[,"location"] == "MIRANDA_DO_DOURO")
  } else {
    seed_location <- which(location_data[,"location"] == "LISBOA")
  }
  
prtl_results_prcsd[["seed_location"]] <- seed_location
  

## Define whether epidemic was successfully seeded

prtl_results_prcsd[["sim_results"]] <-  add_epidemic_status(prtl_results_prcsd[["sim_results"]],
                                                    seed_location = prtl_results_prcsd[["seed_location"]])

## Save the processed output

saveRDS(prtl_results_prcsd, "results_prcsd.rds")

} else if (isTRUE(startsWith(filename, "fra"))) {

  ## Read in results
  
  fra_results <- readRDS(paste0(folder, filename))
  fra_results <- rename(fra_results, patch = home)
  fra_results$patch <- as.numeric(fra_results$patch)
  
  ## Extract scenario info from filename (eg seed_name, movement type etc)
  fra_results_prcsd <- vector("list")
  fra_results_prcsd[["sim_results"]] <- fra_results
  fra_results_prcsd[["seed_name"]] <- word(file_path_sans_ext(filename), -1, sep = fixed('_'))
  fra_results_prcsd[["movement"]] <- sub("_[^_]+$", "", file_path_sans_ext(filename))
  
  
  ## Identify the numerical value of the seed location
  
  # load appropriate location data
  if (isTRUE((grepl("aggr", fra_results_prcsd[["movement"]], fixed=TRUE)))) {
    location_data <- readRDS("france_adm2_location_data.rds")
  } else {
    location_data <- readRDS("france_location_data.rds")
  }
  
  # look up seed location
  if(fra_results_prcsd[["seed_name"]] == "bre" &
     isTRUE(grepl("aggr", fra_results_prcsd[["movement"]], fixed=TRUE))) {
    seed_location <- which(location_data[,"location"] == "FINISTERE")
  } else if (fra_results_prcsd[["seed_name"]] == "bre") {
    seed_location <- which(location_data[,"location"] == "BREST")
  } else {
    seed_location <- which(location_data[,"location"] == "PARIS")
  }
  
fra_results_prcsd[["seed_location"]] <- seed_location
  
## Define whether epidemic was successfully seeded

fra_results_prcsd[["sim_results"]] <-  add_epidemic_status(fra_results_prcsd[["sim_results"]],
                                                    seed_location = fra_results_prcsd[["seed_location"]])

## Save the processed output

saveRDS(fra_results_prcsd, "results_prcsd.rds")

}