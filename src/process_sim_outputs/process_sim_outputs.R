## Script for getting simulation results from cluster set up in correct format for figure generation

if (isTRUE(startsWith(filename, "prtl"))) {

## Pull through filenames and read in results
prtl_files <- list.files("~/cluster/mobility_africa_210428", pattern = "^prtl")
prtl_files <- paste0("~/cluster/mobility_africa_210428/", prtl_files)

prtl_results <- map(prtl_files, function(filename) {
  
  out <- readRDS(filename)
  out <- rename(out, patch = home)
  out$patch <- as.numeric(out$patch)
  
  out  
  
})

names(prtl_results) <- file_path_sans_ext(
  list.files("~/cluster/mobility_africa_210428", pattern = "^prtl")
)

## Extract scenario info from filename (eg seed_name, movement type etc)
prtl_results <- imap(prtl_results, function(scenario, name) {
  
  out <- vector("list")
  out[["sim_results"]] <- scenario
  out[["seed_name"]] <- word(name, -1, sep = fixed('_'))
  out[["movement"]] <- sub("_[^_]+$", "", name)
  out
  
})

## Identify the numerical value of the seed location
prtl_results_prcsd <- map(prtl_results, function(scenario) {
  
  # load appropriate location data
  if (isTRUE((grepl("aggr", scenario[["movement"]], fixed=TRUE)))) {
    location_data <- readRDS("portugal_adm1_location_data.rds")
  } else {
    location_data <- readRDS("portugal_location_data.rds")
  }
  
  # look up seed location
  if(scenario[["seed_name"]] == "mdd" &
     isTRUE(grepl("aggr", scenario[["movement"]], fixed=TRUE))) {
    seed_location <- which(location_data[,"location"] == "BRAGANCA")
  } else if (scenario[["seed_name"]] == "mdd") {
    seed_location <- which(location_data[,"location"] == "MIRANDA_DO_DOURO")
  } else {
    seed_location <- which(location_data[,"location"] == "LISBOA")
  }
  
  scenario[["seed_location"]] <- seed_location
  scenario
  
})

## Define whether epidemic was successfully seeded

prtl_results_prcsd <- map(prtl_results_prcsd, function(scenario) {
  
  scenario[["sim_results"]] <-  add_epidemic_status(scenario[["sim_results"]],
                                                    seed_location = scenario[["seed_location"]])
  scenario
})

## Save the processed output

saveRDS(prtl_results_prcsd, "results_prcsd.rds")

} else if (country_name == "France") {


# FRANCE ------------------------------------------------------------------

## Pull through filenames and read in results
fra_files <- list.files("~/cluster/mobility_africa_210428", pattern = "^fra")
fra_files <- paste0("~/cluster/mobility_africa_210428/", fra_files)

fra_results <- map(fra_files, function(filename) {
  
  out <- readRDS(filename)
  out <- rename(out, patch = home)
  out$patch <- as.numeric(out$patch)
  
  out  
  
})

names(fra_results) <- file_path_sans_ext(
  list.files("~/cluster/mobility_africa_210428", pattern = "^fra")
)

## Extract scenario info from filename (eg seed_name, movement type etc)
fra_results <- imap(fra_results, function(scenario, name) {
  
  out <- vector("list")
  out[["sim_results"]] <- scenario
  out[["seed_name"]] <- word(name, -1, sep = fixed('_'))
  out[["movement"]] <- sub("_[^_]+$", "", name)
  out
  
})

## Identify the numerical value of the seed location
fra_results_prcsd <- map(fra_results, function(scenario) {
  
  # load appropriate location data
  if (isTRUE((grepl("aggr", scenario[["movement"]], fixed=TRUE)))) {
    location_data <- readRDS("france_adm2_location_data.rds")
  } else {
    location_data <- readRDS("france_location_data.rds")
  }
  
  # look up seed location
  if(scenario[["seed_name"]] == "bre" &
     isTRUE(grepl("aggr", scenario[["movement"]], fixed=TRUE))) {
    seed_location <- which(location_data[,"location"] == "FINISTERE")
  } else if (scenario[["seed_name"]] == "bre") {
    seed_location <- which(location_data[,"location"] == "BREST")
  } else {
    seed_location <- which(location_data[,"location"] == "PARIS")
  }
  
  scenario[["seed_location"]] <- seed_location
  scenario
  
})

## Define whether epidemic was successfully seeded

fra_results_prcsd <- map(fra_results_prcsd, function(scenario) {
  
  scenario[["sim_results"]] <-  add_epidemic_status(scenario[["sim_results"]],
                                                    seed_location = scenario[["seed_location"]])
  scenario
})

## Save the processed output

saveRDS(fra_results_prcsd, "results_prcsd.rds")

}