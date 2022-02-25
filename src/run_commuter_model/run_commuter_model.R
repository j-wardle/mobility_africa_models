## Currently running this script separately on cluster
## To do: speak to Rich about running orderly project on cluster

## load movement data

raw <- readRDS("data/scaled_matrix.rds")
raw_aggregated <- readRDS("data/aggregated_scaled_matrix.rds")
gravity1 <- readRDS("data/gravity_matrix1_numbers.rds")
gravity2 <- readRDS("data/gravity_matrix2_numbers.rds")


## model parameters
beta <- 0.6
f <- 1 / 1
gamma <- 1 / 2
dt <- 0.1 # days
run_time <- 300 # days
seed_number <- 5

compartments <- c("susceptible", "exposed", "infected", "recovered")


## Generalized commuter model as function
## This is the function to run - it uses the other functions defined below

commuter_model <- function(sim, seed_location, movement_matrix) {
  
  chunk_days <-  1
  n_step <- chunk_days / dt
  steps <- seq(1:(n_step + 1))
  movement_matrix <- round(movement_matrix)
  
  results <- model_in_chunks(movement_matrix, run_time, chunk_days = 1,
                             seed_location, seed_number, n_step, steps)
  results
  
}


## SEIR functions
## There is one function for infection dynamics while people are in their home locations
## And a second function for the dynamics when people are in their work locations
## Home and work locations are defined in the movement data matrices (lines 6:9)
## Input is a 3D array (state) with dimensions c(number_patches, number_patches, 4)
## The 4 comes from the number of infection compartments

seir_step_home <- function(state, number_patches) {
  
  # get total people living in each location
  N <- rowSums(state[,,"susceptible"]) + rowSums(state[,,"exposed"]) +
    rowSums(state[,,"infected"]) + rowSums(state[,,"recovered"])
  
  # extract numbers in each compartment as a matrix
  S <- state[,,"susceptible"]
  E <- state[,,"exposed"]
  I <- state[,,"infected"]
  R <- state[,,"recovered"]
  
  # calculate how many infectious people in each resident population
  home_infections <- rowSums(state[,,"infected"])
  
  # calculate probability of each transition and then the numbers moving between compartments
  
  pr_SE <- 1 - exp(- beta * home_infections / N * dt)
  n_SE <- matrix(rbinom(ncol(S)*nrow(S), S, pr_SE), ncol = ncol(S))
  
  pr_EI <- 1 - exp(-f * dt)
  n_EI <- matrix(rbinom(ncol(E)*nrow(E), E, pr_EI), ncol = ncol(E))
  
  pr_IR <- 1 - exp(-gamma * dt)
  n_IR <- matrix(rbinom(ncol(I)*nrow(I), I, pr_IR), ncol = ncol(I))
  
  # calculate new numbers in each compartment
  S <- S - n_SE
  E <- E + n_SE - n_EI
  I <- I + n_EI - n_IR
  R <- R + n_IR
  
  # combine all compartments into a 4D array
  array(c(S, E, I, R), dim = c(number_patches, number_patches, 4))
  
}


seir_step_work <- function(state, number_patches) {
  N <- colSums(state[,,"susceptible"]) + colSums(state[,,"exposed"]) +
    colSums(state[,,"infected"]) + colSums(state[,,"recovered"])
  
  S <- state[,,"susceptible"]
  E <- state[,,"exposed"]
  I <- state[,,"infected"]
  R <- state[,,"recovered"]
  
  work_infections <- colSums(state[,,"infected"])
  
  pr_SE <- 1 - exp(- beta * work_infections / N * dt)
  n_SE <- matrix(rbinom(ncol(S)*nrow(S), as.vector(t(S)), pr_SE), ncol = ncol(S), byrow = TRUE)
  # as.vector(t(S)) ensures that we apply the correct pr_SE to each column
  
  pr_EI <- 1 - exp(-f * dt)
  n_EI <- matrix(rbinom(ncol(E)*nrow(E), E, pr_EI), ncol = ncol(E))
  
  pr_IR <- 1 - exp(-gamma * dt)
  n_IR <- matrix(rbinom(ncol(I)*nrow(I), I, pr_IR), ncol = ncol(I))
  
  S <- S - n_SE
  E <- E + n_SE - n_EI
  I <- I + n_EI - n_IR
  R <- R + n_IR
  
  array(c(S, E, I, R), dim = c(number_patches, number_patches, 4))
  
}

# Function to run SEIR model across multiple time-steps
# i %% 10 tells us if the step is in the first half of the day (when people are at home) or
# the second part of the day (when people are at work)

seir_run <- function(data, n_step, number_patches) {
  
  for (i in 1:n_step) {
    
    if (i %% 10 < 5) {
      
      data[,,, i + 1] <- seir_step_home(data[,,, i], number_patches)
      
    } else {
      
      data[,,, i + 1] <- seir_step_work(data[,,, i], number_patches)
      
    }
  }
  
  data
}


# Function to tidy outputs because they can get large v quickly
# Keeps one set of state numbers per day (rather than for every time-step)
seir_tidydata <- function(data, steps) {
  
  results <- as.data.frame.table(data[,,,seq(1, length(steps), by = 10)]) %>%
    dplyr::rename(home = Var1,
                  work = Var2,
                  compartment = Var3,
                  time = Var4,
                  value = Freq)
  
  results$time <- as.numeric(results$time)
  
  results <- tidyr::pivot_wider(results,
                                id_cols = c("home", "work", "time"),
                                names_from = "compartment",
                                values_from = "value") %>%
    dplyr::group_by(time, home) %>%
    dplyr::summarise(across(susceptible:recovered, sum)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(time != max(time))
  
  results
  
}


## Function to run the model in chunks of days
## Implemented to resolve issues with running out of memory when running model
## for large number of days at once 
model_in_chunks <- function(movement_matrix, run_time, chunk_days = 20,
                            seed_location, seed_number, n_step, steps) {
  
  number_patches <- nrow(movement_matrix)
  array_rows <- rownames(movement_matrix)
  array_cols <- colnames(movement_matrix)
  
  number_of_chunks <- ceiling(run_time/ chunk_days)
  
  # Generate arrays in default twenty day chunks due to large size
  # Set up initial array
  chunk_array <- array(0, dim = c(number_patches, number_patches,
                                  length(compartments),
                                  n_step + 1),
                       dimnames = list(array_rows, array_cols, compartments, steps))
  
  # Populate array based on numbers in movement_matrix
  chunk_array[,,"susceptible", 1] <- movement_matrix
  
  # Seed infections
  chunk_array[seed_location, seed_location, "infected", 1] <- seed_number
  
  # Set up list for outputs
  out <- vector("list", length = number_of_chunks)
  
  # Run model for first chunk of days
  out[[1]] <- seir_run(chunk_array, n_step = n_step, number_patches = number_patches)
  
  # Run model for subsequent chunks
  for (i in 2:number_of_chunks) {
    
    chunk_array <- array(0, dim = c(number_patches, number_patches,
                                    length(compartments),
                                    n_step + 1),
                         dimnames = list(array_rows, array_cols, compartments, steps))
    
    # The first array of each chunk is populated using the finishing state of the previous chunk
    chunk_array[,,, 1] <- out[[i-1]][,,,n_step + 1]
    
    # Reduce the amount of output we save in previous step (keep one set of numbers per day)
    out[[i-1]] <- seir_tidydata(out[[i-1]], steps = steps)
    
    # Store the time of the previous output
    out[[i-1]][["time"]] <- out[[i-1]][["time"]] + chunk_days*(i-2)
    
    # Run model for new chunk
    out[[i]] <- seir_run(chunk_array, n_step = n_step, number_patches = number_patches)
    
    
  }
  
  out[[number_of_chunks]] <- seir_tidydata(out[[number_of_chunks]], steps = steps)
  
  out[[number_of_chunks]][["time"]] <- out[[number_of_chunks]][["time"]] + chunk_days*(number_of_chunks-1)
  
  out <- dplyr::bind_rows(out)
  
}


# Get results from cluster

get_results <- function(bundle_name) {
  
  tb <- obj$task_bundle_get(bundle_name)
  idx <- which(tb$status() == "COMPLETE")
  
  results <- vector("list", length = length(idx))
  
  for (tid in names(idx)) {
    
    i <- idx[[tid]]
    
    message("Working on sim ", i)
    
    results[[i]] <- obj$task_result(task_id = tid)
    
  }
  
  results_all <- dplyr::bind_rows(results, .id = "sim")
  results_all$sim <- as.numeric(results_all$sim)
  results_all$time <- as.numeric(results_all$time)
  
  results_all
  
}
