## Functions for running SEIR commuter model on cluster


# SEIR steps during 'home' periods ----------------------------------------


seir_step_home <- function(state, number_patches) {
  N <- rowSums(state[,,"susceptible"]) + rowSums(state[,,"exposed"]) +
    rowSums(state[,,"infected"]) + rowSums(state[,,"recovered"])
  
  S <- state[,,"susceptible"]
  E <- state[,,"exposed"]
  I <- state[,,"infected"]
  R <- state[,,"recovered"]
  
  home_infections <- rowSums(state[,,"infected"])
  
  pr_SE <- 1 - exp(- beta * home_infections / N * dt)
  n_SE <- matrix(rbinom(ncol(S)*nrow(S), S, pr_SE), ncol = ncol(S))
  
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


# SEIR steps during 'work' periods ----------------------------------------


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


# Run SEIR model ----------------------------------------------------------


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


# Tidy SEIR model output --------------------------------------------------


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


# Run model in chunks -----------------------------------------------------
# To address issues with memory size

model_in_chunks <- function(movement_matrix, run_time, chunk_days = 20,
                            seed_location, seed_number, n_step, steps) {
  
  number_patches <- nrow(movement_matrix)
  array_rows <- rownames(movement_matrix)
  array_cols <- colnames(movement_matrix)
  
  number_of_chunks <- ceiling(run_time/ chunk_days)
  
  # Generate arrays in default fifty day chunks due to large size
  chunk_array <- array(0, dim = c(number_patches, number_patches,
                                  length(compartments),
                                  n_step + 1),
                       dimnames = list(array_rows, array_cols, compartments, steps))
  
  chunk_array[,,"susceptible", 1] <- movement_matrix
  chunk_array[seed_location, seed_location, "infected", 1] <- seed_number
  
  out <- vector("list", length = number_of_chunks)
  
  out[[1]] <- seir_run(chunk_array, n_step = n_step, number_patches = number_patches)
  
  for (i in 2:number_of_chunks) {
    
    chunk_array <- array(0, dim = c(number_patches, number_patches,
                                    length(compartments),
                                    n_step + 1),
                         dimnames = list(array_rows, array_cols, compartments, steps))
    
    chunk_array[,,, 1] <- out[[i-1]][,,,n_step + 1]
    
    out[[i-1]] <- seir_tidydata(out[[i-1]], steps = steps)
    
    out[[i-1]][["time"]] <- out[[i-1]][["time"]] + chunk_days*(i-2)
    
    out[[i]] <- seir_run(chunk_array, n_step = n_step, number_patches = number_patches)
    
    
  }
  
  out[[number_of_chunks]] <- seir_tidydata(out[[number_of_chunks]], steps = steps)
  
  out[[number_of_chunks]][["time"]] <- out[[number_of_chunks]][["time"]] + chunk_days*(number_of_chunks-1)
  
  out <- dplyr::bind_rows(out)
  
}



# Get results from cluster ------------------------------------------------

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