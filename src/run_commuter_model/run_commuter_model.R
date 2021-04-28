## Currently running this script separately on cluster
## To do: speak to Rich about running orderly project on cluster

## load location data
## Portugal
# location_data <- readRDS("data/portugal_location_data.rds")
# location_data_adm1 <- readRDS("data/portugal_adm1_location_data.rds")

## France
location_data <- readRDS("data/france_location_data_2007.rds")

## choose movement type

# # raw movement
# raw_adm2 <- readRDS("data/prtgl_scld_matrix.rds")
# raw_adm2 <- round(raw_adm2)
# 
# ## gravity 1
# g1_adm2 <- readRDS("data/prtgl_gravity_matrix1.rds")
# g1_adm2 <- round(g1_adm2*location_data$population) # convert probabilities to numbers
# 
# ## gravity 2
# g2_adm2 <- readRDS("data/prtgl_gravity_matrix2.rds")
# g2_adm2 <- round(g2_adm2*location_data$population) # convert probabilities to numbers
# 
# ## adm1 raw data
# raw_adm1 <- readRDS("data/prtgl_adm1_norm_scld_matrix.rds")
# raw_adm1 <- round(raw_adm1*location_data_adm1$population) # convert probabilities to numbers
# 
# ## adm1 gravity 2 data
# g2_adm1 <- readRDS("data/prtgl_adm1_gravity_matrix2.rds")
# g2_adm1 <- round(g2_adm1*location_data_adm1$population) # convert probabilities to numbers

## France raw movement
fra_raw_adm3 <- readRDS("data/fra2007_scld_matrix.rds")
fra_raw_adm3 <- round(fra_raw_adm3)

## gravity 2
fra_g2_adm3 <- readRDS("data/fra_gravity_matrix2.rds")
fra_g2_adm3 <- round(fra_g2_adm3*location_data$population) # convert probabilities to numbers

## model parameters
beta <- 1.2
f <- 1 / 1.6
gamma <- 1
dt <- 0.1 # days
run_time <- 300 # days
seed_number <- 5

compartments <- c("susceptible", "exposed", "infected", "recovered")

## Lisbon model as function

lisbon_model <- function(sim, movement_matrix) {
  
  seed_location <- "LISBOA"
  chunk_days <-  1
  n_step <- chunk_days / dt
  steps <- seq(1:(n_step + 1))
  
  results <- model_in_chunks(movement_matrix, run_time, chunk_days = 1,
                             seed_location, seed_number, n_step, steps)
  results
  
}

## MdD model as function

mdd_model <- function(sim, movement_matrix) {
  
  seed_location <- "MIRANDA_DO_DOURO"
  chunk_days <-  1
  n_step <- chunk_days / dt
  steps <- seq(1:(n_step + 1))
  
  results <- model_in_chunks(movement_matrix, run_time, chunk_days = 1,
                             seed_location, seed_number, n_step, steps)
  results
  
}

## Generalized commuter model as function

commuter_model <- function(sim, seed_location, movement_matrix) {
  
  chunk_days <-  1
  n_step <- chunk_days / dt
  steps <- seq(1:(n_step + 1))
  
  results <- model_in_chunks(movement_matrix, run_time, chunk_days = 1,
                             seed_location, seed_number, n_step, steps)
  results
  
}