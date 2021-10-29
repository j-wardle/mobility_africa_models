library(glue)
library(purrr)

a <- orderly::orderly_run("process_location_data")
orderly::orderly_commit(a)
a <- orderly::orderly_run("process_movement_data")
orderly::orderly_commit(a)

orderly::orderly_run("fit_mobility_models")

folder <- "~/cluster/mobility_africa_210903_rad/" # this is where simulation results are saved
files <- list.files(folder)

walk(
  files, function(file) {
    orderly::orderly_run(
      "process_sim_outputs",
      parameters = list(
        folder = folder,
        filename = file
      ), use_draft = "newer"
    )
  }
)

walk(
  files, function(file) {
    orderly::orderly_run(
      "produce_sim_metrics",
      parameters = list(
        folder = folder,
        filename = file
      ), use_draft = "newer"
    )
  }
)

source("orderly-helper-scripts/dependencies_for_collate_task.R")

orderly::orderly_run("collate_outputs",
                     parameters = list(folder = folder),
                     use_draft = "newer")


orderly::orderly_run("produce_figures",
                     parameters = list(folder = folder,
                                       spatial_res = "high"),
                     use_draft = "newer")


orderly::orderly_run("peak_figures",
             parameters = list(spatial_res = "high",
                               scenario_number = 4,
                               pathogen1_folder = "~/cluster/mobility_africa_210512/",
                               pathogen2_folder  = "~/cluster/mobility_africa_210728/"
             ),
             use_draft = "newer")
