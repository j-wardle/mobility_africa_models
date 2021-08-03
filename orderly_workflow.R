library(glue)
library(purrr)

folder <- "~/cluster/mobility_africa_210728/" # this is where simulation results are saved
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
