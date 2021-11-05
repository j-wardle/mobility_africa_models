library(glue)
library(purrr)
library(orderly)

a <- orderly::orderly_run("process_location_data")
orderly::orderly_commit(a)
a <- orderly::orderly_run("process_movement_data")
orderly::orderly_commit(a)

a <- orderly::orderly_run("fit_mobility_models")
orderly_commit(a)

# folder <- "~/cluster/mobility_africa_210903_rad/" # this is where simulation results are saved
folder <- "~/cluster/mobility_africa_211029/" # Simulations for smallest spatial scale. Pathogen 1.
files <- list.files(folder)

commit <- TRUE # set this to define if task output should be committed to archive
walk(
  files, function(file) {
    a <- orderly::orderly_run(
      "process_sim_outputs",
      parameters = list(
        folder = folder,
        filename = file
      ) #, use_draft = "newer" # choose if using draft or archived dependencies
    )
    if (commit) orderly_commit(a)
  }
)

commit <- TRUE # set this to define if task output should be committed to archive
walk(
  files, function(file) {
    a <- orderly::orderly_run(
      "produce_sim_metrics",
      parameters = list(
        folder = folder,
        filename = file
      ) #, use_draft = "newer" # choose if using draft or archived dependencies
    )
    if (commit) orderly_commit(a)
  }
)

source("orderly-helper-scripts/dependencies_for_collate_task.R")

a <- orderly::orderly_run("collate_outputs",
                     parameters = list(folder = folder) #,
                     # use_draft = "newer"
                     )
orderly_commit(a)


orderly::orderly_run("produce_figures",
                     parameters = list(folder = folder,
                                       spatial_res = "high"),
                     use_draft = "newer")


## Generate each individual figure

a <- orderly::orderly_run("mobility_figures",
                     parameters = list(spatial_res = "high",
                                       scenario_number = 4)
)
orderly_commit(a)

a <- orderly::orderly_run("invasion_order_figures",
                     parameters = list(country = "france",
                                       spatial_res = "high",
                                       scenario_number = 4,
                                       pathogen1_folder = "~/cluster/mobility_africa_211029/",
                                       pathogen2_folder  = "~/cluster/mobility_africa_211029/"
                     )
)
orderly_commit(a)

a <- orderly::orderly_run("invasion_order_figures",
                          parameters = list(country = "portugal",
                                            spatial_res = "high",
                                            scenario_number = 4,
                                            pathogen1_folder = "~/cluster/mobility_africa_211029/",
                                            pathogen2_folder  = "~/cluster/mobility_africa_211029/"
                          )
)
orderly_commit(a)





orderly::orderly_run("peak_figures",
             parameters = list(country = "france",
                               spatial_res = "high",
                               scenario_number = 4,
                               pathogen1_folder = "~/cluster/mobility_africa_211029/",
                               pathogen2_folder  = "~/cluster/mobility_africa_211029/"
             )#,
             # use_draft = "newer"
             )

orderly::orderly_run("peak_figures",
                     parameters = list(country = "portugal",
                                       spatial_res = "high",
                                       scenario_number = 4,
                                       pathogen1_folder = "~/cluster/mobility_africa_211029/",
                                       pathogen2_folder  = "~/cluster/mobility_africa_211029/"
                     )#,
                     # use_draft = "newer"
)


orderly::orderly_run("first_case_figures",
                     parameters = list(country = "france",
                                       spatial_res = "high",
                                       scenario_number = 4,
                                       pathogen1_folder = "~/cluster/mobility_africa_211029/",
                                       pathogen2_folder  = "~/cluster/mobility_africa_211029/"
                     )#,
                     # use_draft = "newer"
)

orderly::orderly_run("first_case_figures",
                     parameters = list(country = "portugal",
                                       spatial_res = "high",
                                       scenario_number = 4,
                                       pathogen1_folder = "~/cluster/mobility_africa_211029/",
                                       pathogen2_folder  = "~/cluster/mobility_africa_211029/"
                     )#,
                     # use_draft = "newer"
)


orderly::orderly_run("invasion_maps",
                     parameters = list(spatial_res = "high",
                                       scenario_number = 4,
                                       pathogen1_folder = folder #,
                                       # pathogen2_folder  = "~/cluster/mobility_africa_210728/"
                     ) #,
                     # use_draft = "newer"
                     )
