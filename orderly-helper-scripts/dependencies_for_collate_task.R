# library(glue)
# library(purrr)
# 
# folder <- "~/cluster/mobility_africa_210428/"
# files <- list.files(folder)
## Move code above this line to workflow script when I make one

## Code to automatically generate the orderly.yml file containing all the model outputs we run

x <- list(
  script = "collate_outputs.R",
  artefacts = list(
    data = list(
      description = "Collated summary metrics for all locations",
      filenames = c(
        "collated_time_to_peak.rds",
        "collated_time_to_first_case.rds",
        "collated_first_case_all_sims.rds"
      )
    )
  ),
  parameters = "folder",
  packages = c("dplyr", "tidyr", "purrr", "tibble")
)

queries <- glue(
  "latest(parameter:filename == \"{files}\" ",
  "&& parameter:folder == \"{folder}\")"
)

d1 <- map2(
  files, queries, function(file, query) {
    y <- list(
      produce_sim_metrics = list(
        id = query,
        use =  list(
          "time_to_peak.rds",
          "first_case.rds",
          "firstcase_allsims.rds"
        )
      )
    )
    infiles <- map(
      y$produce_sim_metrics$use,
      function(x) strsplit(x, split = ".", fixed = TRUE)[[1]][1]
    )
    names(y$produce_sim_metrics$use) <- glue("{infiles}_{file}.rds")
    y
  }
)

x$depends <- d1

con <- file(
  here::here("src/collate_outputs/orderly.yml"), "w"
)
yaml::write_yaml(x, con)
close(con)
