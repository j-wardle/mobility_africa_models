get_centroid <- function(sf_obj, name_col) {

    geoms <- sf::st_geometry(sf_obj)
    centroids <- sf::st_centroid(geoms)
    coords <- sf::st_coordinates(centroids)
    out <- data.frame(
        location = sf_obj[[name_col]],
        country = unique(sf_obj[["GID_0"]]),
        long = coords[, 1],
        lat = coords[ , 2]
    )
    out
}

## Join data locations recorded in
## data/estimated_data_locations_scale.csv and
## data/empirical_data_locations_scale.csv with
## and type availabilty recorded in data/study_ds_types.csv
## category must be either empirical or estimated
prepare_data_features <- function(data_locations,
                                  features,
                                  category = c("empirical", "estimated")) {

    match.arg(category)

    features <- features[features$data_category == category, ]
    out <- left_join(
      data_locations, features, by = c("bibkey", "country")
    )
    out <- out[out$country %in% africa, ]

    ## Rearrange factor levels so that legends come out OK.
    out$datasource_type <- factor(
        out$datasource_type,
        levels = c(
          "cdr", "social_media", "ipums", "census", "interview", "hdss",
          "GBMD", "unhcr", "genomic", "flight_capacity",
          "data_other_countries", "incidence", "estimates_other",
          "flowminder"
        ),
        ordered = TRUE
    )
    out
}


normalised_flow_matrix <- function(pops, distances, params) {

    out <- mover::flow_matrix(pops, distances, params)
    out <- out / pops
    diag(out) <- 1 - rowSums(out, na.rm = TRUE)

    out



}
