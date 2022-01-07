## orderly::orderly_develop_start()
## ISO3 codes for all countries in Africa.
## List of all countries in Africa
africa <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF",
  "TCD", "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI",
  "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR",
  "LBY", "MDG", "MWI", "MLI", "MRT", "MAR", "MOZ", "NAM", "NER",
  "NGA", "RWA", "STP", "SEN", "SLE", "SOM", "ZAF", "SDN", "SWZ",
  "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE"
)

est_data_loc <- read_csv("estimated_data_locations_scale.csv")
emp_data_loc <- read_csv("empirical_data_locations_scale.csv")
data_features <- read_csv("study_ds_types.csv")
est_data_features <- prepare_data_features(
  est_data_loc, data_features, "estimated"
)
emp_data_features <- prepare_data_features(
  emp_data_loc, data_features, "empirical"
)

emp_data_features_grp <- mutate_at(
  emp_data_features,
  "datasource_type",
  function(x) {
    x <- case_when(
      x == "cdr" ~ "cdr",
      x %in% c("ipums", "census", "interview", "hdss") ~ "census",
      x %in% c("GBMD", "unhcr") ~ "unhcr"
    )
  }
)

 est_data_features_grp <- mutate_at(
   est_data_features,
   "datasource_type",
   function(x) {
     x <- case_when(
       x %in% c("genomic", "incidence") ~ "incidence",
       x %in% c("data_other_countries", "estimates_other", "flowminder") ~ "estimates_other",
       x == "social_media" ~ "social_media",
       x == "flight_capacity" ~ "flight_capacity",
       x == "census" ~ "census"
     )
   }
 )

p1 <- map_data_availability(emp_data_features_grp)
