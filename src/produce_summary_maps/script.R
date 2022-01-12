## orderly::orderly_develop_start(use_draft = "newer")
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
## Africa shape file from http://geoportal.icpac.net/layers/geonode%3Aafr_g2014_2013_0
## Open Data Commons Open Database License / OSM (ODbL/OSM)
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
## TODO empricial data is missing one datasource type (check Epidemics7 poster for comparison)
p1 <- map_data_availability(emp_data_features_grp) +
  ggtitle("Empirical data on human movement")

p2 <- map_data_availability(est_data_features_grp) +
  ggtitle("Human movement estimates")


ggsave("empirical.png", p1)
ggsave("estimated.png", p2)


x <- select(data_features, bibkey, data_category)
x <- distinct(x)
count(x, data_category)

## Within studies reporting empirical data,
## what were the data types
emp <- filter(data_features, data_category == "empirical")
## table(emp$datasource_type)

##       cdr    census      GBMD      hdss interview     ipums     unhcr
##        11         2        42         3         6        33        51
## GBMD is Global Bilateral Migration Database
est <- filter(data_features, data_category != "empirical")
## table(est$datasource_type)

##               census data_other_countries      estimates_other
##                    1                   60                    2
##      flight_capacity           flowminder              genomic
##                   52                   14                   33
##            incidence         social_media
##                    8                    2
other_cntry <- filter(data_features, datasource_type == "data_other_countries")
## unique(other_cntry$bibkey)
## [1] "sorichetta2016mapping"    "wesolowski2014commentary"
