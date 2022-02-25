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

ds_types <- read_csv("study_ds_types.csv")
## sorichetta2016mapping do make use of empirical data but the data
## are pooled across countries to generate proxies.
## Therefore, datasource_grped is really 'data_other_countries' for them

ds_types$datasource_grped <- case_when(
  ds_types$datasource_type == "cdr" ~ "cdr",
  ds_types$datasource_type == "interview" ~ "interview",
  ds_types$datasource_type %in% c("ipums", "census", "hdss") ~ "census",
  ds_types$datasource_type %in% c("GBMD", "unhcr") ~ "unhcr",
  ds_types$datasource_type %in% c("genomic", "incidence") ~ "incidence",
  ds_types$datasource_type %in% c("data_other_countries") ~ "data_other_countries",
  ds_types$datasource_type %in% c("estimates_other", "flowminder") ~ "estimates_other",
  ds_types$datasource_type == "social_media" ~ "social_media",
  ds_types$datasource_type == "flight_capacity" ~ "flight_capacity"
)


est_data_features <- prepare_data_features(
  est_data_loc, ds_types, "estimated"
)

emp_data_features <- prepare_data_features(
  emp_data_loc, ds_types, "empirical"
)

emp_data_features_grp <- mutate_at(
  emp_data_features,
  "datasource_type",
  function(x) {
    x <- case_when(
      x == "cdr" ~ "cdr",
      x %in% c("ipums", "census", "interview", "hdss") ~ "census",
      x %in% c("GBMD", "unhcr") ~ "unhcr",
      x == "social_media" ~ "social_media"
    )
  }
)

est_data_features_grp <- mutate_at(
   est_data_features,
   "datasource_type",
   function(x) {
     x <- case_when(
       x %in% c("genomic", "incidence") ~ "incidence",
       x %in% c("data_other_countries") ~ "data_other_countries",
       x %in% c("flowminder", "estimates_other") ~ "estimates_other",
       x == "social_media" ~ "social_media",
       x == "flight_capacity" ~ "flight_capacity",
       x == "census" ~ "census"
     )
   }
)



## TODO empricial data is missing one datasource type (check Epidemics7 poster for comparison)
emp <- filter(ds_types, data_category == "empirical")
p1 <- map_data_availability(emp_data_features_grp, emp) +
  ggtitle("Empirical data on human movement")

est <- filter(ds_types, data_category != "empirical")
group1 <- c("data_other_countries")
est_data_features_grp1 <- est_data_features_grp[est_data_features_grp$datasource_type %in% group1, ]
est1 <- est[est$datasource_type %in% group1, ]
p2 <- map_data_availability(est_data_features_grp1, est1) +
  ggtitle("Mobility Proxies",
          "based on empirical data from other African countries")

est_data_features_grp1 <- est_data_features_grp[!est_data_features_grp$datasource_type %in% group1, ]
est1 <- est[!est$datasource_type %in% group1, ]
p3 <- map_data_availability(est_data_features_grp1, est1) +
  ggtitle("Mobility Proxies",
          "based on data from non-African countries or on indirect evidence")


ggsave("empirical.png", p1)
ggsave("estimated.png", p2)
ggsave("estimated_indirect.png", p3)


x <- select(ds_types, bibkey, data_category)
x <- distinct(x)
count(x, data_category)

## Within studies reporting empirical data,
## what were the data types

## table(emp$datasource_grped)

##       cdr    census      GBMD      hdss interview     ipums     unhcr
##        11         2        42         3         6        33        51
## GBMD is Global Bilateral Migration Database
est <- filter(ds_types, data_category != "empirical")
## table(est$datasource_grped)

##               census data_other_countries      estimates_other
##                    1                   60                    2
##      flight_capacity           flowminder              genomic
##                   52                   14                   33
##            incidence         social_media
##                    8                    2
other_cntry <- filter(ds_types, datasource_type == "data_other_countries")
## unique(other_cntry$bibkey)
## [1] "sorichetta2016mapping"    "wesolowski2014commentary"
