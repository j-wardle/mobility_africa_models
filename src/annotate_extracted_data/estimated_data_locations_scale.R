#######################################################################
## "gustafson2017identifying"
locations <- read_csv(
  "estimates_data/gustafson2017identifying_chiefdom_longlat.csv",
  col_names = FALSE
)

out <- data.frame(
  bibkey = "gustafson2017identifying",
  location = "",
  country = "SLE",
  long = locations$X1,
  lat = locations$X2,
  scale = "ADM3" ## ADM3
)

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv"
)

#######################################################################
## "dudas2017virus"
## https://github.com/ebov/space-time/blob/master/Data/Location_Data_2016-05-27.csv
locations <- read_csv(
  "estimates_data/dudas2017virus_Location_Data_2016-05-27.csv"
)
## There are sequences from CIV, MLI etc here but the paper does not
## mention using them. So exclude.
locations <- locations[locations$ISO %in% c("GIN", "LBR", "SLE"), ]
out <- data.frame(
  bibkey = "dudas2017virus",
  location = locations$Location,
  country = locations$ISO,
  long = locations$Pop_Centroid_X,
  lat = locations$Pop_Centroid_Y,
  scale = paste0("ADM", locations$Location_Admin_Level)
)

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "huang2013open"
## Data are available from vbd-air upon request.
## However, the same data are available on worldpop
## These are all airports in any city with a population greater than
## 100000.
airport_info <- read_csv(
  "estimates_data/AirportInfo.csv"
)
africa <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF",
  "TCD", "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI",
  "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR",
  "LBY", "MDG", "MWI", "MLI", "MRT", "MAR", "MOZ", "NAM", "NER",
  "NGA", "RWA", "STP", "SEN", "SLE", "SOM", "ZAF", "SDN", "SWZ",
  "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE"
)
cntries <- countrycode(
  africa, "iso3c", "country.name"
)
## Get country name from airport name
y <- map2_dfr(
  cntries, africa, function(cntry, iso3c) {
    message("Looking for ", cntry)
    idx <- str_detect(airport_info$OAGName, fixed(cntry))
    out <- airport_info[idx, ]
    message(paste(out$OAGName, collapse = "\n"))
    out$country <- iso3c
    out
  }
)
## all_airports <- htmltab::htmltab("https://www.ccra.com/airport-codes/")
## airport_info[airport_info$NodeName %in% all_airports$Code, ]
out <- data.frame(
  bibkey = "huang2013open",
  location = y$City,
  country = y$country,
  long = y$Lon,
  lat = y$Lat,
  scale = "airport"
)
write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "sorichetta2016mapping"
zipped <- list.files(
  path = "estimates_data/AFR-MIG",
  pattern = glob2rx("*zip"), full.names = TRUE
)

for (zipfile in zipped) {
  message("Unzipping ", zipfile)
  exdir <- stringr::str_remove(zipfile, ".zip")

  if (!dir.exists(exdir)) {
    unzip(zipfile, exdir = exdir)
  } else {
    message(exdir, " exists")
  }
}

paths <- str_remove(zipped, ".zip")

for (path in paths) {
  zipped <- list.files(
    path = path, pattern = glob2rx("*zip"), full.names = TRUE
  )
  message("Unzipping ", zipped)
  exdir <- stringr::str_remove(zipped, ".zip")
  if (!dir.exists(exdir)) {
    unzip(zipped, exdir = exdir)
  } else {
    message(exdir, " exists")
  }
  ## unzip(zipped)
}
## Names look like this:
## ZAF_5yrs_InternalMigFlows_2010/ZAF_AdminUnit_Centroids
indirs <- list.dirs("estimates_data/AFR-MIG")
indirs <- indirs[grep("Centroids", indirs)]

locations <- map_dfr(
  indirs,
  function(indir) {
    centroids <- sf::st_read(indir)
    out <- data.frame(
      location = "",
      country = centroids$ISO,
      long = centroids$POINT_X,
      lat = centroids$POINT_Y
    )
    out
  }
)
## > count(locations, country) %>% print(n = Inf)
## # A tibble: 45 x 2
##    country     n
##    <chr>   <int>
##  1 AGO        18
##  2 BDI        17
##  3 BEN        12
##  4 BFA        45
##  5 BWA         9 ## 1 has 16, 2 has 30
##  6 CAF        17
##  7 CIV        19 ## 1 has 14, 2 has 33
##  8 CMR        58
##  9 COD        38 ## 1 has 26, 2 has 240
## 10 COG        10 ## 1 has 12, 2 has 48
## 11 COM         3
## 12 DJI        11
## 13 ERI         6
## 14 ETH        11
## 15 GAB         9
## 16 GHA        10
## 17 GIN        34
## 18 GMB         6
## 19 GNB         9
## 20 GNQ         7
## 21 KEN        46
## 22 LBR        15
## 23 MDG        22
## 24 MLI        47
## 25 MOZ        10
## 26 MRT        44
## 27 MWI        31
## 28 MYT        17
## 29 NAM        13
## 30 NER        36
## 31 NGA        38
## 32 RWA        10
## 33 SDN        15
## 34 SEN        34 ## 2 --> 45
## 35 SLE        14
## 36 SOM        18
## 37 STP         4 ## 1 --> 2
## 38 SWZ         4
## 39 TCD        18 ## 1 --> 23
## 40 TGO         5
## 41 TZA        26 ## 1 --> 30
## 42 UGA        56 ## 1 --> 58
## 43 ZAF         9
## 44 ZMB        72
## 45 ZWE        10
adm_count <- read_csv("num_admin_units_africa.csv")
counts <- count(locations, country)
data_adm_levels <- left_join(counts, adm_count)

## Precise matches are ok, check where the match is not exact and
## choose the  ADM Level which has the closest number of units
nonmatching <- data_adm_levels[is.na(data_adm_levels$adm_level), ]
adm_count <- adm_count[adm_count$country %in%
  nonmatching$country, ]

closest <- left_join(
  x = adm_count,
  y = nonmatching,
  by = "country",
  suffix = c("_gadm", "_data")
) %>%
  split(.$country) %>%
  map_dfr(
    function(df) df[which.min(abs(df$n_gadm - df$n_data)), ]
  )

closest <- select(
  closest, country,
  n = n_data, adm_level = adm_level_gadm
)
##################################
## Exclude MYT which is a French overseas
## territory.
data_adm_levels <- data_adm_levels[data_adm_levels$country != "MYT", ]
## Make sure both data frames are in same order.
closest <- arrange(closest, country)
data_adm_levels <- arrange(data_adm_levels, country)
data_adm_levels$adm_level[is.na(data_adm_levels$adm_level)] <-
  closest$adm_level

data_adm_levels$scale <- glue("ADM{data_adm_levels$adm_level}")

locations$bibkey <- "sorichetta2016mapping"
locations <- left_join(locations, data_adm_levels, by = "country")

out <- locations[, c("bibkey", "location", "country", "long", "lat", "scale")]

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "wesolowski2014commentary"
locations <- read_csv(
  "estimates_data/mobility-data-1stSept14/AdmUnits_Within.csv"
)

out <- data.frame(
  bibkey = "wesolowski2014commentary",
  location = "",
  country = locations$country,
  long = locations$from_x,
  lat = locations$from_y
)

## group_by(locations, country) %>% summarise(n = length(unique(from_loc)))
## # A tibble: 15 x 2
##    country     n
##    <chr>   <int>
##  1 BEN        12
##  2 BFA        45
##  3 CIV        19 ## CIV has 33 ADM2 units and 14 ADM1 units.
##  4 CMR        58
##  5 GHA        10
##  6 GIN        34
##  7 GMB         6
##  8 GNB         9
##  9 LBR        15
## 10 MLI        47 ## ADM1 has 9 and ADM2 has 50
## 11 NER         8
## 12 NGA        38 ## ADM1 has 38 units
## 13 SEN        30 ## ADM1 has 14, ADM2 has 45 units
## 14 SLE        14
## 15 TGO         5

levels <- data.frame(
  country = c(
    "BFA", "CIV", "CMR", "GHA", "GIN", "GMB",
    "LBR", "MLI", "NER", "NGA", "SLE", "TGO", "SEN", "BEN", "GNB"
  ),
  scale = glue("ADM{c(2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1)}")
)

out <- left_join(out, levels)
write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "dsilva2017"
data <- read_csv("study_ds_types.csv")
data <- data[data$bibkey == "dsilva2017", ]
data$location <- c("GIN", "LBR", "SLE", "GIN", "LBR", "SLE")
data$country <- c("GIN", "LBR", "SLE", "GIN", "LBR", "SLE")
adm0_centroids <- read_csv(file = "adm0_centroids.csv")
out <- left_join(data, adm0_centroids)
out$scale <- "ADM0"
out <- out[, c("bibkey", "location", "country", "long", "lat", "scale")]

### Gravity model was also fitted at district level.
## 14 districts/areas of Sierra Leone, 15 counties of Liberia, and
## 34 prefectures of Guinea
## So, SLE is ADM2,  LBR is ADM1 and GIN is ADM2.
gin <- readRDS("shapefiles/gadm36_GIN_2_sf.rds")
sle <- readRDS("shapefiles/gadm36_SLE_2_sf.rds")
lbr <- readRDS("shapefiles/gadm36_LBR_1_sf.rds")

centroids <- map2_dfr(
  list(gin, sle, lbr),
  list("2", "2", "1"),
  function(df, level) {
    centroids <- st_centroid(df$geometry)
    centroids <- st_coordinates(centroids)
    locations <- df[[glue::glue("NAME_{level}")]]
    country <- df$GID_0
    message("Doing ", country[1], " ", level)
    x <- data.frame(
      location = locations,
      country = country,
      long = centroids[, 1],
      lat = centroids[, 2]
    )
    x$bibkey <- "dsilva2017"
    x$scale <- glue("ADM{level}")
    x
  }
)

out <- rbind(out, centroids)

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)
#######################################################################
## "silal2015"
## Human movements are incorporated in two ways. Local movements are
## allowed between the five Mpumalanga patches (from all five
## compartments in all three sub-patches). Foreign travel is allowed
## between the Maputo patch and any of the five Mpumalanga patches
## (from all five
## compartments) as illustrated in Fig 2b. It was not possible to access
## quality temporal data on human movement patterns between the six study
## areas. Thus a gravity model was considered to model human migration.
## Migration is modelled as a constant rate between patches that is
## inversely weighted by distance. This rate is varied stochastically in
## model. The constant rate is inferred to parameter estimation in the
## transmission model and a sensitivity analysis of this rate is
## conducted and presented in S1 Text.

out <- data.frame(
  bibkey = "silal2015",
  location = c("Mpumalanga", "Maputo"),
  country = c("ZAF", "MOZ"),
  long = c(28.693960, 32.573174),
  lat = c(-26.140080, -25.969248),
  scale = "specific_location"
)

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "kraemer2017spread"
## great circle distance between the centroids of each pair of districts
## the travel time between each pair of districts
## aggregated and deidentified mobile phone-derived mobility estimates
## at the constituency level from Namibia between Oct 1, 2010, and
## Sept 30, 2011.
## hen used the models to predict between-district mobility for Angola
## and the DR Congo
## shapefiles shared by Moritz, not available otherwise.
out <- st_read("drc_moritz/shp/")
centroids <- st_centroid(out$geometry)
centroids <- st_coordinates(centroids)

out <- data.frame(
  bibkey = "kraemer2017spread",
  locations = "",
  country = out$ISO,
  long = centroids[, 1],
  lat = centroids[, 2],
  scale = "ADM3" ## may or maynot be adm3, i only want to use for plotting
)

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)

#######################################################################
## "silal2015hitting" same as "silal2015"
#######################################################################
## "kramer2016spatial"
## https://datadryad.org/stash/dataset/doi:10.5061/dryad.k95j3
## Use data from flowminder


#######################################################################
## "tatem2012spatial"
## explains the HIV subtype distribution at various locations
## using distances in Africa (straight line distance, friction surface)
## etc. I cannot find the location of the individual samples without
## going into each refeernce. Hence using the centroid of the country
## as location, although the distances are calculated between sample
## locations.
#######################################################################
data <- read_csv("study_ds_types.csv")
data <- data[data$bibkey == "tatem2012spatial", ]
out <- data[, c("bibkey", "country")]
out$locations <- out$country
adm0_centroids <- read_csv(file = "adm0_centroids.csv")
out <- left_join(out, adm0_centroids)
out$scale <- "specific_location"
out <- out[, c("bibkey", "locations", "country", "long", "lat", "scale")]

write_csv(
  x = out,
  path = "estimated_data_locations_scale.csv",
  append = TRUE
)
