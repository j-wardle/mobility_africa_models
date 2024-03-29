url_prefix <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_"
path <- "shapefiles/gadm36_CIV_4_sf.rds"
if (! file.exists(path)) {
  url <- glue("{url_prefix}CIV_{4}_sf.rds")
  GET(url, write_disk(path = path, overwrite = TRUE))
}
civ <- readRDS(path)
geoms <- st_geometry(civ)
centroids <- st_centroid(geoms)
coords <- st_coordinates(centroids)
out <- data.frame(
  bibkey = "lu2013approaching",
  location = civ$NAME_4,
  country = "CIV",
  long = coords[, 1],
  lat = coords[, 2],
  scale = "ADM4"
)

write_csv(
  x = out,
  file = "empirical_data_locations_scale.csv"
)

################################################
######################
zaf <- readRDS("shapefiles/gadm36_ZAF_3_sf.rds")
geoms <- st_geometry(zaf)
centroids <- st_centroid(geoms)
coords <- st_coordinates(centroids)
out <- data.frame(
  bibkey = "choe2014internal",
  location = zaf$NAME_3,
  country = "ZAF",
  long = coords[, 1],
  lat = coords[, 2],
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE)

######################################################################
########## peak2018population ########################################
######################################################################
sle <- readRDS("shapefiles/gadm36_SLE_3_sf.rds")
geoms <- st_geometry(sle)
centroids <- st_centroid(geoms)
coords <- st_coordinates(centroids)
out <- data.frame(
  bibkey = "peak2018population",
  location = sle$NAME_3,
  country = "SLE",
  long = coords[, 1],
  lat = coords[, 2],
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
################## sorichetta2016mapping #############################
######################################################################
countries <- c(
  "CMR", "GHA", "GIN", "MLI", "MWI", "SEN", "UGA", "ZAF", "ZMB"
)
adm_levels <- c("2", "1", "1", "2", "1", "2", "1", "1", "1")

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "sorichetta2016mapping",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(x = out, file = "empirical_data_locations_scale.csv", append = TRUE)

######################################################################
############ wesolowski2014commentary ################################
######################################################################
### Paper says These data were aggregated to quantify human travel
### patterns over the course of the year between 69 Kenyan districts
### and 692 mapped settlements. I can't figure out what admin
### level do the settlements correspond to. Districts are ADM2 I believe.
### Similarly for Mali, IPUMS data used in paper are at district level
### I don't know what that means in GADM language.
## skipping MLI.
countries <- c(
  "SEN", "SEN", "CIV", "KEN", "BFA", "CMR", "GHA", "GIN", "SLE"
)
adm_levels <- c(3, 2, 4, 2, 2, 3, 2, 2, 3)

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "wesolowski2014commentary",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)
write_csv(x = out, file = "empirical_data_locations_scale.csv", append = TRUE)

######################################################################
################# ruyssen2014determinants ############################
adm0_centroids <- read_csv("adm0_centroids.csv")

data <- read_csv("study_ds_types.csv")
data <- data[data$bibkey == "ruyssen2014determinants", "country"]

out <- adm0_centroids[adm0_centroids$country %in% data$country, ]

out$location <- out$country
out$bibkey <- "ruyssen2014determinants"
out <- out[, c("bibkey", "location", "country", "long", "lat")]
out$scale <- "ADM0"
write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## garcia2015modeling
######################################################################
countries <- c(
  "ghana", "kenya", "malawi", "mali", "senegal", "sierra leone",
  "south africa", "south africa", "south africa", "sudan",
  "tanzania", "uganda"
)
countries <- countrycode(countries, "country.name", "iso3c")
adm_levels <- c(
  "1", "2", "2", "2", "2", "2", "1", "1", "1", "2", "1", "1"
)


## https://biogeo.ucdavis.edu/gadm3.6/Rsf/gadm36_GHA_1_sf.rds
url_prefix <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_"
url_suffix <- "_sf.rds"
urls <- glue("{url_prefix}{countries}_{adm_levels}{url_suffix}")

walk(
  urls,
  function(url) {
    path <- str_split_fixed(url, pattern = "/", n = Inf)
    path <- glue("shapefiles/{path[[7]]}")
    if (!file.exists(path)) {
      message("writing to ", path)
      GET(url, write_disk(path = path, overwrite = TRUE))
    }
  }
)

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "garcia2015modeling",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## yukich2013travel
## health centre is located in Adami Tulu Jido Kombolcha
######################################################################
eth <- readRDS("shapefiles/gadm36_ETH_3_sf.rds")
idx <- grep("Adami", eth$NAME_3)
geom <- st_geometry(eth[idx, ])
centroid <- st_centroid(geom)
coords <- st_coordinates(centroid)

out <- data.frame(
  bibkey = "yukich2013travel",
  location = "Adami Tulu Jido Kombolcha",
  country = "ETH",
  long = coords[, 1],
  lat = coords[, 2],
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## iqbal2007geo
######################################################################
adm0_centroids <- read_csv("adm0_centroids.csv")


data <- read_csv("study_ds_types.csv")
data <- data[data$bibkey == "iqbal2007geo", "country"]
## data$iso3c <- countrycode(
##   data$country, "country.name", "iso3c"
## )



out <- adm0_centroids[adm0_centroids$country %in% data$country, ]

out$location <- out$country
out$bibkey <- "iqbal2007geo"
out <- out[, c("bibkey", "location", "country", "long", "lat")]
out$scale <- "ADM0"
write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## mastrorillo2016influence
## 52 South African district councils
## I believe these should be ADM2
## zaf <- readRDS("shapefiles/gadm36_ZAF_2_sf.rds")
## dim(zaf)
## [1] 52 14

zaf <- readRDS("shapefiles/gadm36_ZAF_2_sf.rds")
geoms <- st_geometry(zaf)
centroids <- st_centroid(geoms)
coords <- st_coordinates(centroids)
out <- data.frame(
  bibkey = "mastrorillo2016influence",
  location = zaf$NAME_2,
  country = "ZAF",
  long = coords[, 1],
  lat = coords[, 2],
  scale = "ADM2"
)


write_csv(
  x = out,
  file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## pindolia2013demographics
######################################################################

countries <- c("kenya", "tanzania", "uganda")
countries <- countrycode(countries, "country.name", "iso3c")
adm_levels <- rep("2", 3)
## https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GHA_1_sf.rds
url_prefix <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_"
url_suffix <- "_sf.rds"

urls <- glue("{url_prefix}{countries}_{adm_levels}{url_suffix}")

walk(
  urls,
  function(url) {
    path <- str_split_fixed(url, pattern = "/", n = Inf)
    path <- glue("shapefiles/{path[[7]]}")
    message("writing to ", path)
    if (!file.exists(path)) {
      GET(url, write_disk(path = path, overwrite = TRUE))
    } else {
      message("File ", path, "exists.")
    }
  }
)

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "pindolia2013demographics",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## marshall2018
## Study sites are detailed in
## https://malariajournal.biomedcentral.com/articles/10.1186/s12936-016-1252-3
## Mali: Bamako, Baya, Baroueli, Mopti, Fatoma
## Burkina Faso Ouagadougou, Sapone, Boussé
## Zambia: Lusaka, Samfya, Kitwe, Nakonde, Chipata,
## Tanzania: Dar es Salaam, Ifakara, Muheza, Mtwara
## Co-ordinates extracted from https://www.latlong.net
## For Bamako and Baya, copied from here:
## https://tools.wmflabs.org/wp-world/umkreis.php?la=en&submit=Table&lon=-8&lat=12.65&rang=250&map=0&limit=500
######################################################################
mli <- data.frame(
  bibkey = "marshall2018",
  location = c("Bamako", "Baya", "Baroueli", "Mopti", "Fatoma"),
  country = c("MLI", "MLI", "MLI", "MLI", "MLI"),
  long = c(-7.983333, -8.20083333, -6.832850, -4.195620, -4.059990),
  lat = c(12.633333, 11.64444444, 13.076600, 14.497030, 14.613520)
)

bfa <- data.frame(
  bibkey = "marshall2018",
  location = c("Ouagadougou", "Sapone", "Boussé"),
  country = c("BFA", "BFA", "BFA"),
  long = c(-1.519920, -1.598750, -1.888880),
  lat = c(12.371530, 12.054910, 12.660800)
)

zambia <- data.frame(
  bibkey = "marshall2018",
  location = c("Lusaka", "Samfya", "Kitwe", "Nakonde", "Chipata"),
  country = rep("ZMB", 5),
  long = c(28.322817, 29.550751, 28.222580, 32.760818, 32.647110),
  lat = c(-15.387526, -11.364310, -12.797050, -9.318700, -13.638380)
)

tza <- data.frame(
  bibkey = "marshall2018",
  location = c("Dar es Salaam", "Ifakara", "Muheza", "Mtwara"),
  country = rep("TZA", 4),
  long = c(39.208328, 36.678180, 38.791600, 40.175961),
  lat = c(-6.792354, -8.141640, -5.177190, -10.279280)
)

out <- rbind(mli, bfa, zambia, tza)
out$scale <- "ADM3"

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)
######################################################################
## tompkins2016
######################################################################
countries <- "SEN"
adm_levels <- "3"
out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "tompkins2016",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]

    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)
######################################################################
## ruktanonchai2016
## Data used in study were at settlement level.
## Data aggregated to constituency level are made available
## GADM only has levels 0, 1, and 2 for Namibia
######################################################################
countries <- "NAM"
adm_levels <- "2"
out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "ruktanonchai2016",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## wesolowski2013
######################################################################
countries <- "KEN"
adm_levels <- "1"

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "wesolowski2013",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)
write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

######################################################################
## mari2017big
######################################################################
countries <- "SEN"
adm_levels <- "3"

out <- map2_dfr(
  countries,
  adm_levels,
  function(country, adm_level) {
    infile <- glue("shapefiles/gadm36_{country}_{adm_level}_sf.rds")
    message("Reading ", infile)
    cntry <- readRDS(infile)
    geoms <- st_geometry(cntry)
    centroids <- st_centroid(geoms)
    coords <- st_coordinates(centroids)
    message(
      "Length of locations ",
      length(cntry[[paste0("NAME_", adm_level)]])
    )
    message(
      "Length of centroids ",
      nrow(coords)
    )
    x <- data.frame(
      bibkey = "mari2017big",
      location = cntry[[paste0("NAME_", adm_level)]],
      country = country,
      long = coords[, 1],
      lat = coords[, 2]
    )
    x$scale <- glue("ADM{adm_level}")
    x
  }
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

#######################################################################
## matamalas2016assessing
## Same as above
#######################################################################
out$bibkey <- "matamalas2016assessing"
write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

#######################################################################
## finger2016mobile
## Same as above.
#######################################################################
out$bibkey <- "finger2016mobile"
write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

#######################################################################
## collinson2014migration
## Agincourt in South Africa
#######################################################################
out <- data.frame(
  bibkey = "collinson2014migration",
  location = "Agincourt",
  country = "ZAF",
  long = 31.229470,
  lat = -24.828060,
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

#######################################################################
## andrews2012projecting
## Masiphumelele, South Africa
#######################################################################
out <- data.frame(
  bibkey = "andrews2012projecting",
  location = "Masiphumelele",
  country = "ZAF",
  long = 18.377730,
  lat = -34.128830,
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)

#######################################################################
## dobra2017space
## KwaZulu-Natal, South Africa
#######################################################################
out <- data.frame(
  bibkey = "dobra2017space",
  location = "KwaZulu-Natal",
  country = "ZAF",
  long = 30.882700,
  lat = -28.944469,
  scale = "ADM3"
)

write_csv(
  x = out, file = "empirical_data_locations_scale.csv", append = TRUE
)


#######################################################################
## "dobra2018loglinear"
## geolocated tweets mapped to 213 municipalities of South Africa.
zaf <- readRDS("shapefiles/gadm36_ZAF_3_sf.rds")
## There are 234 of these rather than 213
centroids <- st_centroid(zaf)
centroids <- st_coordinates(centroids$geometry)

out <- data.frame(
  bibkey = "dobra2018loglinear",
  locations = zaf$NAME_3,
  country = "ZAF",
  long = centroids[, 1],
  lat = centroids[, 2],
  scale = "ADM3"
)

write_csv(
  x = out,
  path = "empirical_data_locations_scale.csv",
  append = TRUE
)
