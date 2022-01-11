dir.create("shapefiles")
africa <- c(
  "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF",
  "TCD", "COM", "COG", "CIV", "COD", "DJI", "EGY", "GNQ", "ERI",
  "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR",
  "LBY", "MDG", "MWI", "MLI", "MRT", "MAR", "MOZ", "NAM", "NER",
  "NGA", "RWA", "STP", "SEN", "SLE", "SOM", "ZAF", "SDN", "SWZ",
  "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE"
)

url_prefix <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_"
urls <- map(
  c(1, 2, 3, 4), ~ glue("{url_prefix}{africa}_{.x}_sf.rds")
)
urls <- unlist(urls)

walk(
  urls,
  function(url) {
    path <- str_split_fixed(url, pattern = "/", n = Inf)
    path <- glue("shapefiles/{path[[7]]}")
    if (file.exists(path)) {
      message(path, " already exists. Skipping.")
    } else {
      message("writing to ", path)
      out <- GET(url, write_disk(path = path, overwrite = TRUE))
      if (out$status_code != "200") {
        message("Removing ", path)
        file.remove(path)
      }
    }
  }
)

paths <- str_split_fixed(urls, pattern = "/", n = Inf)
paths <- glue("shapefiles/{paths[, 7]}")
paths <- paths[file.exists(paths)]

adm_count <- map_dfr(
  paths,
  function(df) {
    iso3c <- str_split_fixed(df, pattern = "_", n = Inf)
    country <- iso3c[2]
    level <- iso3c[3]
    message("Reading ", df)
    if (file.exists(df)) {
      ## Not all countries have all levels
      cntry <- readRDS(df)
      data.frame(
        country = country,
        adm_level = level,
        n = nrow(cntry)
      )
    } else NULL
  }
)

write_csv(
  x = adm_count,
  path = "num_admin_units_africa.csv"
)
files2zip <- dir('shapefiles', full.names = TRUE)
zip::zip(zipfile = "africa_shapefiles.zip", files = files2zip)
