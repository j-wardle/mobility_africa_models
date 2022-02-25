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

## Color scheme for possible data sources
## arranged according to their utility

datasource_palette <- c(
  cdr = "#9e6ebd", ## high utility
  ## medium utility
  ipums = "#7aa457",
  census = "#7aa457",
  interview = "a48157",
  hdss = "#7aa457",
  ## low utility
  GBMD = "#cb6751",
  unhcr = "#cb6751",
  ## Estimated data sources
  genomic = "#000000",
  flight_capacity = "#E69F00",
  data_other_countries = "#56B4E9",
  estimates_other = "#56e9d5",
  incidence = "#CC79A7",
  estimates_other = "#0072B2",
  flowminder = "#0072B2",
  social_media = "#F0E442"
)

datasource_labels <- c(
  ##cdr = "Mobile phone data; data on daily/weekly movement", ## high utility
  cdr = "CDR",
  ## medium utility
  ipums = "Census micro-data",
  ##  census = "Census, demographic surveillance surveys, interviews; data on annual or 5-yearly migration",
  census = "Census",
  interview = "Interviews",
  hdss = "DHS",
  ## low utility
  GBMD = "Global bi-lateral migration data",
  unhcr = "Long-term or refugee migration",
  ## Estimated data sources
  genomic = "Genomes",
  flight_capacity = "Flight capacity",
  data_other_countries = "Empirical data (other countries)",
  incidence = "Incidence/genomic data",
  estimates_other = "Mobility proxies",
  flowminder = "Mobility proxies",
  social_media = "Social media"
)
## Availability mapped to transperency
availability_scale <- c(
  yes = 1,
  yes_in_principle = 0.7,
  no = 0.4
)

## Spatial scale mapped to size
size_scale <- c(
  ADM0 = 4,
  ADM1 = 3,
  airport = 3.5,
  ADM2 = 2,
  ADM3 = 1,
  ADM4 = 0.5,
  specific_location = 0.5
)

cluster_palette <- c(
  `1` = "#E69F00",
  `2` = "#009E73",
  `3` = "#0072B2",
  `4` = "#CC79A7",
  `5` = "#49ad88",
  `6` = "#c18b40",
  `7` = "#6b8ece",
  `8` = "#c55d86"
)

paper_theme <- list(
  dateformat = scales::date_format("%m-%Y"),
  theme = theme_classic(base_size = 14),
  xticklabels = theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6)
  ),
  legend = theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
)


project_theme <- list(
  seir_scale = c(
    "susceptible" = "#0258c6",
    "exposed" = "#1c5e34",
    "infected" = "#a20087",
    "recovered" = "#ff94b2"
  ),

  theme = theme_classic(base_size = 10),
  legend = theme(legend.title = element_blank()),
  axis_labels = theme(axis.title = element_blank())
)
