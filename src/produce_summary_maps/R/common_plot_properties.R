## Color scheme for possible data sources
## arranged according to their utility

datasource_palette <- c(
  cdr = "#9e6ebd", ## high utility
  ## medium utility
  ipums = "#7aa457",
  census = "#7aa457",
  interview = "#000000",
  hdss = "#7aa457",
  ## low utility
  GBMD = "#cb6751",
  unhcr = "#cb6751",
  ## Estimated data sources
  genomic = "#000000",
  flight_capacity = "#E69F00",
  data_other_countries = "#56B4E9",
  estimates_other = "#56e9d5",
  incidence = "#0b5394",
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
  unhcr = "Long-term/refugee",
  ## Estimated data sources
  genomic = "Genomes",
  flight_capacity = "Flight data",
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
  ADM0 = 5,
  ADM1 = 4,
  airport = 4,
  ADM2 = 3,
  ADM3 = 2,
  ADM4 = 1,
  specific_location = 1
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
