## Read in population weighted centroid estimates from Wes
## Tidy so that names match mobile phone data

## Prepare each country in turn


# PORTUGAL ----------------------------------------------------------------

# Load population data and mobile phone id names
prt_adm2_populations <- read.delim("task_data/centroids_2006_2.txt") %>% 
  filter(N0 == "Portugal") %>% 
  clean_names()

prt_id_names <- read.table("task_data/portugal_id_list.txt") %>% 
  rename(id = V1,
         name = V2)

# Clean adm2 names to match formatting of id_names
prt_adm2_populations$n2 <- iconv(prt_adm2_populations$n2, to = "ASCII//TRANSLIT")
prt_adm2_populations$n2 <- gsub(" ", "_", toupper(prt_adm2_populations$n2))

# Check for duplicates
n_occur <- data.frame(table(prt_adm2_populations$n2))
n_occur[n_occur$Freq > 1,]

# There are two duplicates

prt_adm2_populations <- prt_adm2_populations %>%
  filter(!(n2 == "CALHETA")) %>%   # two municipalities on islands of Madeira and Azores
  filter(!(n2 == "LAGOA" & n1 == "Azores")) # remove municipality on Azores

# Filter centroid dataset to keep only matches with id_names

portugal_location_data <- prt_adm2_populations %>% 
  filter(n2 %in% prt_id_names$name) %>% 
  select(n2, pop, pop_cent_lon, pop_cent_lat) %>% 
  rename(location = n2,
         population = pop,
         x = pop_cent_lon,
         y = pop_cent_lat) %>% 
  arrange(location)

# Save 2006 data for Portugal adm2 units
saveRDS(portugal_location_data, "portugal_location_data.rds")



## Create a cleaned lookup for identifying the adm1 unit that each adm2 belongs in

# First clean adm names in same way as before
prt_adm2_populations$n1 <- iconv(prt_adm2_populations$n1, to = "ASCII//TRANSLIT")
prt_adm2_populations$n1 <- gsub(" ", "_", toupper(prt_adm2_populations$n1))

portugal_adm_lookup <- prt_adm2_populations %>%
  select(n1, n2)

saveRDS(portugal_adm_lookup, "portugal_adm_lookup.rds")


## Create dataframe with cleaned population and coordinates of adm1 units

prt_adm1_populations <- read.delim("task_data/centroids_2006_1.txt") %>%
  filter(N0 == "Portugal") %>%
  clean_names()

prt_adm1_populations$n1 <- iconv(prt_adm1_populations$n1, to = "ASCII//TRANSLIT")
prt_adm1_populations$n1 <- gsub(" ", "_", toupper(prt_adm1_populations$n1))

portugal_adm1_location_data <- prt_adm1_populations %>%
  filter(!(n1 %in% c("AZORES", "MADEIRA"))) %>%
  select(n1, pop, pop_cent_lon, pop_cent_lat) %>%
  rename(location = n1,
         population = pop,
         x = pop_cent_lon,
         y = pop_cent_lat) %>%
  arrange(location)

saveRDS(portugal_adm1_location_data, "portugal_adm1_location_data.rds")



# FRANCE ------------------------------------------------------------------

## Create adm3 population dataframe

# Load population data and mobile phone id names
fra_adm3_populations <- read.delim("task_data/centroids_2007_3.txt") %>% 
  filter(N0 == "France") %>% 
  clean_names()

fra_id_names <- read.table("task_data/france_id_list.txt", encoding = 'UTF-8') %>% 
  rename(id = V1,
         name = V2)

# Clean adm3 and id names to have same formatting
fra_adm3_populations$n3 <- iconv(fra_adm3_populations$n3, to = "ASCII//TRANSLIT")
fra_adm3_populations$n3 <- gsub(" ", "_", toupper(fra_adm3_populations$n3))

fra_id_names$name <- iconv(fra_id_names$name, from = "UTF-8", to = "ASCII//TRANSLIT")
fra_id_names$name <- gsub(" ", "_", toupper(fra_id_names$name))

# Check for duplicates
fra_n_occur <- data.frame(table(fra_adm3_populations$n3))
fra_n_occur[fra_n_occur$Freq > 1,]

# Tidy up naming of locations
fra_adm3_populations <- fra_adm3_populations %>%
  mutate(n3 = replace(n3, n2 == "Paris", "PARIS"),
         n3 = replace(n3, n3 == "CHATEAU-CHINON_(VILLE)", "CHATEAU-CHINON(VILLE)"),
         n3 = stringr::str_replace(n3, "LE_", "LE"),
         n3 = stringr::str_replace(n3, "LA_", "LA"),
         n3 = stringr::str_replace(n3, "LES_", "LES"))

# Some boundary changes were made in France in 2007. These are reflected in the population dataset,
# but the mobile phone data uses the previous boundaries. We handle this with the following cleaning.
# Arcachon was formed out of Bordeaux. Re-combine with Bordeaux area
# Fougeres-Vitre was previously just called Fougeres

fra_adm3_populations <- fra_adm3_populations %>% 
  mutate(n3 = replace(n3, n3 == "ARCACHON", "BORDEAUX"),
         n3 = replace(n3, n3 == "FOUGERES-VITRE", "FOUGERES"))

# Keep locations that feature in mobile data. Exclude island of Corse as likely to skew gravity model
france_location_data <- fra_adm3_populations %>%
  filter(n3 %in% fra_id_names$name & n1 != "Corse") %>%
  select(n3, pop, pop_cent_lon, pop_cent_lat) %>%
  rename(location = n3,
         population = pop,
         x = pop_cent_lon,
         y = pop_cent_lat) %>%
  arrange(location)

# Each Paris arrondissement features separately in pop data. Mobile data has a combined Paris.
# Combine arrondissements to give one Paris area

france_location_data <- france_location_data %>%
  group_by(location) %>%
  summarise(x = weighted.mean(x, population),
            y = weighted.mean(y, population),
            population = sum(population)) %>%
  select(location, population, x, y)

saveRDS(france_location_data, "france_location_data.rds")


## Create a cleaned lookup for identifying the adm2 unit that each adm3 belongs in

# First clean adm names in same way as before
fra_adm3_populations$n2 <- iconv(fra_adm3_populations$n2, to = "ASCII//TRANSLIT")
fra_adm3_populations$n2 <- gsub(" ", "_", toupper(fra_adm3_populations$n2))

fra_adm_lookup <- fra_adm3_populations %>%
  select(n2, n3)

saveRDS(fra_adm_lookup, "france_adm_lookup.rds")


## Create dataframe with cleaned population and coordinates of France adm2 units

fra_adm2_populations <- read.delim("task_data/centroids_2007_2.txt") %>%
  filter(N0 == "France") %>%
  clean_names()

fra_adm2_populations$n2 <- iconv(fra_adm2_populations$n2, to = "ASCII//TRANSLIT")
fra_adm2_populations$n2 <- gsub(" ", "_", toupper(fra_adm2_populations$n2))

france_adm2_location_data <- fra_adm2_populations %>%
  filter(!(n1 == "Corse")) %>%
  select(n2, pop, pop_cent_lon, pop_cent_lat) %>%
  rename(location = n2,
         population = pop,
         x = pop_cent_lon,
         y = pop_cent_lat) %>%
  arrange(location)

saveRDS(france_adm2_location_data, "france_adm2_location_data.rds")