## orderly::orderly_develop_start(use_draft = "newer",
##                                parameters = list(week_ending = "2021-01-10",
##                                location = "Arizona"))


## Read in population weighted centroid estimates from Wes
## Tidy so that names match mobile phone data

## Prepare each country in turn



# PORTUGAL ----------------------------------------------------------------

# Load population data and mobile phone id names
adm2_populations <- read.delim("task_data/centroids_2006_2.txt") %>% 
  filter(N0 == "Portugal") %>% 
  clean_names()

id_names <- read.table("task_data/portugal_id_list.txt") %>% 
  rename(id = V1,
         name = V2)

# Clean adm2 names to match formatting of id_names
adm2_populations$n2 <- iconv(adm2_populations$n2, to = "ASCII//TRANSLIT")
adm2_populations$n2 <- gsub(" ", "_", toupper(adm2_populations$n2))

# Check for duplicates
n_occur <- data.frame(table(adm2_populations$n2))
n_occur[n_occur$Freq > 1,]

# There are two duplicates

adm2_populations <- adm2_populations %>%
  filter(!(n2 == "CALHETA")) %>%   # two municipalities on islands of Madeira and Azores
  filter(!(n2 == "LAGOA" & n1 == "Azores")) # remove municipality on Azores

# Filter centroid dataset to keep only matches with id_names

portugal_location_data <- adm2_populations %>% 
  filter(n2 %in% id_names$name) %>% 
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
adm2_populations$n1 <- iconv(adm2_populations$n1, to = "ASCII//TRANSLIT")
adm2_populations$n1 <- gsub(" ", "_", toupper(adm2_populations$n1))

portugal_adm_lookup <- adm2_populations %>%
  select(n1, n2)

saveRDS(portugal_adm_lookup, "portugal_adm_lookup.rds")


## Create dataframe with cleaned population and coordinates of adm1 units

adm1_populations <- read.delim("task_data/centroids_2006_1.txt") %>%
  filter(N0 == "Portugal") %>%
  clean_names()

adm1_populations$n1 <- iconv(adm1_populations$n1, to = "ASCII//TRANSLIT")
adm1_populations$n1 <- gsub(" ", "_", toupper(adm1_populations$n1))

portugal_adm1_location_data <- adm1_populations %>%
  filter(!(n1 %in% c("AZORES", "MADEIRA"))) %>%
  select(n1, pop, pop_cent_lon, pop_cent_lat) %>%
  rename(location = n1,
         population = pop,
         x = pop_cent_lon,
         y = pop_cent_lat) %>%
  arrange(location)

saveRDS(portugal_adm1_location_data, "portugal_adm1_location_data.rds")

 
# #########
# ## FRANCE
# #########
# 
# 
# adm3_populations <- read.delim("data/france_pops.txt", encoding = 'UTF-8') 
# adm3_populations$pop <- as.numeric(gsub(",", "", adm3_populations$pop))
# 
# id_names <- read.table("data/mobile_data/france/id_list.txt", encoding = 'UTF-8') %>% 
#   rename(id = V1,
#          name = V2)
# 
# # Check for any duplicates
# unique(adm3_populations$adm3[duplicated(adm3_populations$adm3)])
# # there are two places called Saint-Denis and two called Saint-Pierre
# # Both Saint-Pierre's are on islands so can be deleted
# # Keep the larger Saint-Denis (second is on an island)
# 
# adm3_populations <- adm3_populations %>%
#   filter(!(adm3 == "Saint-Pierre")) %>%   # remove two arrondissements on islands
#   filter(!(adm3 == "Saint-Denis" & pop == 204304)) # remove another arrondissement on island
# 
# 
# ## Clean adm3 names to match names in id_names, then filter to keep only matches
# 
# adm3_populations$adm3 <- iconv(adm3_populations$adm3, from = "UTF-8", to = "ASCII//TRANSLIT")
# adm3_populations$adm3 <- gsub(" ", "_", toupper(adm3_populations$adm3))
# 
# id_names$name <- iconv(id_names$name, from = "UTF-8", to = "ASCII//TRANSLIT")
# id_names$name <- gsub(" ", "_", toupper(id_names$name))
# 
# france_location_data <- adm3_populations %>% 
#   filter(adm3 %in% id_names$name) %>%    ##some boundaries have changed... how do we handle??
#   rename(location = adm3,
#          population = pop) %>% 
#   arrange(location)
# 
# # File with France population data where adm matches
# saveRDS(france_location_data, "data/france_location_data.rds")
# 
# 
# ## New data from Wes for 2007.
# # Clean here
# library(dplyr)
# library(janitor)
# 
# adm3_populations <- read.delim("C:/Users/jw2519/Downloads/centroids_2007_3.txt") %>% 
#   filter(N0 == "France") %>% 
#   clean_names()
# 
# adm3_populations$n3 <- iconv(adm3_populations$n3, to = "ASCII//TRANSLIT")
# adm3_populations$n3 <- gsub(" ", "_", toupper(adm3_populations$n3))
# 
# compare <- read.delim("C:/Users/jw2519/Downloads/centroids_2007_3.txt") %>% 
#   filter(N0 == "France") %>% 
#   clean_names()
# 
# 
# id_names <- read.table("data/mobile_data/france/id_list.txt", encoding = 'UTF-8') %>% 
#   rename(id = V1,
#          name = V2)
# 
# id_names$name <- iconv(id_names$name, from = "UTF-8", to = "ASCII//TRANSLIT")
# id_names$name <- gsub(" ", "_", toupper(id_names$name))
# 
# pop_names <- adm3_populations$n3
# mob_names <- id_names$name
# 
# 
# adm3_populations <- adm3_populations %>% 
#   mutate(n3 = replace(n3, n2 == "Paris", "PARIS"),
#          n3 = replace(n3, n3 == "CHATEAU-CHINON_(VILLE)", "CHATEAU-CHINON(VILLE)"),
#          n3 = stringr::str_replace(n3, "LE_", "LE"),
#          n3 = stringr::str_replace(n3, "LA_", "LA"),
#          n3 = stringr::str_replace(n3, "LES_", "LES"))
# 
# 
# x <- adm3_populations %>% 
#   filter(n3 %in% id_names$name)
# 
# y <- id_names %>% 
#   filter(!(name %in% adm3_populations$n3))
# 
# z <- adm3_populations %>% 
#   filter(!(n3 %in% id_names$name)) %>% 
#   select(n3, pop)
# # 3 of the areas in the population dataset are not featured in the mobile data
# 
# france_location_data_2007 <- adm3_populations %>% 
#   filter(n3 %in% id_names$name) %>%    
#   select(n3, pop, pop_cent_lon, pop_cent_lat) %>% 
#   rename(location = n3,
#          population = pop,
#          x = pop_cent_lon,
#          y = pop_cent_lat) %>% 
#   arrange(location)
# 
# # each Paris arrondissement features so combine to give one Paris area (as in mobile data)
# france_location_data_2007 <- france_location_data_2007 %>% 
#   group_by(location) %>% 
#   summarise(x = weighted.mean(x, population),
#             y = weighted.mean(y, population),
#             population = sum(population)) %>% 
#   select(location, population, x, y)
# 
# 
# # File with France population data where adm matches
# saveRDS(france_location_data_2007, "data/france_location_data_2007.rds")
