require(tidyverse)
require(sf)

try(setwd("C:/Users/profr/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  
try(setwd("~/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  

read_csv("covid_data.csv") %>%
  filter(regionName == "London") %>%
  select(starts_with("2020-"), starts_with("2021-"), starts_with("2022-")) %>%
  pivot_longer(everything(), values_to = "NCases", names_to = "date") %>%
  group_by(date) %>%
  summarise(NCases = sum(NCases)) %>%
  slice_max(NCases) %>%
  arrange(desc(NCases)) %>%
  select(date) %>%
  pull ->
  max.cases


read_csv("covid_data.csv") %>%
  select(MSOA11CD, MSOA11NM, LtlaName, regionName, IMD,
         starts_with("age"), carebeds, AandE, `All Ages`, starts_with(max.cases)) %>%
  mutate(across(starts_with("age"), ~ round(. / `All Ages` * 100, 1))) %>%
  rename(NPersons = `All Ages`) %>%
  mutate(`age0-11` = `age0-1` + `age2-11`,
         `age12-17` = `age12-16` + `age17`,
         `age25-34` = `age25-29` + `age30-34`,
         `age50-59` = `age50-54` + `age55-59`,
         `age60-69` = `age60-64` + `age65-69`,
         `age70+` = `age70-74` + `age75-79` + `age80-84` + `age85-89` + `age90+`) %>%
  select(MSOA11CD, MSOA11NM, LtlaName, regionName, IMD, `age0-11`, 
         `age12-17`, `age18-24`, `age25-34`, `age35-39`, `age50-59`, `age60-69`, `age70+`,
         carebeds, AandE, NPersons, starts_with(max.cases)) %>%
  rename(NCases = ncol(.)) -> df


names(df) <- gsub("-", ".", names(df))

try(setwd("C:/Users/profr/Dropbox/Data/Boundary Files/England_msoa_2011_sgen_clipped"), silent = TRUE)  
try(setwd("~/Dropbox/Data/Boundary Files/England_msoa_2011_sgen_clipped"), silent = TRUE)  

read_sf("england_msoa_2011_sgen_clipped.shp") %>%
  rename(MSOA11CD = code) %>%
  select(-name) %>%
  inner_join(df, by = "MSOA11CD") %>%
  rename(age70plus = `age70+`) %>%
  mutate(density = as.numeric(NPersons/st_area(.))) %>%
  mutate(rate = NCases / NPersons * 100) ->
  covid_xmas_2021

try(setwd("C:/Users/profr/Dropbox/github/MandM/data"), silent = TRUE)
try(setwd("~/Dropbox/github/MandM/data"), silent = TRUE)

write_sf(ldn_covid, "covid_xmas_2021.geojson")

