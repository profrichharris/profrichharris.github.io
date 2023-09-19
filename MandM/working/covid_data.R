require(tidyverse)
require(sf)

try(setwd("C:/Users/profr/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  
try(setwd("~/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  

read_csv("covid_data.csv") %>%
  select(MSOA11CD, MSOA11NM, LtlaCode, LtlaName, regionName, IMD,
         starts_with("age"), carebeds, AandE, `All Ages`, NCases) %>%
  mutate(Rate = round(NCases / `All Ages` * 1000, 1)) %>%
  mutate(across(starts_with("age"), ~ round(. / `All Ages` * 100, 1))) %>%
  rename(NPersons = `All Ages`) %>%
  mutate(`age0-11` = `age0-1` + `age2-11`,
         `age12-17` = `age12-16` + `age17`,
         `age25-34` = `age25-29` + `age30-34`,
         `age50-59` = `age50-54` + `age55-59`,
         `age60-69` = `age60-64` + `age65-69`,
         `age70+` = `age70-74` + `age75-79` + `age80-84` + `age85-89` + `age90+`) %>%
  select(MSOA11CD, MSOA11NM, LtlaCode, LtlaName, regionName, Rate, IMD, `age0-11`, 
         `age12-17`, `age18-24`, `age25-34`, `age35-39`, `age50-59`, `age60-69`, `age70+`,
         carebeds, AandE, NPersons, NCases) -> df

names(df) <- gsub("-", "\\.", names(df))

try(setwd("C:/Users/profr/Dropbox/Data/Boundary Files/England_msoa_2011_sgen_clipped"), silent = TRUE)
try(setwd("~/Dropbox/Data/Boundary Files/England_msoa_2011_sgen_clipped"), silent = TRUE)

read_sf("england_msoa_2011_sgen_clipped.shp") %>%
  rename(MSOA11CD = code) %>%
  select(-name) %>%
  inner_join(df, by = "MSOA11CD") %>%
  rename(age70plus = `age70+`) %>%
  mutate(density = as.numeric(NPersons/st_area(.))) ->
  covid

try(setwd("C:/Users/profr/Dropbox/github/MandM/data"), silent = TRUE)
try(setwd("~/Dropbox/github/MandM/data"), silent = TRUE)

write_sf(covid, "covid.geojson")



