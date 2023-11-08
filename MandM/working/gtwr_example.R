require(tidyverse)
require(sf)
require(lubridate)

try(setwd("C:/Users/profr/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  
try(setwd("~/Dropbox/Geography and A Geographer/Draft Chapters/Section 3 Urban and Social Geography/Chapter 10 - Harris & QuSS/Analysis/Data/"), silent = TRUE)  

read_csv("covid_data.csv") %>%
  select(MSOA11CD, MSOA11NM, TCITY15NM, PLACE, regionName, IMD,
         starts_with("age"), carebeds, AandE, `All Ages`,
         starts_with("2020-"), starts_with("2021-"), starts_with("2022-")) %>%
  mutate(across(starts_with("age"), ~ round(. / `All Ages` * 100, 1))) %>%
  rename(NPersons = `All Ages`) %>%
  mutate(`age0-11` = `age0-1` + `age2-11`,
         `age12-17` = `age12-16` + `age17`,
         `age25-34` = `age25-29` + `age30-34`,
         `age50-59` = `age50-54` + `age55-59`,
         `age60-69` = `age60-64` + `age65-69`,
         `age70+` = `age70-74` + `age75-79` + `age80-84` + `age85-89` + `age90+`) %>%
  pivot_longer(c(starts_with("2020-"), starts_with("2021-"), starts_with("2022-")),
                      names_to = "date", values_to = "NCases") %>%
  mutate(month = substr(date, 1, 7)) %>%
  group_by(month, MSOA11CD) %>%
  summarise(across("regionName", first),
            across("TCITY15NM", first),
            across("PLACE", first),
            across(starts_with("IMD"), first),
            across(starts_with("age"), first),
            across("carebeds", first),
            across("AandE", first),
            across("NCases", sum),
            across("NPersons", sum), .groups = "keep") %>%
  mutate(Rate = round(NCases / NPersons * 1000, 1)) %>%
  ungroup %>%
  arrange(month) %>%
  mutate(month_code = as.numeric(factor(month))) -> df
  

names(df) <- gsub("-", ".", names(df))

df %>%
  filter(grepl("Manchester", PLACE)) %>%
  mutate(id = 1:nrow(.)) ->
  df


setwd("C:/Users/profr/Dropbox/Data/Boundary Files/England_msoa_2011_sgen_clipped")

read_sf("england_msoa_2011_sgen_clipped.shp") %>%
  rename(MSOA11CD = code) %>%
  select(-name) %>%
  inner_join(df, by = "MSOA11CD") %>%
  rename(age70plus = `age70+`) %>%
  mutate(density = as.numeric(NPersons/st_area(.))) ->
  covid  

setwd("C:/Users/profr/Dropbox/github/MandM/workspaces")
save(covid, file = "manchester.RData")

covid_sp <- as_Spatial(covid)
fml <- formula(Rate ~ IMD + age0.11 + age25.34 + age50.59 + carebeds + poly(density, 2))
bw <- bw.gtwr(fml, data = covid_sp, obs.tv = covid$month_code, adaptive = TRUE)
gtwrmod <- gtwr(fml, data = covid_sp, st.bw = bw, obs.tv = covid$month_code, adaptive = TRUE)

save(covid, covid_sp, fml, bw, gtwrmod, file = "gtwrmod.RData")

