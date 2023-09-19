require(tidyverse)
require(data.table)
require(sf)
require(ggplot2)
require(classInt)
require(ggspatial)

setwd("C:/Users/profr/Dropbox/github/MandM/working")

download.file("https://github.com/profrichharris/profrichharris.github.io/raw/main/MandM/boundary%20files/MDB_Local_Municipal_Boundary_2011.geojson",
              "municipal.geojson", mode = "wb", quiet = TRUE)
# Source: https://dataportal-mdb-sa.opendata.arcgis.com/datasets/439676d2f2f146889a6eb92da80db780_0/about

read_sf("municipal.geojson") ->
  municipal
  

download.file("https://github.com/profrichharris/profrichharris.github.io/raw/main/MandM/data/table_2022-07-12_11-02-27.csv",
              "education.csv", mode = "wb", quiet = TRUE)
# Source: http://superweb.statssa.gov.za

read_csv("education.csv", skip = 9, n_max = 1, col_names = FALSE,
         col_select = 1:10) %>%
  mutate(across(everything(), ~ str_replace_all(., " ", "_"))) %>%
  mutate(across(everything(), ~ str_replace_all(., "/", "_"))) %>%
  unlist(use.names = FALSE) ->
  row_names

fread("education.csv",
      skip = 11, nrows = 234, select = 1:10,
      col.names = row_names) %>%
  as_tibble %>%
  separate(1, into = c("LocalMunicipalityCode", "LocalMunicipalityName"), sep = ": ") %>%
  mutate(Total = Total - (Unspecified + Not_applicable)) %>%
  mutate(across(where(is.numeric), ~ . / Total * 100)) %>%
  select(-Total, -Unspecified, -Not_applicable) %>%
  rowwise %>%
  mutate(Some_primary = sum(c_across(4: ncol(.)))) %>%
  mutate(Complete_primary = sum(c_across(5: ncol(.)))) %>%
  mutate(Some_secondary = sum(c_across(6: ncol(.)))) %>%  
  mutate(Grade_12_Std_10 = sum(c_across(7: ncol(.)))) %>%
  ungroup ->
  education

write_csv(education, "education.csv")

municipal %>%  
  left_join(education, by = c("LocalMunicipalityCode")) ->
  mapdata



download.file("https://github.com/profrichharris/profrichharris.github.io/blob/main/MandM/boundary%20files/hotosm_zaf_populated_places_points_shp.zip?raw=true",
              "cities.zip", mode = "wb", quiet = TRUE)
unzip("cities.zip")
## Source: https://data.humdata.org/dataset/hotosm_zaf_populated_places

read_sf("hotosm_zaf_populated_places_points.shp") %>%
  filter(place == "city") %>%
  mutate(population = as.numeric(population)) ->
  cities

require(remotes)
install_github("yutannihilation/ggsflabel")
require(ggsflabel)

g <- mapdata %>%
  ggplot() +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(aes(fill = No_schooling)) +
  geom_sf(data = cities) +
  geom_sf_label_repel(data = cities, aes(label = name), alpha = 0.7, size = 3) +
  scale_fill_gradient2("%", breaks = round(classIntervals(mapdata$No_schooling, n = 7, style = "jenks")$brks, 0),
                      low = "dark blue", mid = "white", high = "dark red", midpoint = median(mapdata$No_schooling)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(
    title = "Percentage of Population with No Schooling",
    subtitle = "2011 South African Census Data",
    caption = "Source: Statistics South Africa"
  )


jpeg("noschool.jpg", height = 8, width = 8, units = "in", res = 300)
print(g)
dev.off()




  
