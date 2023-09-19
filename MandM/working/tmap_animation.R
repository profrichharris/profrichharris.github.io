t <- tm_graticules(col = "light grey") +
  tm_shape(municipal[-128,], is.master = TRUE) +
  tm_polygons(col = "grey", border.col = "black") +
  tm_shape(municipal[-128,]) +
  tm_fill("No_schooling_gp", palette = "RdYlBu", title = "%") +
  tm_borders(col = "white") +
  tm_facets(along = "ProvinceName", free.coords = FALSE) +
  tm_legend(title = "Percentage of Population with No Schooling", bg.color = "white", bg.alpha = 0.7) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom"), bg.color = "white") +
  tm_credits("Source: 2011 Census / Statistics South Africa", bg.color = "white")

setwd("C:/Users/profr/Dropbox/github/MandM")
tmap_animation(t, filename = "no_schooling.gif", delay = 100)



tmap_mode("plot")

tm_graticules(col = "light grey") +
  tm_shape(municipal[-128,]) +
  tm_fill("No_schooling_gp", palette = "RdYlBu", title = "% Population with No Schooling",
          legend.is.portrait = FALSE) +
  tm_borders(col = "white") +
  tm_facets(by = "ProvinceName", free.coords = TRUE) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(legend.outside.position = "bottom")

tmap_save(tmap_last(), "facet_map.jpg", height = 6, units = "in")