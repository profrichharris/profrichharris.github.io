set.seed(20072022)
municipal$No_schooling2 <- sample(municipal$No_schooling, length(municipal$No_schooling))
municipal$No_schooling_gp2 <- cut(municipal$No_schooling2, brks, include.lowest = TRUE)

ggplot(data = municipal, aes(fill = No_schooling_gp2)) +
  geom_sf() +
  scale_fill_brewer("%", palette = "RdYlBu", direction = -1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Random permutation of the previous map's data"
  ) 

setwd("C:/Users/profr/Dropbox/github/MandM")
ggsave("map_permutation.jpg", bg = "white")