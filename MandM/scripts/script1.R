# Install any missing packages and require them

installed <- installed.packages()[,1]
pkgs <- c("XML", "tidyverse", "sf", "ggplot2",
          "ggspatial", "cowplot")
install <- pkgs[!(pkgs %in% installed)]
if(length(install)) install.packages(install, dependencies = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# Read-in the attribute data and the boundary file:
df <- read_csv("https://github.com/profrichharris/profrichharris.github.io/blob/main/MandM/data/diversity.csv?raw=true")
map <- st_read("https://github.com/profrichharris/profrichharris.github.io/raw/main/MandM/boundary%20files/cities.geojson", quiet = TRUE)

# Although more complex, at heart what the following code does is
# join the map to the data and then produce a separate map for
# each city and time period, using a consistent style

df |>
  pivot_longer(where(is.numeric), values_to = "index", names_to = "year") %>%
  mutate(year = paste0("20",substring(year, 3, 4))) %>%
  left_join(map, ., by = "OAXXCD") %>%
  mutate(group = paste(CITY, year, sep = " ~ ")) %>%
  split(.$group) %>%
  
  lapply(function(x) {
    
    ggplot(x, aes(fill = index)) +
      geom_sf(col = "transparent") +
      scale_fill_viridis_c("Diversity",
                           values = c(0,0.25,0.5,0.7,0.85,0.95,1)) +
      annotation_north_arrow(location = "tl",
                             style = north_arrow_minimal(text_size = 10),
                             height = unit(0.6, "cm"), width = unit(0.6, "cm")) +
      annotation_scale(location = "br", style = "ticks", line_width = 0.5,
                       text_cex = 0.5, tick_height = 0.4,
                       height = unit(0.15, "cm"), text_pad = unit(0.10, "cm")) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 8, hjust = 0.5),
            legend.title = element_text(size = 7, vjust = 3),
            legend.text =element_text(size = 6), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(t = 0,  
                                 r = 0,  
                                 b = 0,
                                 l = 0)) +
      labs(title = paste0(x$CITY[1], ": ", x$year[1]))
  }) -> g

# The following gets the common legend for the maps
# and stops it being printed 12 times -- once will be enough!

legend <- get_legend(g[[1]])
lapply(g, function(x) {
  x + theme(legend.position='none')
}) -> g

# This brings all the maps together as one

ggdraw(plot_grid(plot_grid(plotlist = g, ncol=3, align='v'),
                 plot_grid(NULL, legend, ncol=1, scale = 0.5),
                 rel_widths=c(1, 0.1),
                 rel_heights=c(1, 0,1))) -> g

# Print the maps to a .pdf file

pdf("maps.pdf", paper = "a4")
print(g)
dev.off()