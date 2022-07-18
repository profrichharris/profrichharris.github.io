# Load the required packages and install them if they are not available
# Because I am presently using R version 4.2.1 but there is not yet a version of the package
# ipumsr available for it, I use install_version() in the remotes package to use an older version
# It also allows the development package ipsumexamples to be installed from github

while(!require(remotes)) install.packages("remotes", dependencies = TRUE)
while(!require(ipumsr)) install_version("ipumsr", version = "0.5.0", upgrade = "never")
while(!require(ipumsexamples)) install_github("ipums/ipumsr/ipumsexamples")
while(!require(XML)) install.packages("XML", dependencies = TRUE)
while(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
while(!require(sf)) install.packages("sf", dependencies = TRUE)
while(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
while(!require(classInt)) install.packages("ggspatial", dependencies = TRUE)
while(!require(ggspatial)) install.packages("ggspatial", dependencies = TRUE)

# The rest of this code is adapted from the first parts of
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-geography.html
# I have simplified the code a little

fuel_missing_vals <- c(0, 60, 66, 70, 99)
fuel_solid_vals <- c(50:56, 61, 73, 74, 75)

# Read in some example IPUMS data for individuals and then aggregate (group) it to the
# geographic level, calculating the percentages using solid fuel for cooking in each year

system.file("extdata", "ipumsi_00011.xml", package = "ipumsexamples") %>%
  read_ipums_micro(., verbose = FALSE) %>%
  filter(COUNTRY == 170) %>%
  select(COUNTRY, GEOLEV1, YEAR, PERWT, FUELCOOK) %>%
  mutate(SOLIDFUEL = ifelse(FUELCOOK %in% fuel_solid_vals, 1, 0)) %>%
  filter(!(FUELCOOK %in% fuel_missing_vals)) %>%
  group_by(COUNTRY, GEOLEV1, YEAR) %>%
  summarise(SOLIDFUELWT = weighted.mean(SOLIDFUEL, w = PERWT), SOLIDFUEL = mean(SOLIDFUEL), .groups = "keep") %>%
  mutate(across(starts_with("SOLID"), ~ . * 100)) ->
  ipumsi_summary
 
# Read in the corresponding boundary file of the geographic areas
# Also simplify (remove vertices from) the outlines of the areas, which will make
# plotting them faster

system.file("extdata", "geo1_co1964_2005.zip", package = "ipumsexamples") %>%
  read_ipums_sf(., verbose = FALSE) %>%
  st_simplify(dTolerance = 100, preserveTopology = TRUE) -> 
  colombia_shape

# Join the boundary file to the attribute data
# It uses one of the in-built helper functions in the ipumsr package to help
# make the handling of the IPSUM data & boundary files easier
  
ipumsi <- ipums_shape_inner_join(
    ipumsi_summary, 
    colombia_shape,
    by = c("GEOLEV1" = "GEOLEVEL1")
)

# Map the data

breaks <- classIntervals(ipumsi$SOLIDFUEL, n = 5, style = "equal")$brks
ggplot(data = ipumsi) +
  annotation_map_tile(type = "cartolight", progress = "none") +
  geom_sf(aes(fill = SOLIDFUEL), alpha = 0.8) +
  scale_fill_gradient("%", breaks = breaks, labels = round(breaks, 1),
                      low = "white", high = "dark red") +
  facet_wrap(~ YEAR) +
  xlim(c(79, 67)) +
  ylim(c(-5, 13)) +
  theme_minimal() +
  labs(
    title = "% of Children 0-5 Who Live in a Home That Cooks Using Solid Fuel",
    subtitle = "Colombia (1985, 1993, 2005) Census Data",
    caption = "Source: IPUMS-International"
  )
  


