#install.packages("lpSolve", dependencies = TRUE)
require(lpSolve)
require(RANN)
require(sp)
require(cartogram)

wards %>%
  st_transform(22234) %>%
  mutate(area = as.numeric(st_area(.)),
        w = as.numeric(sqrt(area))) ->
  wards_prj

wards_carto <- cartogram_cont(wards_prj, weight = "w", maxSizeError = 1.4, prepare = "none")
wards_combined <- st_union(wards_prj, wards_bak)

bbox <- st_bbox(wards_combined)
ydiff <- bbox[4] - bbox[2]
xdiff <- bbox[3] - bbox[1]

n <- 8 * nrow(wards_prj)
y <- sqrt(n / (xdiff/ydiff))
x <- n / y
x <- ceiling(x)
y <- ceiling(y)

grd <- st_sf(st_make_grid(wards_combined, n = c(x, y), square = FALSE))

ward_pts <- st_centroid(wards_carto, of_largest_polygon = TRUE)
grd_pts <- st_centroid(grd)

cost.mat <- st_distance(ward_pts, grd_pts)^2
row.rhs <- rep(1, nrow(ward_pts))
col.rhs <- rep(1, nrow(grd_pts))
row.signs <- rep("=", nrow(ward_pts))
col.signs <- rep("<=", nrow(grd_pts))

optimisation <- lp.transport(cost.mat, "min", row.signs, row.rhs, col.signs, col.rhs)$solution

mch <- sapply(1:nrow(optimisation), \(i) which(optimisation[i, ] == 1))
grd <- grd[mch,]
grd$High_income <- wards_prj$High_income

pts <- st_coordinates(st_centroid(grd))
tmp <- ceiling(nn2(pts, pts, k = 2)$nn.dists[,2])
i <- which(tmp > min(tmp))

grd <- grd[-i,]

ggplot() +
  geom_sf(data = wards_carto, aes(fill = High_income), col = "white", size = 0.4) +
  geom_sf(data = grd, aes(fill = High_income), col = "dark grey") +
  scale_fill_distiller("%", palette = "Blues", direction =  1) +
  theme_minimal() +
  theme(legend.position = "bottom")
