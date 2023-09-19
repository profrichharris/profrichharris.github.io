if(!("cartogram" %in% installed.packages()[,1])) install.packages("cartogram")
if(!("sf" %in% installed.packages()[,1])) install.packages("sf")
if(!("lpSolve" %in% installed.packages()[,1])) install.packages("lpSolve") 

require(sf)

create_carto <- \(x, w = NULL, k = 2, itermax = 25, maxSizeError = 1.4) {
  if(class(x)[1] != "sf") stop("Object x is not of class sf")
  if(is.null(w)) x$w <- as.numeric(st_area(x))^(1/k)
  cartogram::cartogram_cont(x, "w", itermax = itermax, maxSizeError = maxSizeError, prepare = "none")
}


create_grid <- \(x, m = 6) {
  bbox <- st_bbox(x)
  ydiff <- bbox[4] - bbox[2]
  xdiff <- bbox[3] - bbox[1]
  
  n <- m * nrow(x)
  ny <- sqrt(n / (xdiff/ydiff))
  nx <- n / ny
  nx <- ceiling(nx)
  ny <- ceiling(ny)
  
  grd <- st_sf(st_make_grid(x, n = c(nx, ny), square = FALSE))
}


allocate_grid <- \(x, grd, k = 2) {
  x_pts <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)
  grd_pts <- st_centroid(grd)
  
  cost.mat <- st_distance(x_pts, grd_pts)^k
  row.rhs <- rep(1, length(x_pts))
  col.rhs <- rep(1, nrow(grd_pts))
  row.signs <- rep("=", length(x_pts))
  col.signs <- rep("<=", nrow(grd_pts))

  optimisation <- lpSolve::lp.transport(cost.mat, "min", row.signs, row.rhs, col.signs, col.rhs)$solution
  
  mch <- sapply(1:nrow(optimisation), \(i) which(optimisation[i, ] == 1))
  grd <- st_sf(grd[mch,])
  cbind(grd, st_drop_geometry(x))
}


create_layers <- \(x, grd) {
  
  area <- sapply(1: nrow(x), \(i) {
    y <- st_intersects(x[i, ], grd)[[1]]
    if(i %in% y) {
      if(length(y) == 1) return(st_area(x[i, ]))
      if(length(y) > 1) {
        area <- st_area(x[i, ])
        overlaps <- y[-(y == i)]
        area <- area - st_area(st_intersection(st_geometry(x[i, ]), st_geometry(grd[overlaps[j], ])))
        return(area) 
      }
    } else {
      return(0)
    }
  })

  i <- which(area > 1.5*as.numeric(st_area(grd))[1])
  list(x[i, ], grd[-i, ])
}








merge_maps <- \(x, grd) {
  pts_grd <- st_coordinates(st_centroid(st_geometry(grd)))
  pts_original <- cbind(grd$xs, grd$ys)
  d <- sqrt((pts_grd[, 1] - pts_original[, 1])^2 + (pts_grd[, 2] - pts_original[, 2])^2)
  if(!("RANN" %in% installed.packages()[,1])) install.packages("RANN")    
  dknn <- ceiling(RANN::nn2(pts_grd, pts_grd, k = 2)$nn.dists[, 2])
  i <- which(d < min(dknn)/2 & grd$area > 2*as.numeric(st_area(grd))[1])
  list(x[i, ], grd[-i, ])
}




ggplot() +
  geom_sf(data =  wards_carto, aes(fill = High_income), col = "white", size = 0.4) +
  scale_fill_distiller("%", palette = "Blues", direction =  1) +
  theme_minimal() +
  theme(legend.position = "bottom")


ggplot() +
  geom_sf(data = tmp[[1]], aes(fill = High_income), col = "white", size = 0.4) +
  geom_sf(data = tmp[[2]], aes(fill = High_income), col = "dark grey") +
  scale_fill_distiller("%", palette = "Blues", direction =  1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Hexogram % of the population who are higher earners",
    subtitle = "South African Western Cape (2011)"
  )

ggsave()
