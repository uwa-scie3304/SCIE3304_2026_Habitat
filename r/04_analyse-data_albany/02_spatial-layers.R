


e <- ext(117.845, 117.931, -35.086, -35.025)

albany <- rast("data/geographe/spatial/rasters/Albany_20230402_Albany_LiDAR_Mean.bag") %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  project("EPSG:4326") %>%
  crop(e) %>%
  trim()

plot(albany[[1]])