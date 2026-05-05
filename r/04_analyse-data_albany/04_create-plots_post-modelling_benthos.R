# Create post-modelling habitat figures

# Clear your environment
rm(list = ls())

# Set the study area
area <- "albany"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(scales)
library(grid)
library(viridis)

# Plot extent
prediction_limits <- c(117.84, 117.94, -35.09, -35.02)
e <- ext(prediction_limits)

# Load spatial files
ausc <- st_read(
  paste0("data/", area, "/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp"),
  quiet = TRUE
) %>%
  st_crop(e) %>%
  st_transform(4326)
plot(ausc)

# Bathymetry
bathy <- rast("data/albany/spatial/rasters/Princess-Royal-Harbour_LiDAR_Mean.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

# Read prediction raster
dat <- readRDS(
  paste0(
    "output/model-output/", area, "/habitat/",
    area, "_predicted-seagrass.rds"
  )
)

# Find seagrass prediction column
pred_col <- if ("p_seagrass.fit" %in% names(dat)) {
  "p_seagrass.fit"
} else if ("seagrass" %in% names(dat)) {
  "seagrass"
} else {
  names(dat)[!names(dat) %in% c("x", "y")][1]
}

# Build plot
p_seagrass <- ggplot() +
  geom_raster(
    data = dat,
    aes(x = x, y = y, fill = .data[[pred_col]])
  ) +  scale_fill_viridis_c(
    name = "Probability",
    na.value = "transparent",
    limits = c(0.12, 0.25),
    oob = scales::squish
  ) +
  geom_contour(
    data = bathy,
    aes(x = x, y = y, z = Depth),
    colour = "firebrick",
    breaks = c(-2, -3, -5),
    linewidth = 0.5
  ) +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
  coord_sf(
    xlim = prediction_limits[1:2],
    ylim = prediction_limits[3:4],
    crs = 4326,
    expand = FALSE
  ) +
  labs(
    title = "Predicted seagrass habitat",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width = unit(0.45, "cm")
  )

print(p_seagrass)

ggsave(
  filename = paste0(
    "plots/", area, "/", area,
    "_predicted-seagrass.png"
  ),
  plot = p_seagrass,
  height = 5,
  width = 6,
  dpi = 900,
  units = "in",
  bg = "white"
)
