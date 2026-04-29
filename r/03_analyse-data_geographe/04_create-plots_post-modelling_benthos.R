# Create post-modelling habitat figures for reporting

# Clear your environment
rm(list = ls())

# Set the study area
area <- "geographe"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(scatterpie)
library(CheckEM)
library(grid)
library(viridis)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

# Load necessary spatial files
ausc <- st_read(paste0("data/", area, "/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")) %>%
  st_crop(e) %>%
  st_transform(4326)

marine_parks <- st_read(paste0("data/", area, "/spatial/shapefiles/western-australia_marine-parks-all.shp")) %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner"))

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

npz <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read(paste0("data/", area, "/spatial/shapefiles/amb_coastal_waters_limit.shp")) %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast(paste0("data/", area, "/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif")) %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# -------------------------------------------------------------------
# Predicted habitat plots - combined years
# -------------------------------------------------------------------

# Map pretty habitat names to raster layer prefixes in dat
habitat_lookup <- c(
  "Sand" = "sand",
  "Macroalgae" = "macro",
  "Seagrass" = "seagrass",
  "Sessile invertebrates" = "inverts",
  "Rock" = "rock"
)

# Optional habitat colours for other functions if needed
hab_cols <- c(
  "Sand" = "wheat",
  "Macroalgae" = "darkgoldenrod4",
  "Seagrass" = "forestgreen",
  "Rock" = "grey40",
  "Sessile invertebrates" = "plum"
)

# Plot extent
prediction_limits <- c(115.035, 115.57, -33.665, -33.34)

# Read combined prediction raster stack
dat <- readRDS(
  paste0(
    "output/model-output/", area, "/habitat/",
    area, "_predicted-habitat.rds"
  )
)

# Convert to plotting data frame
pred_class <- as.data.frame(dat, xy = TRUE)

# Add a dummy label only if plotting functions expect a year column
pred_class <- pred_class %>%
  dplyr::mutate(year = "Combined")

pred_plot <- normalise_se(data = pred_class)

# -------------------------------------------------------------------
# PART 1: Combined categorical habitat plot
# -------------------------------------------------------------------

p_cat <- categoricalhabitat_plot_single(
  pred_plot = pred_plot,
  prediction_limits = prediction_limits
)

print(p_cat)

ggsave(
  filename = paste0(
    "plots/", area, "/", area,
    "_predicted-habitat-categorical.png"
  ),
  plot = p_cat,
  height = 6,
  width = 8,
  dpi = 600,
  units = "in",
  bg = "white"
)

# -------------------------------------------------------------------
# PART 2: Combined dominant habitat plot
# -------------------------------------------------------------------

p_dom <- dominantbenthos_plot_single(
  pred_plot = pred_plot,
  prediction_limits = prediction_limits
) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = -0.1, unit = "cm")
  )

print(p_dom)

ggsave(
  filename = paste0(
    "plots/", area, "/", area,
    "_predicted-dominant-habitat.png"
  ),
  plot = p_dom,
  height = 6,
  width = 8,
  dpi = 600,
  units = "in",
  bg = "white"
)

# -------------------------------------------------------------------
# PART 3: Combined dominant habitat + combined SE
# -------------------------------------------------------------------

# This uses the multi-year function with a single "Combined" entry.
# If the function facets by year, this should still work.
dat_list <- list("Combined" = dat)

p_dom_se <- dominantbenthos_plot_multi(
  dat_list = dat_list,
  prediction_limits = prediction_limits
)

print(p_dom_se)

ggsave(
  filename = paste0(
    "plots/", area, "/", area,
    "_predicted-dominant-benthos-and-combined-se.png"
  ),
  plot = p_dom_se,
  height = 7,
  width = 8,
  dpi = 900,
  units = "in",
  bg = "white"
)

# -------------------------------------------------------------------
# PART 4: Combined individual habitat plots
# -------------------------------------------------------------------

for (habitat_name in names(habitat_lookup)) {
  
  message("Building individual habitat plot for: ", habitat_name)
  
  layer_stub <- habitat_lookup[[habitat_name]]
  
  p_hab <- individualbenthic_plot(
    habitat_name = habitat_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,
    se_limits = NULL
  )
  
  print(p_hab)
  
  out_name <- habitat_name %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "-")
  
  ggsave(
    filename = paste0(
      "plots/", area, "/", area,
      "_predicted-individual-habitat_", out_name, ".png"
    ),
    plot = p_hab,
    height = 5,
    width = 8,
    dpi = 900,
    units = "in",
    bg = "white"
  )
}

# ---- Scatterpie data prep ----

# Set the extent of the study
e <- ext(114.8, 116, -33.8, -33)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast(paste0("data/", area, "/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif")) %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

metadata_bathy_derivatives <- readRDS(
  paste0("data/", area, "/tidy/", area, "_metadata-bathymetry-derivatives.rds")
) %>%
  clean_names()

benthos <- readRDS(
  paste0("data/", area, "/tidy/", area, "_benthos-count.RDS")
) %>%
  dplyr::rename(
    Macroalgae = macroalgae,
    Seagrass = seagrasses,
    Sand = sand,
    Rock = rock,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives, by = c("campaignid", "sample", "year", "status")) %>%
  arrange(desc(Sand))

hab_fills <- scale_fill_manual(
  name = "Habitat",
  limits = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand"),
  values = c(
    "Rock" = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae" = "darkgoldenrod4",
    "Seagrass" = "forestgreen",
    "Sand" = "wheat"
  )
)

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
  # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

# -------------------------------------------------------------------
# Scatterpie plot - combined years
# -------------------------------------------------------------------

site_limits <- c(115.0, 115.67, -33.3, -33.65)

benthos_combined <- benthos %>%
  dplyr::filter(
    is.finite(longitude_dd),
    is.finite(latitude_dd)
  ) %>%
  dplyr::arrange(desc(Sand))

p_scatterpie <- scatterpie_plot_single(
  benthos_year = benthos_combined,
  site_limits = site_limits,
  pie_radius = 0.005
)

print(p_scatterpie)

ggsave(
  filename = paste0(
    "plots/", area, "/", area, "_scatterpie.png"
  ),
  plot = p_scatterpie,
  height = 6,
  width = 10,
  dpi = 300,
  bg = "white"
)
