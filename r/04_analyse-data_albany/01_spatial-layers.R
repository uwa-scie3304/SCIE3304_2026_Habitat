# Format spatial covariates, extract covariates for each sampling location

# Clear the environment
rm(list = ls())

# Set the study area
area <- "albany"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(stars)
library(starsExtra)

# Load the bathymetry data
bathy <- rast("data/albany/spatial/rasters/Princess-Royal-Harbour_LiDAR_Mean.tif")
plot(bathy)

preds <- terrain(bathy, neighbors = 8,
                 v = c("aspect", "roughness"),
                 unit = "degrees")
names(preds) <- c("geoscience_aspect", "geoscience_roughness")

# # Create detrended bathymetry
# zstar <- st_as_stars(bathy)
# detre <- detrend(zstar, parallel = 8)
# detre <- as(object = detre, Class = "SpatRaster")
# names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds
                   # , detre[[1]]
                   ))
names(preds)[1] <- "geoscience_depth"

# Save the bathymetry derivatives
saveRDS(preds, file = paste0("data/", area, "/spatial/rasters/",
                             area, "_bathymetry-derivatives.rds"))

# Read in the metadata
metadata <- read.csv(paste0("data/", area, "/raw/SCIE3304 Metadata and labsheets - SCIE3304-2026_Metadata.csv")) %>%
  dplyr::select(sample, longitude_dd, latitude_dd, site) %>%
  glimpse()

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T, pch = 16, col = "red")

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf)) %>%
  filter_at(vars(geoscience_depth, geoscience_aspect, geoscience_roughness
                 # , geoscience_detrended
                 ),
            all_vars(!is.na(.))) %>% # Removes samples missing bathymetry derivatives - check these
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", area, "/tidy/", area, "_metadata-bathymetry-derivatives.rds"))
