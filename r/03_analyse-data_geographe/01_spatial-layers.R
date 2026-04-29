# Format spatial covariates, extract covariates for each sampling location

# Download Aus bathymetry raster from https://pid.geoscience.gov.au/dataset/ga/150050
# and save it in "data/geographe/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif"

# Clear the environment
rm(list = ls())

# Set the study area
area <- "geographe"

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)
library(rerddap)

# Set the extent of the study
e <- ext(115.04, 115.60, -33.67, -33.346)

# Load the bathymetry data
bathy <- rast(paste0("data/", area, "/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif")) %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(bathy)

# Create terrain metrics (bathymetry derivatives)
preds <- terrain(bathy, neighbors = 8,
                 v = c("aspect", "roughness"),
                 unit = "degrees")
names(preds) <- c("geoscience_aspect", "geoscience_roughness")

# Create detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "SpatRaster")
names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds, detre[[1]]))
names(preds)[1] <- "geoscience_depth"

# Save the bathymetry derivatives
saveRDS(preds, file = paste0("data/", area, "/spatial/rasters/",
                      area, "_bathymetry-derivatives.rds"))

# Read in the metadata

metadata <- readRDS(paste0("data/", area, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T)

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf)) %>%
  filter_at(vars(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
            all_vars(!is.na(.))) %>% # Removes samples missing bathymetry derivatives - check these
  dplyr::select(-ID) %>%
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", area, "/tidy/", area, "_metadata-bathymetry-derivatives.rds"))
