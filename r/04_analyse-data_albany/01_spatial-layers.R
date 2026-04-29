# Format spatial covariates, extract covariates for each sampling location

# Clear the environment
rm(list = ls())

# Set the study area
area <- "albany"

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

# Load the bathymetry data
bathy <- rast("data/albany/spatial/rasters/Princess-Royal-Harbour_LiDAR_Mean.tif")
plot(bathy)
