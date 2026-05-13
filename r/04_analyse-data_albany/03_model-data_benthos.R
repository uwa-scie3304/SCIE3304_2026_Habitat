rm(list=ls())

library(tidyverse)
library(mgcv)
library(terra)
library(CheckEM)
library(patchwork)

# Set the study area
area <- "albany"

metadata_bathy_derivatives <- readRDS(paste0("data/", area, "/tidy/", area, "_metadata-bathymetry-derivatives.rds")) %>%
  mutate(sample = as.character(sample)) %>%
  glimpse()

# Bring in and format the data----
habi <- readRDS(paste0("data/", area, "/tidy/", area, "_benthos-count.RDS")) %>%
  left_join(metadata_bathy_derivatives, by = join_by(sample)) %>%
  dplyr::filter(!is.na(geoscience_depth)) %>%
  # dplyr::filter(!geoscience_roughness > 3) %>% # Filter outliers - check
  glimpse()

model_dat <- habi %>%
  pivot_longer(cols = c(Macroalgae, Sand, Rock, Reef, Seagrasses, Halophila, Posidonia),
               names_to = "response", values_to = "number") %>%
  select(campaignid, sample, response, number, geoscience_depth, geoscience_aspect, geoscience_roughness) %>%
  glimpse()

# Set predictor variables---
pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness"
               # , "geoscience_detrended"
               )

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(model_dat[ , pred.vars]), 2)

# Review of individual predictors for even distribution---
CheckEM::plot_transformations(pred.vars = pred.vars, dat = model_dat)

# Seagrass simple model
m_seagrass.simple <- gam(Seagrasses ~
                    s(geoscience_depth, k = 5, bs = "cr"), # discuss k
                  data = habi)
summary(m_seagrass.simple)
plot(m_seagrass.simple)

# Seagrass more correct model
m_seagrass <- gam(cbind(Seagrasses, total_pts - Seagrasses) ~
                    s(geoscience_depth, k = 5, bs = "cr"), # discuss k
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrass)
plot(m_seagrass)

# Make one for Halophila
m_halophila <- gam(cbind(Halophila, total_pts - Halophila) ~
                    s(geoscience_depth, k = 5, bs = "cr"), # discuss k
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_halophila)
plot(m_halophila)

# Make one for Posidonia
m_posidonia <- gam(cbind(Posidonia, total_pts - Posidonia) ~
                     s(geoscience_depth, k = 5, bs = "cr"), # discuss k
                   data = habi, method = "REML", family = binomial("logit"))
summary(m_posidonia)
plot(m_posidonia)

# Read predictor rasters to predict onto
preds <- readRDS(paste0("data/", area, "/spatial/rasters/", area, "_bathymetry-derivatives.rds"))

preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# Seagrass overall: predict, rasterise and plot
predhab <- cbind(preddf, "p_seagrass" = predict(m_seagrass, preddf, type = "response", se.fit = T)) %>%
  glimpse()

prasts <- rast(predhab %>%
                 dplyr::select(x, y, starts_with("p_")),
               crs = "epsg:4326")

plot(prasts)
summary(prasts)

# ----------------------------------------
# Save outputs
# ----------------------------------------

# extract modeled habitat type
pred_col <- grep("^p_.*\\.fit$", names(predhab), value = TRUE)
habitat <- sub("^p_(.*)\\.fit$", "\\1", pred_col)[1]

# dataframe
saveRDS(
  predhab,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".rds")
)

# raster
writeRaster(
  prasts,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".tif"),
  overwrite = TRUE
)

# Posidonia: predict, rasterise and plot
predhab <- cbind(preddf, "p_posidonia" = predict(m_posidonia, preddf, type = "response", se.fit = T)) %>%
  glimpse()

prasts <- rast(predhab %>%
                 dplyr::select(x, y, starts_with("p_")),
               crs = "epsg:4326")

plot(prasts)
summary(prasts)

# extract modeled habitat type
pred_col <- grep("^p_.*\\.fit$", names(predhab), value = TRUE)
habitat <- sub("^p_(.*)\\.fit$", "\\1", pred_col)[1]

# dataframe
saveRDS(
  predhab,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".rds")
)

# raster
writeRaster(
  prasts,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".tif"),
  overwrite = TRUE
)


# Halophila: predict, rasterise and plot
predhab <- cbind(preddf, "p_halophila" = predict(m_halophila, preddf, type = "response", se.fit = T)) %>%
  glimpse()

prasts <- rast(predhab %>%
                 dplyr::select(x, y, starts_with("p_")),
               crs = "epsg:4326")

plot(prasts)
summary(prasts)

# extract modeled habitat type
pred_col <- grep("^p_.*\\.fit$", names(predhab), value = TRUE)
habitat <- sub("^p_(.*)\\.fit$", "\\1", pred_col)[1]

# dataframe
saveRDS(
  predhab,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".rds")
)

# raster
writeRaster(
  prasts,
  paste0("output/model-output/", area, "/habitat/",
         area, "_predicted-", habitat, ".tif"),
  overwrite = TRUE
)
