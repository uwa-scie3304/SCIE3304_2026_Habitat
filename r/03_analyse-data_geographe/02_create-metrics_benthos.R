# Combine and format benthos data for full subsets modelling

rm(list = ls())

library(tidyverse)

# Set the study area
area <- "geographe"

benthos <- readRDS(paste0("data/", area, "/raw/", area, "_benthos.RDS")) %>%
  dplyr::select(campaignid, sample, year, status, macroalgae, seagrasses,
                sand = unconsolidated, rock = consolidated,
                sessile_invertebrates, total_pts = total_points_annotated) %>%
  dplyr::mutate(reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", area, "/tidy/", area, "_benthos-count.RDS"))
