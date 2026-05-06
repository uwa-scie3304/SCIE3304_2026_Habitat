# Combine and format benthos data for full subsets modelling

rm(list = ls())

library(tidyverse)

# Set the study area
area <- "albany"

benthos <- read.csv(paste0("data/", area, "/tidy/2026-04_SCIE3304_HABITAT_BOSS_benthos-count.csv")) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2,
                                    level_2 %in% "Seagrasses" & genus %in% "Posidonia" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Amphibolis" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Halophila" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Zostera" ~ genus,
                                    level_2 %in% "Seagrasses" & is.na(genus) ~ "Unknown Seagrass",
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3,
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3,
                                    level_2 %in% "Unscorable" ~ level_2)) %>%
  dplyr::select(campaignid, sample, habitat, count) %>%
  group_by(campaignid, sample, habitat) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::mutate(total_points_annotated = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) %>%
  dplyr::select(campaignid, sample, 
                Macroalgae, Posidonia, Amphibolis, Halophila, Zostera, 'Unknown Seagrass',
                Sand = 'Unconsolidated (soft)', Rock = 'Consolidated (hard)',
                total_pts = total_points_annotated, Unscorable) %>%
  dplyr::mutate(Reef = Macroalgae + Rock,
                Seagrasses = Posidonia + Amphibolis + Halophila + Zostera + `Unknown Seagrass`) %>%
  mutate(sample = sub(".*_(\\d+)\\s*$", "\\1", sample)) %>%
  glimpse()

# check how many samples
length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", area, "/tidy/", area, "_benthos-count.RDS"))
