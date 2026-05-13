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


seagrass_with_epiphytes_tags <- read.csv(paste0("data/", area, "/tidy/2026-04_SCIE3304_HABITAT_BOSS_benthos-count.csv")) %>%
  dplyr::mutate(tag_names = case_when(level_2 %in% "Seagrasses" & tag_names %in% "" ~ "without epiphyte",
                                      .default = tag_names)) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2,
                                    level_2 %in% "Seagrasses" & genus %in% "Posidonia" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Amphibolis" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Halophila" ~ genus,
                                    level_2 %in% "Seagrasses" & genus %in% "Zostera" ~ genus,
                                    level_2 %in% "Seagrasses" & is.na(genus) ~ "Unknown Seagrass",
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3,
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3,
                                    level_2 %in% "Unscorable" ~ level_2)) %>%
  dplyr::mutate(habitat = if_else(tag_names %in% "Epiphyte", paste0(habitat, " with epiphyte"), habitat)) %>%
    dplyr::mutate(habitat = if_else(tag_names %in% "without epiphyte", paste0(habitat, " without epiphyte"), habitat)) %>%
  dplyr::select(campaignid, sample, habitat, count) %>%
  group_by(campaignid, sample, habitat) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::mutate(total_points_annotated = sum(count)) %>%
  ungroup() %>%
 
  pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) %>%
  dplyr::select(campaignid, sample, 
                Macroalgae, 
                'Posidonia without epiphyte', 
                'Posidonia with epiphyte', 
                'Amphibolis without epiphyte', 
                'Amphibolis with epiphyte', 
                'Halophila without epiphyte', 
                'Halophila with epiphyte',
                'Zostera without epiphyte', 
                # 'Zostera with epiphyte',
                'Unknown Seagrass without epiphyte',
                'Unknown Seagrass with epiphyte',
                Sand = 'Unconsolidated (soft)', 
                Rock = 'Consolidated (hard)',
                total_pts = total_points_annotated, Unscorable) %>%
  dplyr::mutate(Reef = Macroalgae + Rock,
                `Seagrasses without epiphyte` = `Posidonia without epiphyte` + `Amphibolis without epiphyte` + `Halophila without epiphyte` + `Zostera without epiphyte` + `Unknown Seagrass without epiphyte` + `Posidonia with epiphyte` + `Amphibolis with epiphyte` + `Halophila with epiphyte` + 
                  # `Zostera with epiphyte` + 
                  `Unknown Seagrass with epiphyte`,
                `Seagrasses with epiphyte` = `Posidonia with epiphyte` + `Amphibolis with epiphyte` + `Halophila with epiphyte` + 
                  #`Zostera with epiphyte' + 
                  `Unknown Seagrass with epiphyte`) %>%
  mutate(sample = sub(".*_(\\d+)\\s*$", "\\1", sample)) %>%
  glimpse()

names(seagrass_with_epiphytes_tags) %>% sort()

saveRDS(seagrass_with_epiphytes_tags, paste0("data/", area, "/tidy/", area, "_benthos-count-with-tags.RDS"))


# Seagrass with and without epiphytes scatter piecharts ----
metadata <- read_csv("data/albany/raw/SCIE3304 Metadata and labsheets - SCIE3304-2026_Metadata.csv") %>%
  dplyr::select(sample, longitude_dd, latitude_dd, date_time, location, site, depth_m) %>%
  dplyr::mutate(sample = paste0("2026-04_SCIE3304_HABITAT_BOSS_", sample)) %>%
  mutate(sample = sub(".*_(\\d+)\\s*$", "\\1", sample)) %>%
  glimpse()

seagrass_pies <- seagrass_with_epiphytes_tags %>%
  left_join(metadata)

leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
  addLayersControl(baseGroups = c("World Imagery", "Open Street Map"), options = layersControlOptions(collapsed = FALSE)) %>%
  addMinicharts(seagrass_pies$longitude_dd, seagrass_pies$latitude_dd, 
                type = "pie", #colorPalette = cols_seagrasses, 
                chartdata = seagrass_pies[grep("Seagrasses", names(seagrass_pies))], 
                width = 40, 
                transitionTime = 0) %>%
  setView(mean(as.numeric(seagrass_pies$longitude_dd)),
          mean(as.numeric(seagrass_pies$latitude_dd)), zoom = 12)
