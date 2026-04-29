###
# Project: SCIE3304 Habitat group 2026
# Data:    2025 Squidle + Benthic annotations
# Task:    Format Squidle + data
# author:  Abbey Gibbons
# date:    April 2026
##

# Clear obejcts from the environment
rm(list = ls())
# install.packages('remotes')
# library('remotes')
# options(timeout=9999999)
# 
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
# Load libraries
library(tidyverse)
library(CheckEM)
# library(SQAPI)


# Load the schema file to get caab codes ----

schema <- read.csv("data/raw/squidle-schema_20250325.csv") %>%
  dplyr::mutate(caab_code = as.character(vocab_registry.caab)) %>%
  dplyr::select(uuid, caab_code) %>%
  glimpse()

# Download the data from Squidle 
# Save the data in the data/raw/ folder, rename the file
# Change the filename below to the new file (remember you can use tab to choose the file)
raw_benthos <- read.csv("data/raw/2026-04_SCIE3304_HABITAT_BOSS_benthos.csv")

benthos <- raw_benthos %>%
  clean_names() %>%
  dplyr::rename(campaignid = any_of(c("point_media_deployment_campaign_key",
                                      "point_media_deployment_campaign_name")),
                sample = any_of(c("point_media_deployment_key",
                                  "point_media_deployment_name")),
                uuid = label_uuid) %>%
  left_join(schema) %>%
  dplyr::mutate(caab_code = if_else(label_lineage_names %in% c("Unscorable", "3 Field of View > Open"), "00000001", caab_code)) %>%
  left_join(CheckEM::catami) 

# Check here that they are all open water - if they are not you need to talk to Brooke about how to fix
benthos_missing <- benthos %>%
  filter(caab_code %in% NA) %>%
  distinct(label_lineage_names)

benthos_clean <- benthos %>%
  dplyr::mutate(count = 1) %>%
  group_by(campaignid, sample, caab_code, across(starts_with("level")), family, genus, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  glimpse()

write.csv(benthos_clean, file = paste0("data/tidy/",
                                 unique(benthos_clean$campaignid),
                                 "_benthos-count.csv"), # Change here for relief
          row.names = F)