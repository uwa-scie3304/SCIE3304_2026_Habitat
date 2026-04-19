## This code should turn the images (.jpg format) into the correct files and format for the SQUIDLE+ BRUV analysis stuff. will also combine with the meta data sheet, ensure everything is saved in the right place and as the right file type i.e. images must be .jpg and metadat sheet should be .csv
## this code exports it to folder called full_res maybe???

## Creating a folder structure and formatting habitat images from BRUV imagery for SQUIDLE+
## Written by jacquomo.monk@utas.edu.au

## This code assumes you have a folder containing all habitat images called "Habitat" and one metadatadata csv file that is
## structured based on GlobalArchive standards found in the CheckEM user guide https://marine-ecology.shinyapps.io/CheckEM/
## Example data is provided in 202106_Huon_AMP_BRUV_metadatadata_Habitat.zip in this repo

## To transfer images to IMAS for ingestion into SQUIDLE+. 
## Use this transfer code:
## https://www.dropbox.com/request/Tfn77UCrpeDfTw6xaQvu
## Please note you are sharing the entire folder structure from Instution down (see below)

## Clean up environment
rm(list=ls())


### Load libraries
library(tidyverse)
library(magick)
library(lubridate)
library(leaflet)
library(fs)
library(googlesheets4)

## 1.0 Do some setup
### Set the path to the folder containing the images
folder_path <- choose.dir() #navigate to folder
setwd(folder_path)
getwd()

### Set up some extra attribute values not in GlobalArchive but needed in SQUIDLE+
data.camera_model = "GoPro 12 Black"
data.contact.primary = "tim.langlois@uwa.edu.au"
data.contact.secondary = "brooke.gibbons@uwa.edu.au"
data.funder = "UWA"
data.grant_no = "SCIE3304"

# ### Set up a naming conventions for folders etc. This is important to be the same/consistent between uploads
institute = "UWA" #set this for your institute. should remain the same between uploads
platform = "UWA_HabitatBOSS" #set this for each platform format should remain the same between uploads
campaign = "202607_Albany_BOSS" #set this for each campaign. format should remain the same between uploads
jpgname = "habitatBOSS" #dont change this unless you are working with a different platform
full_res = "full_res"#dont change this
thumbnails = "thumbnails" #dont change this


### Set up folder structure
#### Define the base directory (you can adjust this as needed)
base_dir <- file.path(institute, platform, campaign)

#### Define the folders to create
folders <- c("full_res", "thumbnails")

#### Create the folder structure
for (folder in folders) {
  dir_path <- file.path(base_dir, folder)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created folder: ", dir_path)
  } else {
    message("Folder already exists: ", dir_path)
  }
}


## 2.0 Read in metadata and sort out attribute names
## make sure column names in meta data sheet match the format of the code
#need to download and save the finished metadata sheet before running this line
## save as .csv file or this wont work and in the same working directory - this should be the folder "images"

url <- "https://docs.google.com/spreadsheets/d/1Gk-TETHFwD-DUj_y-pfyav10F-dnfFCe6SgLBbUk6oI/edit?gid=896363886#gid=896363886"

googlesheets4::gs4_deauth()
googlesheets4::gs4_auth(email = "tim.langlois@marineecology.io")

metadata <- googlesheets4::read_sheet(url, sheet = "SCIE3304-2026_Metadata")%>%
  
  
  mutate(site = sample) %>%
  # mutate( timestamp_start = parse_datetime(date_time)) %>%  #convert time date to UTC
  # with_tz("UTC") %>%
  # mutate(timestamp_start = format(timestamp_start, "%Y-%m-%dT%H:%M:%S %Z"))%>% #this part does not work
  mutate(pose.lat = latitude_dd) %>%
  mutate(pose.lon	= longitude_dd) %>%
  mutate( mutatepose.dep = depth_m) %>%
  mutate(pose.data.inclusion_probability = NA) %>%
  mutate(data.spatially_balanced	=  ifelse(!is.na(pose.data.inclusion_probability), "y", "n")) %>% #assign a yes/no based on if there is a inclusion probability
  mutate(        data.camera_model = data.camera_model) %>%
  mutate(        data.contact.primary =  data.contact.primary) %>%
  mutate(       data.contact.secondary = data.contact.secondary) %>%
  mutate(      data.funder = data.funder) %>%
  mutate(        data.grant_no = data.grant_no) %>% #optional. remove if not used # )%>%
  # mutate(timestamp_start = parse_datetime(date_time))%>%
  # #dplyr::select("site","timestamp_start":"data.grant_no")%>%
  glimpse()
str(metadata)

files <- list.files(pattern = "\\.csv$")
print(files)


### 2.1 Lets do some basic checks to make sure everything looks right in metadatadata contents
#### Check for missing values
missing_values <- metadata %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count")

print("Missing values per column:")
print(missing_values)

#### Check for duplicate rows
duplicates <- metadata %>%
  filter(duplicated(.))

if (nrow(duplicates) > 0) {
  print("Duplicate rows found:")
  print(duplicates)
} else {
  print("No duplicate rows found.")
}


#### Check data types and ranges
column_summary <- metadata %>%
  reframe(across(
    everything(),
    list(
      class = ~ class(.),
      min = ~ if (is.numeric(.)) min(., na.rm = TRUE) else NA,
      max = ~ if (is.numeric(.)) max(., na.rm = TRUE) else NA
    )))

print("Column data types and ranges:")
print(column_summary)

#### Check for any empty or zero-length strings in character columns
empty_strings <- metadata %>%
  summarise(across(where(is.character), ~ sum(. == "")))

print("Empty strings in character columns:")
print(empty_strings)

#### Check for any empty or negative depth values
if ("pose.dep" %in% colnames(metadata)) {
  depth_issues <- metadata %>%
    filter(is.na(pose.dep) | pose.dep < 0)
  
  if (nrow(depth_issues) > 0) {
    print("Rows with empty or negative depth values in 'Depth':")
    print(depth_issues)
  } else {
    print("No empty or negative depth values in 'Depth'.")
  }
} else {
  print("Column 'Depth' not found in metadatadata.")
}

## 3.0 Now lets work on checking and moving images into folders
### 3.1 Extra optional - Sometimes more cleaning is needed. In this example we are removing unnecessary part of a string at end of image file name
#### List all .jpg files in the folder
image_names <- list.files(file.path(getwd(),"Habitat"), pattern = "\\(jpg|jpeg|png|gif|bmp|tiff|JPG|JPEG|PNG|GIF|BMP|TIFF)$", full.names = TRUE)

#### Remove _Part1 from file names
cleaned_names <- gsub("_Part1", "", basename(image_names))

#### Create new file paths
new_file_paths <- file.path(file.path(getwd(),"Habitat"), cleaned_names)

#### Rename files
file.rename(image_names, new_file_paths)

#### Confirm renamed files
list.files(file.path(getwd(),"Habitat"))

### 3.2 Good idea to make sure all all image files to jpg and output into full_res folder
#### List all images files in the folder
image_files <- list.files(file.path(getwd(),"Habitat"),pattern = "\\.(jpg|jpeg|png|gif|bmp|tiff|JPG|JPEG|PNG|GIF|BMP|TIFF)$", full.names = TRUE)

#### Convert all image files to JPG format
##### set output folder
full_res_folder <- file.path(getwd(), "Habitat", "full_res")

##### Loop through and rename/move files
for (file in image_files) {
  tryCatch({
    ## Check if the file exists
    if (!file.exists(file)) {
      message(paste("File not found:", file))
      next
    }
    
    ## Construct new file name with .jpg extension
    new_file <- file.path(full_res_folder, gsub("\\..*$", ".jpg", basename(file)))
    
    ## Check if moving across drives, use file.copy + unlink instead
    if (!file.rename(file, new_file)) {
      if (file.copy(file, new_file)) {
        unlink(file) # Only remove original if copy succeeds
        message(paste("Copied and removed:", file, "to", new_file))
      } else {
        message(paste("Failed to copy:", file))
      }
    } else {
      message(paste("Moved:", file, "to", new_file))
    }
  }, error = function(e) {
    message(paste("Error with file:", file, "Error:", e$message))
  })
}

### 3.3 Now rename these new jpg files to include campaign
#### Note that this naming pattern needs to be standard for your dataset between campaigns.
#### List all JPG files in the folder
jpg_files <- list.files(full_res_folder, pattern = ".jpg", full.names = TRUE)
jpg_files <- list.files("full_res", pattern = ".jpg", full.names = TRUE) #test -> tested and normaly works 

#### Function to loop through each JPG file and rename it. 
replace_prefix <- function(file, old_prefix, campaign, jpgname, image_path) {
  new_name <- str_replace(basename(file), paste0("^", old_prefix), str_c(campaign, "_", jpgname, "_"))
  file.rename(file, file.path(image_path, new_name))
}

#### Apply to all jpg files
walk(jpg_files, replace_prefix, 
     old_prefix = "Huon_", #Change this for your data
     campaign = campaign, #add campaign to file name
     jpgname = jpgname, 
     image_path = full_res_folder)


### 3.4 Now lets join jpg images to metadatadata for SQ+
#### Sample data frame
final.metadatadata <- list.files(path = full_res_folder, pattern = "\\.jpg$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE) %>%
  as.data.frame() %>%
  rename(image.path = ".") %>%
  mutate(
    image = str_extract(image.path, "[^/]+$"),
    key = str_remove(image, "\\.jpg$"),
    site = str_extract(key, "\\d+_\\d+"),
    site = if_else(str_detect(key, "EXP"), paste0("EXP_", str_extract(site, "\\d+"))#Hack if you have weird text in your drop names
                   , site)) %>%
  left_join(metadata, by = 'site') %>%
  dplyr::select("key","site","timestamp_start","pose.lat","pose.lon","pose.dep":"data.grant_no")%>% #note you may need to change selection if you dont have a grant number
  glimpse()

#test for 3.4 -> tested and doesnt work for now everything else does above this one 
final.metadatadata <- list.files(path = "full_res", pattern = "\\.jpg$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE) %>%
  as.data.frame() %>%
  rename(image.path = ".") %>%
  mutate(
    image = str_extract(image.path, "[^/]+$"),
    key = str_remove(image, "\\.jpg$"),
    site = str_extract(key, "\\d+_\\d+"),
    site = if_else(str_detect(key, "EXP"), paste0("EXP_", str_extract(site, "\\d+"))#Hack if you have weird text in your drop names
                   , site)) %>%
  left_join(metadata, by = 'site') %>%
  dplyr::select("key","site","timestamp_start","pose.lat","pose.lon","pose.dep":"data.grant_no")%>% #note you may need to change selection if you dont have a grant number
  glimpse()

## 4.0 Plot metadatadata on map to make sure it looks ok
### Create a Leaflet map
leaflet(final.metadatadata) %>%
  addTiles() %>%
  addMarkers(lng = ~pose.lon, lat = ~pose.lat, label = ~key)

# ### Write out final metadatadata for later
# write.csv(final.metadatadata, file.path(file.path(getwd(), base_dir), "202106_Huon_AMP_BRUV_metadatadata_formatted_final.csv"), row.names = FALSE, na = "") ##change name as needed leaving metadatadata_formatted. Optional save if you need it

## 5.0 Now create thumbnails
### Create list of full res images
jpg_files <- list.files(full_res_folder, pattern = ".jpg", full.names = TRUE, ignore.case = TRUE)

### Set thumbnail folder
thumbnails_folder <- file.path(getwd(),base_dir, "thumbnails")

### Loop through the jpg files, resize, and save as thumbnails
for (jpg_file in jpg_files) {
  ## Read the image
  img <- image_read(jpg_file)
  
  ## Calculate the thumbnail dimensions to ensure a maximum height of 350px
  original_width <- image_info(img)$width
  original_height <- image_info(img)$height
  
  thumbnail_height <- min(350, original_height)  # Ensure thumbnail_height is a maximum of 350px
  thumbnail_width <- (thumbnail_height / original_height) * original_width  # Maintain the original aspect ratio
  
  ## Resize the image
  img_thumbnail <- image_scale(img, geometry = paste0(round(thumbnail_width), "x", round(thumbnail_height)))
  
  ## Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(jpg_file))
  
  ## Save the thumbnail in the output folder
  thumbnail_output_path <- file.path(thumbnails_folder, paste0(file_name, ".jpg"))
  image_write(img_thumbnail, path = thumbnail_output_path, format = "jpeg")
}


## 9.0 Now let's make deployment folder for each deployment, move these images into these folders and split metadatadata
### Quick checks before moving files to deployment folders
#### 9.1 Check site names metadatadata
unique_sites <- unique(final.metadatadata$site)
print(unique_sites)

### 9.2 Check number of files in full res and thumbnail folders - should have same number of files in each
#### Get list of jpg files in each folder
full_res_files <- list.files(full_res_folder, pattern = "\\.jpg$", full.names = FALSE)
thumbnails_files <- list.files(thumbnails_folder, pattern = "\\.jpg$", full.names = FALSE)

#### Count number of files in each folder and get file names
file_counts <- tibble(
  folder = c("Full Resolution", "Thumbnails"),
  file_count = c(length(full_res_files), length(thumbnails_files))
)

#### Get file names for comparison
file_names <- tibble(
  full_res_files = full_res_files,
  thumbnails_files = thumbnails_files
)

##### Check if they have the same number of files
if(length(full_res_files) == length(thumbnails_files)) {
  message("Both folders have the same number of jpg files.")
} else {
  message("The number of jpg files in each folder is different.")
}

##### Check if file names match
if(setequal(full_res_files, thumbnails_files)) {
  message("File names match between the folders.")
} else {
  message("File names do not match between the folders.")
} 


### 9.3 Now move images from full_res to subfolders in deployment folders
#### Extract site from 'key' column in 'metadatadata'
unique_imgs <- final.metadatadata %>%
  dplyr::select(site, key) %>%
  glimpse()

#### Move all full_res and thumbnail jpgs from full_res and thumbnails folders to full_res and thumbnails subfolders in each deployment 
path_to_folder <- file.path(getwd(), base_dir)

for (i in 1:nrow(unique_imgs)) {
  site <- unique_imgs$site[i]
  key <- unique_imgs$key[i]
  
  site_folder <- file.path(path_to_folder, site)
  full_res_subfolder <- file.path(site_folder, "full_res")
  thumbnails_subfolder <- file.path(site_folder, "thumbnails")
  
  ## Create site, full_res, and thumbnails folders if they don't exist
  if (!dir.exists(site_folder)) {
    dir.create(site_folder, recursive = TRUE)
  }
  if (!dir.exists(full_res_subfolder)) {
    dir.create(full_res_subfolder, recursive = TRUE)
  }
  if (!dir.exists(thumbnails_subfolder)) {
    dir.create(thumbnails_subfolder, recursive = TRUE)
  }
  
  ## Move only the matching full_res JPG file
  full_res_file <- file.path(full_res_folder, paste0(key, ".jpg"))
  if (file.exists(full_res_file) && !file.exists(file.path(full_res_subfolder, basename(full_res_file)))) {
    file.rename(full_res_file, file.path(full_res_subfolder, basename(full_res_file)))
  }
  
  ## Move only the matching thumbnail JPG file
  thumbnail_file <- file.path(thumbnails_folder, paste0(key, ".jpg"))
  if (file.exists(thumbnail_file) && !file.exists(file.path(thumbnails_subfolder, basename(thumbnail_file)))) {
    file.rename(thumbnail_file, file.path(thumbnails_subfolder, basename(thumbnail_file)))
  }
}


#### 9.4 Now let's split metadatadata csv into each deployment folder
for (site in unique_sites) {
  ## Subset final metadatadata for the current site
  site_metadatadata <- final.metadatadata[final.metadatadata$site == site, ]
  
  ## Remove the site column
  site_metadatadata <- site_metadatadata[, !names(site_metadatadata) %in% "site"]
  
  ## Define the file name and path
  file_name <- paste0(campaign, "_", platform, "_", site, "_metadatadata.csv")
  file_path <- file.path(path_to_folder, site, file_name)
  
  ## Create the directory if it doesn't exist
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  ## Write the subsetted metadatadata to a CSV file
  write.csv(site_metadatadata, file_path, row.names = FALSE)
}

### 10 Run some checks to makes sure each deployment folder has correct number of files 
#### Function to count files by type in a directory
count_files_by_type <- function(dir) {
  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  file_types <- tools::file_ext(files)
  file_types <- ifelse(file_types == "", "No Extension", file_types)
  file_type_counts <- table(file_types)
  data.frame(File_Type = names(file_type_counts), Count = as.numeric(file_type_counts))
}

#### Get a list of all directories in the specified folder
dirs <- list.dirs(path_to_folder, full.names = TRUE, recursive = TRUE)

#### Loop through each directory and count the number of files by type
files_count_by_type <- lapply(dirs, count_files_by_type)

#### Create a data frame with the folder names and file counts by type
summary_df <- data.frame(
  Folder = rep(dirs, sapply(files_count_by_type, nrow)),
  do.call(rbind, files_count_by_type))%>%
  view()

### 11 Do some cleaning to delete old files folders
#### Define the pattern for the CSV file to delete
csv_pattern <- "final.csv$"  # This ensures it ends with 'final.csv'

#### Get a list of CSV files that match the pattern in the output folder
csv_files <- list.files(path_to_folder, pattern = csv_pattern, full.names = TRUE)

#### Delete the matched CSV file(s)
for (csv_file in csv_files) {
  if (file.exists(csv_file)) {
    file.remove(csv_file)
    cat("Deleted CSV file:", csv_file, "\n")
  } else {
    cat("CSV file does not exist:", csv_file, "\n")
  }
}

#### Delete the full_res and thumbnails folders
folders_to_delete <- c("full_res", "thumbnails")
for (folder in folders_to_delete) {
  folder_path <- file.path(path_to_folder, folder)
  if (dir.exists(folder_path)) {
    unlink(folder_path, recursive = TRUE, force = TRUE)
    cat("Deleted folder:", folder_path, "\n")
  } else {
    cat("Folder does not exist:", folder_path, "\n")
  }
}