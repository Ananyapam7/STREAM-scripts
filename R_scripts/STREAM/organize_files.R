library(stringr)

# Function to extract child ID
extract_child_id <- function(file_name) {
  # Using regex to match the pattern child_id_[COUNTRY CODE]-[CHILD ID]-[COMPONENT]
  pattern <- "^child_id_([A-Z]{2}-\\d{4}-[A-Z]{2})"
  str_match(file_name, pattern)[,2]
}

topLevelFolder = "/home/ananyapam/Projects/STREAM-scripts/data/STREAM_sample_data"
media_files <- list.files(path=paste0(topLevelFolder, "/media"), full.names = TRUE)

# Add the prefix "child_id_" to each file in the 'media' directory
prefixed_files <- file.path(paste0(topLevelFolder, "/media"), paste0("child_id_", basename(media_files)))
file.rename(media_files, prefixed_files)
media_files <- list.files(path=paste0(topLevelFolder, "/media"), full.names = TRUE)

destination_files <- gsub("media/", "", media_files)
file.rename(media_files, destination_files)
unlink(paste0(topLevelFolder, "/media"), recursive = TRUE)

setwd(topLevelFolder)

files <- list.files(pattern = "child_id_.*", full.names = TRUE)

for (file in files) {
  file_name <- basename(file)
  child_id <- extract_child_id(file_name)
  child_id <- paste0("child_id_", child_id)
  if (is.na(child_id)) next
  
  if (!dir.exists(child_id)) {
    dir.create(child_id)
  }
  file.rename(file, file.path(child_id, file_name))
}

cat("Files have been organized into child ID directories.")