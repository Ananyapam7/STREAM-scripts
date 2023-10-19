# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Button Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'purrr', 'stringr', 'jsonlite', 'rjson')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

# Create directory to save all processed files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create(file.path(getwd(), "ButtonTask/Datasets-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "ButtonTask/Processed/"), showWarnings = FALSE, recursive = T)

topLevelFolder <- "/home/ananyapam/Projects/STREAM/data/START_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- (numberOfFolders)
social <- rep(NA, (numberOfFolders))
non_social <- rep(NA, (numberOfFolders))
green_click <- rep(NA, (numberOfFolders))
red_click <- rep(NA, (numberOfFolders))
trials <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    xlFileNames <- list.files(path = thisFolder, pattern="choose_touch.*\\.xlsx$", full.names=TRUE)
    child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
    if (length(xlFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      ########################## Data Extraction #############################
      # Assuming there's only one file with 'bubbles' in the name per num_folder
      sheet_names <- excel_sheets(xlFileNames[1])
      
      data_list <- map(sheet_names, ~read_excel(xlFileNames[1], sheet = .x, col_names = FALSE))
      
      data <- data_list[[1]]
      
      blank_rows <- which(is.na(data$...1) & is.na(data$...2)) # blank rows will act as separators
      
      # Extract meta-information
      meta_info <- data[1:(blank_rows[1] - 1),]
      
      # Convert meta-info to a named list for easier access
      meta_list <- setNames(as.list(meta_info$...2), meta_info$...1)
      
      # Extract device details
      device_info <- data[(blank_rows[1] + 1):(blank_rows[2] - 1),]
      
      # Convert device info to a named list
      device_list <- setNames(as.list(device_info$...2), device_info$...1)
      
      if(nrow(meta_info) == 0 || nrow(device_info) == 0) {
        warning(paste0("Empty meta_info or device_info in ", thisFolder))
        next # Skip to next iteration
      }
      
      interrupted[num_folder] <- device_list$interrupted
      
      ########################################################################
      
      green_click[num_folder] <- as.numeric(device_list$greenClickCount)
      red_click[num_folder] <- as.numeric(device_list$redClickCount)
      trials[num_folder] <- green_click[num_folder] + red_click[num_folder]
      
      if (trials[num_folder] >= 1) {
        # Extract the main data
        headers <- data[(blank_rows[2] + 1),]
        main_data <- data[(blank_rows[2] + 2):nrow(data),]
        colnames(main_data) <- headers
        
        # Filter rows with valid button presses
        valid_presses <- main_data[which(!is.na(main_data$button)),]
        # Need to extract the first valid press
        first_valid <- valid_presses[1,]
        button_pressed <- first_valid$button
        video_info <- first_valid$video_name
        
        if (button_pressed == "Red") {
          if (str_detect(video_info, "non")) {
            non_social[num_folder] <- red_click[num_folder]
            social[num_folder] <- green_click[num_folder]
          } else if (str_detect(video_info, "social")) {
            non_social[num_folder] <- green_click[num_folder]
            social[num_folder] <- red_click[num_folder]
          }
        } else if (button_pressed == "Green") {
          if (str_detect(video_info, "non")) {
            non_social[num_folder] <- green_click[num_folder]
            social[num_folder] <- red_click[num_folder]
          } else if (str_detect(video_info, "social")) {
            non_social[num_folder] <- red_click[num_folder]
            social[num_folder] <- green_click[num_folder]
          }
        } else {
          warning(paste0("Unexpected error in ", thisFolder))
        }
        ########################################################################
        # Write the data as a JSON file
        json_data_list <- list(
          meta_info = meta_list,
          device_info = device_list,
          attempts = main_data # This will store all sub-attempt data
        )
        json_filename <- paste0("ButtonTask/Processed", "/child_", child_ids[num_folder], ".json")
        write_json(json_data_list, json_filename, auto_unbox = TRUE, pretty = TRUE)
        ########################################################################
      }
      else{
        warning(paste0("No trials found in ", thisFolder))
      }
    }
    else{
      warning(paste0("No choose_touch sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  }, warning = function(w) {
    message(paste0("Warning in folder ", num_folder, ": ", w))
  })
}

file.copy(from = xlFiles_processed, to = file.path(getwd(), "ButtonTask/Datasets-processed/", basename(xlFiles_processed)))

button_task_data <- data.frame(
  child_ids,
  social,
  non_social,
  trials,
  soc_pref = social / trials,
  interrupted
)

head(button_task_data)
write.csv(button_task_data, "button.csv", row.names = FALSE)