# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Bubble Task Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'purrr', 'stringr', 'dplyr', 'lubridate', 'jsonlite', 'rjson')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

# Create directory to save all processed files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create(file.path(getwd(), "BubbleTask/Datasets-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "BubbleTask/Processed/"), showWarnings = FALSE, recursive = T)

topLevelFolder <- "/home/ananyapam/Projects/STREAM/data/START_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- (numberOfFolders)
mean_pressure <- rep(NA, (numberOfFolders))
mean_disX <- rep(NA, (numberOfFolders))
mean_disY <- rep(NA, (numberOfFolders))
time_taken <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))
bubble_popped <- rep(NA, (numberOfFolders))
xlFiles_processed <- c()

compute_time_diff <- function(endTime, startTime) {
  
  # Splitting date-time and milliseconds
  end_time_parts <- strsplit(endTime, " ")[[1]]
  start_time_parts <- strsplit(startTime, " ")[[1]]
  
  # Parsing date-time
  end_datetime <- dmy_hms(paste(end_time_parts[1:2], collapse = " "))
  start_datetime <- dmy_hms(paste(start_time_parts[1:2], collapse = " "))
  
  # Computing the difference in milli-seconds
  time_diff_ms <- as.numeric(difftime(end_datetime, start_datetime, units = "secs"))*1000
  
  # Adjusting for milliseconds difference
  ms_diff <- as.numeric(end_time_parts[3]) - as.numeric(start_time_parts[3])
  total_diff_secs <- time_diff_ms + ms_diff
  
  return(total_diff_secs/1000)
}

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
    xlFileNames <- list.files(path = thisFolder, pattern="bubble.*\\.xlsx$", full.names=TRUE)
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
    if (length(xlFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      ########################## Data Extraction #############################
      # Assuming there's only one file with 'bubbles' in the name per num_folder
      sheet_names <- excel_sheets(xlFileNames[1])
      
      # Read data from the first sheet and extract device_info
      first_sheet_data <- read_excel(xlFileNames[1], sheet = sheet_names[1], col_names = FALSE)
      blank_rows_first <- which(is.na(first_sheet_data$...1) & is.na(first_sheet_data$...2)) # blank rows in the first sheet
      device_info_first <- first_sheet_data[(blank_rows_first[1] + 1):(blank_rows_first[2] - 1),]
      device_list_first <- setNames(as.list(device_info_first$...2), device_info_first$...1)
      
      last_sheet_name <- sheet_names[length(sheet_names)]

      data_list <- map(sheet_names, ~read_excel(xlFileNames[1], sheet = last_sheet_name, col_names = FALSE))
      
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
      bubble_popped[num_folder] <- device_list$bubblesPopped
      
      ########################################################################
      
      # Extract bubble task data
      if((device_list$bubblesPopped) > 0){
        headers <- data[(blank_rows[2] + 1),]
        main_data <- data[(blank_rows[2] + 2):nrow(data),]
        colnames(main_data) <- headers
        
        # Fill the NA values in bubble column
        
        filtered_data <- main_data[!is.na(main_data$bubble), ]
        
        # Assuming your dataset is named df
        filtered_data <- filtered_data %>%
          rowwise() %>%
          mutate(
            bubble_num = as.numeric(str_extract(bubble, "[0-9]+$")), # Extract the number from bubble column
            distance_x = abs(as.numeric(get(paste0("bubble_", bubble_num, "_x"))) - as.numeric(touch_x)), # Compute the difference for x
            distance_y = abs(as.numeric(get(paste0("bubble_", bubble_num, "_y"))) - as.numeric(touch_y))  # Compute the difference for y
          ) %>%
          select(-bubble_num) # Remove the temporary bubble_num column
        
        # Add the mean of the distances to the array
        mean_disX[num_folder] <- mean(filtered_data$distance_x, na.rm = TRUE)
        mean_disY[num_folder] <- mean(filtered_data$distance_y, na.rm = TRUE)
        
        # Add the mean of the pressure to the array
        mean_pressure[num_folder] <- mean(as.numeric(main_data$touch_pressure), na.rm = TRUE)
        
        # Add the time taken
        time_taken[num_folder] <- compute_time_diff(device_list$endTime, device_list_first$startTime)
        
        ########################################################################
        # Write the data as a JSON file
        json_data_list <- list(
          meta_info = meta_list,
          device_info = device_list,
          attempts = main_data
        )
        json_filename <- paste0("BubbleTask/Processed", "/child_", child_ids[num_folder], ".json")
        write_json(json_data_list, json_filename, auto_unbox = TRUE, pretty = TRUE)
      }
    }
    else {
      warning(paste0("No bubble sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  }, warning = function(w) {
    message(paste0("Warning in folder ", num_folder, ": ", w))
  })
}

file.copy(from = xlFiles_processed, to = file.path(getwd(), "BubbleTask/Datasets-processed/", basename(xlFiles_processed)))

bubble_task_data <- data.frame(
  child_ids,
  mean_pressure,
  mean_disX,
  mean_disY,
  time_taken,
  interrupted,
  bubble_popped
)

head(bubble_task_data)
write.csv(bubble_task_data, "bubble.csv", row.names = FALSE)