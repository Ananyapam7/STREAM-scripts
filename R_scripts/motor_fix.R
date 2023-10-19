# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Motor Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'zoo', 'dplyr', 'purrr', 'stringr', 'rjson', 'jsonlite')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

compute_derivatives <- function(x, t) {
  if (length(x) != length(t)) {
    stop("x and t must have the same length")
  }
  
  # Ensure t is in increasing order
  if (any(diff(t) <= 0)) {
    stop("t must be in strictly increasing order")
  }
  
  # Compute derivatives using finite differences
  speed <- c(0, diff(x) / diff(t))
  acceleration <- c(0, diff(speed) / diff(t)[-length(t)])
  jerk <- c(0, diff(acceleration) / diff(t)[-length(t)][-length(t)])
  
  # Pad 0 values to make vectors of the same length
  acceleration <- c(0, acceleration, 0)
  jerk <- c(0, jerk, 0, 0)
  
  list(speed = speed, acceleration = acceleration, jerk = jerk)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create(file.path(getwd(), "MotorTask/Datasets-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "MotorTask/Processed/"), showWarnings = FALSE, recursive = T)

topLevelFolder <- "/home/ananyapam/Projects/STREAM/data/START_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- rep(NA, (numberOfFolders))
rmse <- rep(NA, (numberOfFolders))
weighted_x_freq_gain <- rep(NA, (numberOfFolders))
weighted_y_freq_gain <- rep(NA, (numberOfFolders))
speed <- rep(NA, (numberOfFolders))
acceleration <- rep(NA, (numberOfFolders))
jerk <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()

# Function to parse the excel file
parse_excel <- function(filepath, sheetname) {
  # Read the excel file
  data <- read_excel(filepath, sheet = sheetname, col_names = FALSE)
  
  # Convert entire data to character
  data <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  
  # Identify the first row in the 3rd column with a value, if this logic changes, I am dead
  start_data_row <- which(data[,3] != "" & !is.na(data[,3]))[1]
  
  # Identify the last row before start_data_row with values in both the first two columns
  valid_metadata_rows <- which(data[1:(start_data_row-1),1] != "" & 
                                 data[1:(start_data_row-1),2] != "" & 
                                 !is.na(data[1:(start_data_row-1),1]) & 
                                 !is.na(data[1:(start_data_row-1),2]))
  
  last_metadata_row <- tail(valid_metadata_rows, 1)
  
  # Extract metadata keys and values
  metadata_keys <- data[1:last_metadata_row, 1]
  metadata_values <- data[1:last_metadata_row, 2]
  
  # Filter out entries where the key is NA or empty string
  valid_entries <- !(is.na(metadata_keys) | metadata_keys == "")
  metadata_keys <- metadata_keys[valid_entries]
  metadata_values <- metadata_values[valid_entries]
  
  # Create a named list
  metadata_list <- setNames(as.list(metadata_values), metadata_keys)
  
  # Extract the remaining data
  data_block <- data[(last_metadata_row+1):nrow(data),]
  data_block <- data_block[rowSums(is.na(data_block)) != ncol(data_block), ]
  ## Parsing the sub attempts now from the data block
  split_rows <- which(str_detect(data_block$...1, "Sub-attempt"))
  
  # If no "Sub-attempt" rows are found
  if(length(split_rows) == 0) {
    colnames(data_block) <- unlist(data_block[1, ])
    data_block <- data_block[-1, ]
    sub_attempt_data <- list(data_block)
  } else {
    # Add an extra split at the end of the data_block to ensure capturing the last block
    split_rows <- c(split_rows, nrow(data_block) + 1)
    # Extract dataframes into a list
    sub_attempt_data <- map(1:(length(split_rows) - 1), function(i) {
      attempt_data <- data_block[(split_rows[i] + 1):(split_rows[i+1] - 1), ]
      colnames(attempt_data) <- unlist(attempt_data[1, ])
      attempt_data <- attempt_data[-1, ]
      return(attempt_data)
    })
  }
  
  return(list(metadata = metadata_list, data_block = data_block, sub_attempt_data = sub_attempt_data))
}

parse_last_uninterrupted_sheet <- function(filepath){
  # Get all sheet names
  sheet_names <- excel_sheets(filepath)
  
  # Extract attempt numbers from the sheet names
  attempt_numbers <- str_extract(sheet_names, "(?<=Attempt #)\\d+") %>% as.numeric()
  
  # Order the sheet names by the extracted attempt numbers in descending order
  ordered_sheet_names <- sheet_names[order(attempt_numbers, decreasing = TRUE)]
  
  # Iterate through ordered sheets to find the sheet with 'interrupted=0'
  target_sheet <- NULL
  for(sheet in ordered_sheet_names) {
    metadata <- parse_excel(filepath, sheet)$metadata
    
    # Check if 'interrupted' key exists in the metadata and is equal to '0'
    if(!is.null(metadata$interrupted) && metadata$interrupted == "0") {
      target_sheet <- sheet
      break
    }
  }
  
  # If no such sheet exists, use the sheet with the highest attempt number (which will be the first in our ordered list)
  if(is.null(target_sheet)) {
    target_sheet <- ordered_sheet_names[1]
  }
  
  data <- parse_excel(filepath, target_sheet)
  
  return(setNames(list(data), target_sheet))
}

process_motor_task <- function(filepath){
  rmse <- NA
  weighted_x_freq_gain <- NA
  weighted_y_freq_gain <- NA
  speed <- NA
  acceleration <- NA
  jerk <- NA
  interrupted <- NA
  
  #last_attempt_data <- parse_last_attempt_sheet(filepath)[[1]]
  
  last_attempt_data <- parse_last_uninterrupted_sheet(filepath)[[1]]
  
  metadata <- last_attempt_data$metadata
  data <- last_attempt_data$sub_attempt_data
  sub_attempts <- length(data)
  
  interrupted <- metadata$interrupted
  
  rmse_subattempt <- rep(NA, sub_attempts)
  weighted_x_freq_gain_subattempt <- rep(NA, sub_attempts)
  weighted_y_freq_gain_subattempt <- rep(NA, sub_attempts)
  speed_subattempt <- rep(NA, sub_attempts)
  acceleration_subattempt <- rep(NA, sub_attempts)
  jerk_subattempt <- rep(NA, sub_attempts)
  
  for(attempt_index in 1:sub_attempts){
    attempt_data <- data[[attempt_index]]
    first_touch_index <- which(attempt_data$touch_pressure != 0)[1]
    attempt_data <- attempt_data[first_touch_index:nrow(attempt_data), ]
    
    # Average the data over unique timestamps
    averaged_data <- attempt_data %>%
      select(time, touch_x, touch_y, bee_x, bee_y, time) %>%
      mutate(across(everything(), as.numeric)) %>%
      group_by(time) %>%
      summarise(touch_x = mean(touch_x), touch_y = mean(touch_y), bee_x = mean(bee_x), bee_y = mean(bee_y))
    
    # Set the time from t=0
    averaged_data$time <- averaged_data$time - averaged_data$time[1]
    
    # Interpolate the time and set the start from t=0
    interpolated_time <- seq(from = (averaged_data$time)[1], to = (averaged_data$time)[length(averaged_data$time)], length.out = length(averaged_data$time))
    interpolated_time <- interpolated_time - interpolated_time[1]
    
    interpolated_touch_x <- c(scale(approx(averaged_data$time, averaged_data$touch_x, xout = interpolated_time)$y, scale = FALSE))
    interpolated_touch_y <- c(scale(approx(averaged_data$time, averaged_data$touch_y, xout = interpolated_time)$y, scale = FALSE))
    
    interpolated_bee_x <- c(scale(approx(averaged_data$time, averaged_data$bee_x, xout = interpolated_time)$y, scale = FALSE))
    interpolated_bee_y <- c(scale(approx(averaged_data$time, averaged_data$bee_y, xout = interpolated_time)$y, scale = FALSE))
    
    ## Do changes here
    write.csv(interpolated_bee_x, "inter_beex.csv")
    write.csv(interpolated_bee_y, "inter_beey.csv")
    write.csv(interpolated_touch_x, "inter_touchx.csv")
    write.csv(interpolated_touch_y, "inter_touchy.csv")
    write.csv(interpolated_time, "inter_time.csv")
  }
}

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
    xlFileNames <- list.files(path = thisFolder, pattern="motor.*\\.xlsx$", full.names=TRUE)
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
    if (length(xlFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      ########################## Data Extraction #################################
      process_motor_task(xlFileNames)
      ############################################################################
    }
    else {
      warning(paste0("No motor sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  }, warning = function(w) {
    message(paste0("Warning in folder ", num_folder, ": ", w))
  })
}

motor_task_data <- data.frame(
  child_ids,
  rmse,
  weighted_x_freq_gain,
  weighted_y_freq_gain,
  speed,
  acceleration,
  jerk,
  interrupted
)

head(motor_task_data)
write.csv(motor_task_data, "motor.csv", row.names = FALSE)