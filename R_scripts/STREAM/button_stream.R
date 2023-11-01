# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Button Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'purrr', 'stringr')
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

topLevelFolder <- "/home/ananyapam/Projects/STREAM-scripts/data/STREAM_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- rep(NA, (numberOfFolders))
social <- rep(NA, (numberOfFolders))
non_social <- rep(NA, (numberOfFolders))
trials <- rep(NA, (numberOfFolders))
soc_pref <- rep(NA, (numberOfFolders))
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
      sub_data <- data_block[(split_rows[i] + 1):(split_rows[i+1] - 1), ]
      colnames(sub_data) <- unlist(sub_data[1, ])
      sub_data <- sub_data[-1, ]
      return(sub_data)
    })
  }
  
  return(list(metadata = metadata_list, data_block = data_block, sub_attempt_data = sub_attempt_data))
}

# Function to extract data from sheets representing the last attempt
parse_last_attempt_sheet <- function(filepath) {
  # Get all sheet names
  sheet_names <- excel_sheets(filepath)
  
  # Extract attempt numbers from the sheet names
  attempt_numbers <- str_extract(sheet_names, "(?<=Attempt #)\\d+") %>% as.numeric()
  
  # Identify the maximum attempt number
  max_attempt <- max(attempt_numbers, na.rm = TRUE)
  
  # Filter sheet names that match the maximum attempt number
  last_attempt_sheets <- sheet_names[attempt_numbers == max_attempt]
  
  # Extract data only from these sheets using the parse_excel_bubble function
  last_attempt_data <- map(last_attempt_sheets, ~parse_excel(filepath, .x))
  
  # Return the data as a named list where names are the last attempt sheet names
  return(setNames(last_attempt_data, last_attempt_sheets))
}

process_button_task <- function(filepath){
  social <- NA
  non_social <- NA
  trials <- NA
  soc_pref <- NA
  interrupted <- NA

  last_attempt_data <- parse_last_attempt_sheet(filepath)[[1]]
  
  metadata <- last_attempt_data$metadata
  data <- last_attempt_data$sub_attempt_data
  sub_attempts <- length(data)
  
  green_clicks <- as.numeric(metadata$`Green click count`)
  red_clicks <- as.numeric(metadata$`Red click count`)
  trials <- green_clicks + red_clicks
  interrupted <- metadata$Interrupted

  if(trials >= 1){
    # Extract the last sub-attempt
    attempt_data <- data[[sub_attempts]]
    
    # Filter rows with valid button presses
    valid_presses <- attempt_data[which(!is.na(attempt_data$button)),]
    # Need to extract the first valid press
    first_valid <- valid_presses[1,]
    button_pressed <- first_valid$button
    video_info <- first_valid$video_name
    
    if (button_pressed == "red") {
      if (str_detect(video_info, "non")) {
        non_social <- red_clicks
        social <- green_clicks
      } else if (str_detect(video_info, "social")) {
        non_social <- green_clicks
        social <- red_clicks
      }
    } else if (button_pressed == "green") {
      if (str_detect(video_info, "non")) {
        non_social <- green_clicks
        social <- red_clicks
      } else if (str_detect(video_info, "social")) {
        non_social <- red_clicks
        social <- green_clicks
      }
    }
    soc_pref <- social/trials
  }
  return(c(social,
           non_social,
           trials,
           soc_pref,
           interrupted))
}

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    xlFileNames <- list.files(path = thisFolder, pattern="button.*\\.xlsx$", full.names=TRUE)
    child_ids[num_folder] <- str_extract(thisFolder, "(?<=child_id_).+")
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
    if (length(xlFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      button_results <- process_button_task(xlFileNames)
      social[num_folder] <- button_results[1]
      non_social[num_folder] <- button_results[2]
      trials[num_folder] <- button_results[3]
      soc_pref[num_folder] <- button_results[4]
      interrupted[num_folder] <- button_results[5]
    }
    else{
      warning(paste0("No button sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  }, warning = function(w) {
    message(paste0("Warning in folder ", num_folder, ": ", w))
  })
}

button_task_data <- data.frame(
  child_ids,
  social,
  non_social,
  trials,
  soc_pref,
  interrupted
)

head(button_task_data)
write.csv(button_task_data, "button.csv", row.names = FALSE)