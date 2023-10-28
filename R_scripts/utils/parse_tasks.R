library(readxl)
library(stringr)
library(purrr)

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

parse_all_sheets <- function(filepath) {
  # Get the names of all sheets in the Excel file
  sheet_names <- excel_sheets(filepath)
  
  # Loop through sheet names and parse data from each sheet using the parse_excel function
  sheet_data_list <- map(sheet_names, ~parse_excel(filepath, .x))
  
  # Return the data as a named list where names are sheet names
  return(setNames(sheet_data_list, sheet_names))
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

# This works for button, bubble, motor task
# Not for coloring task as it's excel sheets have a different format

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

process_bubble_task <- function(filepath){
  mean_pressure <- NA
  mean_disX <- NA
  mean_disY <- NA
  time_taken <- NA
  interrupted <- NA
  bubble_popped <- NA
  
  last_attempt_data <- parse_last_attempt_sheet(filepath)[[1]]
  
  metadata <- last_attempt_data$metadata
  data <- last_attempt_data$sub_attempt_data
  sub_attempts <- length(data)
  
  interrupted <- metadata$interrupted
  bubblesPopped <- metadata$bubblesPopped
  
  if(bubblesPopped > 0){
    # Extract the last sub-attempt
    attempt_data <- data[[sub_attempts]]
    
    filtered_data <- attempt_data[!is.na(attempt_data$bubble), ]
    
    filtered_data <- filtered_data %>%
      rowwise() %>%
      mutate(
        bubble_num = as.numeric(str_extract(bubble, "[0-9]+$")), # Extract the number from bubble column
        distance_x = abs(as.numeric(get(paste0("bubble_", bubble_num, "_x"))) - as.numeric(touch_x)), # Compute the difference for x
        distance_y = abs(as.numeric(get(paste0("bubble_", bubble_num, "_y"))) - as.numeric(touch_y))  # Compute the difference for y
      ) %>%
      select(-bubble_num) # Remove the temporary bubble_num column
    
    mean_disX <- mean(filtered_data$distance_x, na.rm = TRUE)
    mean_disY <- mean(filtered_data$distance_y, na.rm = TRUE)
    
    # Add the mean of the pressure to the array
    mean_pressure <- mean(as.numeric(attempt_data$touch_pressure), na.rm = TRUE)
    
    # Add the time taken
    time_taken <- compute_time_diff(metadata$endTime, metadata$startTime)
  }
}

result <- process_bubble_task("/home/ananyapam/Projects/STREAM/data/START_sample_data/child_729_survey_1205_results_1542330763/child_id_survey_1205_bubbles_jubbing_results_1542330757.xlsx")