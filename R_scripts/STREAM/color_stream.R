# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Coloring Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'dplyr', 'purrr', 'stringr', 'magick', 'sp', 'lubridate') # sudo apt-get install libmagick++-dev
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

# Create directory to save all processed files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

car_coordinates <- read.csv("/home/ananyapam/Projects/STREAM-scripts/R_scripts/car_outline.csv")

topLevelFolder <- "/home/ananyapam/Projects/STREAM-scripts/data/"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- rep(NA, (numberOfFolders))
points_inside <- rep(NA, (numberOfFolders))
points_outside <- rep(NA, (numberOfFolders))
crossover_counts <- rep(NA, (numberOfFolders))
crossover_durations <- rep(NA, (numberOfFolders))
pixel_data <- rep(NA, (numberOfFolders))
time_taken <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()
imageFiles_processed <- c()

expected_columns <- c("color", "device_x", "device_y", "device_z", "image", "fileId","time", 
                      "touch_pressure", "touch_size", "touch_x", "touch_x_dp", 
                      "touch_y", "touch_y_dp")

parse_excel <- function(filepath, sheetname) {
  range_to_read <- cell_cols(1:length(expected_columns))
  # Read the excel file
  data <- read_excel(filepath, sheet = sheetname, col_names = FALSE, range = range_to_read)

  # Convert entire data to character
  data <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  
  # Identify the first row in the 3rd column with a value
  start_data_row <- which(data[,3] != "" & !is.na(data[,3]))[1]
  
  if(is.na(start_data_row)){
    start_data_row <- which(str_detect(data$...1, "image")) + 1
  }
  
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
  
  # Parsing the sub attempts now from the data block
  split_rows <- which(str_detect(data_block$...1, "image"))
  
  # Dealing with pathological bad data cases
  if(length(split_rows) == 1 && nrow(data_block) == 1){
    data_block <- setNames(data.frame(matrix(ncol = length(expected_columns), nrow = 1)), expected_columns)
    sub_attempt_data <- list(data_block)
    return(list(metadata = metadata_list, data_block = data_block, sub_attempt_data = sub_attempt_data))
  }
  
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

parse_last_attempt_sheet <- function(filepath) {
  # Get all sheet names
  sheet_names <- excel_sheets(filepath)
  
  # Extract attempt numbers from the sheet names
  attempt_numbers <- str_extract(sheet_names, "(?<=Attempt #)\\d+") %>% as.numeric()
  
  # Identify the maximum attempt number
  max_attempt <- max(attempt_numbers, na.rm = TRUE)
  
  # Filter sheet names that match the maximum attempt number
  last_attempt_sheets <- sheet_names[attempt_numbers == max_attempt]
  
  range_to_read <- cell_cols(1:length(expected_columns))
  
  # Read the excel file
  last_attempt_data <- read_excel(filepath, sheet = last_attempt_sheets, col_names = FALSE, range = range_to_read)
  
  # Convert entire data to character
  data <- as.data.frame(lapply(last_attempt_data, as.character), stringsAsFactors = FALSE)
  
  # Identify the first row in the 3rd column with a value
  start_data_row <- which(data[,3] != "" & !is.na(data[,3]))[1]
  
  if(is.na(start_data_row)){
    start_data_row <- which(str_detect(data$...1, "image")) + 1
  }
  
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
  
  # Parsing the sub attempts now from the data block
  split_rows <- which(str_detect(data_block$...1, "image"))
  
  # Dealing with pathological bad data cases
  if(length(split_rows) == 1 && nrow(data_block) == 1){
    data_block <- setNames(data.frame(matrix(ncol = length(expected_columns), nrow = 1)), expected_columns)
    sub_attempt_data <- list(data_block)
    return(list(metadata = metadata_list, data_block = data_block, sub_attempt_data = sub_attempt_data))
  }
  
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

compute_time_diff <- function(endTime, startTime) {
  # Provided times
  start_time <- as.POSIXct(endTime, format="%d/%m/%Y %H:%M:%OS")
  end_time <- as.POSIXct(startTime, format="%d/%m/%Y %H:%M:%OS")
  
  # Calculate time difference
  time_difference <- difftime(end_time, start_time, units="secs")
  
  # Print result
  return (time_difference)
}

process_color_data_task <- function(filepath, imagefilepaths){
  points_inside <- NA
  points_outside <- NA
  crossover_counts <- NA
  crossover_durations <- NA
  pixel_data <- NA
  time_taken <- NA
  interrupted <- NA
  
  color_data <- parse_last_attempt_sheet(filepath)
  metadata <- color_data$metadata
  data <- color_data$sub_attempt_data
  num_sub_attempts <- length(data)
  interrupted <- metadata$Interrupted
  
  time_taken <- compute_time_diff(metadata$`Start time`, metadata$`End time`)
  
  inside_points_attempts <- numeric(num_sub_attempts)
  outside_points_attempts <- numeric(num_sub_attempts)
  crossover_counts_attempts <- numeric(num_sub_attempts)
  cross_durations_attempts <- numeric(num_sub_attempts)
  
  ## Need to change this part
  for(sub_attempt_index in 1:num_sub_attempts) {
    #print(sub_attempt_index)
    movements <- data[[sub_attempt_index]]
    
    if(nrow(movements)==0){
      inside_points_attempts[sub_attempt_index] <- NA
      outside_points_attempts[sub_attempt_index] <- NA
      crossover_counts_attempts[sub_attempt_index] <-NA
      cross_durations_attempts[sub_attempt_index] <- NA
      next
    }
    coordinates <- data.frame(x = car_coordinates$x, y=car_coordinates$y)
    
    x_coord <- as.numeric(coordinates$x)
    y_coord <- as.numeric(coordinates$y)
    
    x_coord <- x_coord - mean(x_coord)
    y_coord <- y_coord - mean(y_coord)
    
    x_movement <- as.numeric(movements$touch_x)
    y_movement <- as.numeric(movements$touch_y)
    
    x_movement <- x_movement - mean(x_movement)
    y_movement <- y_movement - mean(y_movement)
    
    # Create a spatial polygon from the coordinates
    sp_polygon <- SpatialPolygons(list(Polygons(list(Polygon(cbind(x_coord, y_coord))), ID = "1")))
    #plot(sp_polygon)
    # Create a spatial points data frame from the movements
    sp_points <- SpatialPoints(cbind(x_movement, y_movement))
    #plot(sp_points, col="red", pch=19)
    # Check if the points are inside the polygon
    point_locations <- over(sp_points, sp_polygon)
    
    # Store the number of points inside and outside of the polygon
    inside_points_attempts[sub_attempt_index] <- sum(!is.na(point_locations))
    outside_points_attempts[sub_attempt_index] <- sum(is.na(point_locations))
    
    # Detect crossover points
    crossover_changes <- diff(as.numeric(!is.na(point_locations)))
    unique_crossovers <- which(crossover_changes != 0)
    crossover_counts_attempts[sub_attempt_index] <- length(unique_crossovers)
    
    # Calculate the duration of crossovers
    if (length(unique_crossovers) > 1) {
      crossover_durations_attempts <- diff(as.numeric(movements[unique_crossovers,]$time))
    } else {
      crossover_durations_attempts <- 0
    }
    cross_durations_attempts[sub_attempt_index] <- sum(crossover_durations_attempts)
    
  }
  
  points_inside <- mean(inside_points_attempts, na.rm = TRUE)
  points_outside <- mean(outside_points_attempts, na.rm = TRUE)
  crossover_counts <- mean(crossover_counts_attempts, na.rm = TRUE)
  crossover_durations <- mean(cross_durations_attempts, na.rm = TRUE)
  
  pixel_data_attempt <- c()
  
  for (img_file in imagefilepaths){
    # Read the image with magick
    image_data <- image_read(img_file)
    
    # Convert to grayscale and then threshold
    image_data <- image_data %>% 
      image_convert(colorspace = "Gray") %>%
      image_threshold("black", "95%")
    
    # Get pixel values
    pixel_values <- as.integer(image_data)
    
    # Count pixels
    pixel_counts <- table(pixel_values)
    
    # Dynamically determine black and white pixel values
    black_pixel_value <- as.character(min(as.numeric(names(pixel_counts))))
    white_pixel_value <- as.character(max(as.numeric(names(pixel_counts))))
    
    # Calculate proportion of colored pixels
    all_pixels <- sum(pixel_counts)
    prop_col <- as.numeric(pixel_counts[black_pixel_value]) / all_pixels
    pixel_data_attempt <- c(pixel_data_attempt, prop_col)
  }
  
  pixel_data <- mean(pixel_data_attempt, na.rm=TRUE)
  
  return (c(points_inside,
            points_outside,
            crossover_counts,
            crossover_durations,
            pixel_data,
            time_taken,
            interrupted))
}

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    # New naming is colouring
    xlFileNames <- list.files(path = thisFolder, pattern="colouring.*\\.xlsx$", full.names=TRUE)
    child_ids[num_folder] <- str_extract(thisFolder, "(?<=child_id_).+")
    imgFileNames <- list.files(path = thisFolder, pattern = "colouring.*\\.(jpg|png|gif|bmp|tiff)$", full.names = TRUE)
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    imageFiles_processed <- c(imageFiles_processed, imgFileNames)
    if (length(xlFileNames) > 0 && length(imgFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      ########################## Data Extraction #################################
      color_results <- process_color_data_task(xlFileNames, imgFileNames)
      points_inside[num_folder] <- color_results[1]
      points_outside[num_folder] <- color_results[2]
      crossover_counts[num_folder] <- color_results[3]
      crossover_durations[num_folder] <- color_results[4]
      pixel_data[num_folder] <- color_results[5]
      time_taken[num_folder] <- color_results[6]
      interrupted[num_folder] <- color_results[7]
    }
    else {
      print(paste0("No coloring sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  })
}

color_task_data <- data.frame(
  child_ids, 
  points_inside, 
  points_outside,
  crossover_counts,
  crossover_durations, 
  pixel_data,
  time_taken, 
  interrupted
)

head(color_task_data)
write.csv(color_task_data, "color.csv", row.names = FALSE)