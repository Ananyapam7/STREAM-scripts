# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Coloring Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'dplyr', 'purrr', 'stringr', 'magick', 'sp', 'rjson', 'jsonlite', 'lubridate') # sudo apt-get install libmagick++-dev
missingpackages <- packagelist[!packagelist %in% installed.packages()[,1]]
if (length(missingpackages)>0){install.packages(missingpackages)}
toinstall <- packagelist[which(!packagelist %in% (.packages()))]
invisible(lapply(toinstall,library,character.only=T))
rm(list=ls())

# Create directory to save all processed files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create(file.path(getwd(), "ColoringTask/Datasets-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "ColoringTask/Images-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "ColoringTask/Processed/"), showWarnings = FALSE, recursive = T)

topLevelFolder <- "/home/ananyapam/Projects/STREAM-scripts/data/START_sample_data"

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

expected_columns <- c("color", "device_x", "device_y", "device_z", "time", 
                      "touch_pressure", "touch_size", "touch_x", "touch_x_dp", 
                      "touch_y", "touch_y_dp")

# Function to extract image and scaled image data
parse_image_data <- function(filepath, sheetname) {
  data <- read_excel(filepath, sheet = sheetname, col_names = TRUE)
  return(data)
}

parse_color_excel <- function(filepath) {
  # Get all the sheet names
  sheet_names <- excel_sheets(filepath)
  
  # Filter sheet names to consider only those named as "Attempt #1", "Attempt #2", etc.
  attempt_sheets <- grep("^Attempt #[0-9]+$", sheet_names, value = TRUE)
  
  # Initialize an empty list to store the parsed data
  parsed_data <- list()
  
  # Variable to keep track if we've found an 'interrupted=0' sheet
  interrupted_sheet_found <- FALSE
  
  # Iterate over the attempt sheets
  for(attempt_sheet in attempt_sheets) {
    attempt_data <- parse_excel(filepath, attempt_sheet)
    
    # If we haven't found an 'interrupted=0' sheet yet, and the current sheet has 'interrupted=0' in metadata
    if (!interrupted_sheet_found && "interrupted" %in% names(attempt_data$metadata) && 
        attempt_data$metadata$interrupted == "0") {
      
      parsed_data[[attempt_sheet]] <- list(attempt = attempt_data)
      # Determine the number of sub-attempts based on the sub_attempt_data list length
      num_sub_attempts <- length(attempt_data$sub_attempt_data)
      
      # For each sub-attempt, read the next 2 sheets: Image and Scaled Image
      for(j in 1:num_sub_attempts) {
        # Image data
        img_sheet_name <- paste0(attempt_sheet, " Image #" , j)
        if (img_sheet_name %in% sheet_names) {
          parsed_data[[attempt_sheet]][[img_sheet_name]] <- parse_image_data(filepath, img_sheet_name)
        }
        
        # Scaled Image data
        scaled_img_sheet_name <- paste0(attempt_sheet, " Scaled image #" , j)
        if (scaled_img_sheet_name %in% sheet_names) {
          parsed_data[[attempt_sheet]][[scaled_img_sheet_name]] <- parse_image_data(filepath, scaled_img_sheet_name)
        }
      }
      # Mark that we've found our sheet
      interrupted_sheet_found <- TRUE
      break
    }
  }
  
  # If no 'interrupted=0' sheet was found, process the last attempt sheet and its image sheets
  if (!interrupted_sheet_found) {
    last_attempt_sheet <- tail(attempt_sheets, 1)
    attempt_data <- parse_excel(filepath, last_attempt_sheet)
    parsed_data[[last_attempt_sheet]] <- list(attempt = attempt_data)
    
    num_sub_attempts <- length(attempt_data$sub_attempt_data)
    
    for(j in 1:num_sub_attempts) {
      img_sheet_name <- paste0(last_attempt_sheet, " Image #" , j)
      if (img_sheet_name %in% sheet_names) {
        parsed_data[[last_attempt_sheet]][[img_sheet_name]] <- parse_image_data(filepath, img_sheet_name)
      }
      
      scaled_img_sheet_name <- paste0(last_attempt_sheet, " Scaled image #" , j)
      if (scaled_img_sheet_name %in% sheet_names) {
        parsed_data[[last_attempt_sheet]][[scaled_img_sheet_name]] <- parse_image_data(filepath, scaled_img_sheet_name)
      }
    }
  }
  
  return(parsed_data)
}

# Function to extract all the data from the structured Excel file
parse_color_excel_old1 <- function(filepath) {
  # Get all the sheet names
  sheet_names <- excel_sheets(filepath)
  
  # Initialize an empty list to store the parsed data
  parsed_data <- list()
  
  # Iterate over the sheet names and parse data based on the naming pattern
  i <- 1
  while(i <= length(sheet_names)) {
    print(i)
    # Read the Attempt sheet
    attempt_data <- parse_excel(filepath, sheet_names[i])
    
    parsed_data[[sheet_names[i]]] <- list(attempt = attempt_data)
    # Determine the number of sub-attempts based on the sub_attempt_data list length
    num_sub_attempts <- length(attempt_data$sub_attempt_data)
    
    # For each sub-attempt, read the next 2 sheets: Image and Scaled Image
    for(j in 1:num_sub_attempts) {
      # Image data
      img_sheet_name <- paste0(sheet_names[i], " Image #" , j)
      parsed_data[[sheet_names[i]]][[img_sheet_name]] <- parse_image_data(filepath, img_sheet_name)
      
      # Scaled Image data
      scaled_img_sheet_name <- paste0(sheet_names[i], " Scaled image #" , j)
      parsed_data[[sheet_names[i]]][[scaled_img_sheet_name]] <- parse_image_data(filepath, scaled_img_sheet_name)
    }
    i <- i + 2 * num_sub_attempts
    
    # Move to the next Attempt sheet (or exit the loop if no more sheets)
    i <- i + 1
  }
  
  return(parsed_data)
}

# Function to parse the excel file
parse_excel_old1 <- function(filepath, sheetname) {
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

parse_excel <- function(filepath, sheetname) {
  range_to_read <- cell_cols(1:length(expected_columns))
  # Read the excel file
  data <- read_excel(filepath, sheet = sheetname, col_names = FALSE, range = range_to_read)

  # Convert entire data to character
  data <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  
  # Identify the first row in the 3rd column with a value
  start_data_row <- which(data[,3] != "" & !is.na(data[,3]))[1]
  
  if(is.na(start_data_row)){
    start_data_row <- which(str_detect(data$...1, "Sub-attempt")) + 1
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
  split_rows <- which(str_detect(data_block$...1, "Sub-attempt"))
  
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

find_last_uninterrupted_attempt <- function(data_list) {
  # Reverse iterate over the list
  for(i in length(data_list):1) {
    # Check if the interrupted attribute is 0
    if(data_list[[i]]$attempt$metadata$interrupted == 0) {
      return(data_list[[i]])
    }
  }
  return(NULL)  # Return NULL if no valid attempt is found
}

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

process_color_data_task <- function(filepath, imagefilepaths){
  points_inside <- NA
  points_outside <- NA
  crossover_counts <- NA
  crossover_durations <- NA
  pixel_data <- NA
  time_taken <- NA
  interrupted <- NA
  
  parsed_color_data_last_attempt <- parse_color_excel(filepath)[[1]]
  
  #parsed_color_data_last_attempt <- find_last_uninterrupted_attempt(parsed_color_data)
  
  #parsed_color_data_last_attempt <- parsed_color_data
  
  #parsed_color_data_last_attempt <- parsed_color_data[[length(parsed_color_data)]]
  
  color_data <- parsed_color_data_last_attempt[[1]]
  metadata <- color_data$metadata
  data <- color_data$sub_attempt_data
  num_sub_attempts <- length(data)
  interrupted <- metadata$interrupted
  time_taken <- compute_time_diff(metadata$endTime, metadata$startTime)
  
  inside_points_attempts <- numeric(num_sub_attempts)
  outside_points_attempts <- numeric(num_sub_attempts)
  crossover_counts_attempts <- numeric(num_sub_attempts)
  cross_durations_attempts <- numeric(num_sub_attempts)
  
  for(sub_attempt_index in 1:num_sub_attempts) {
    movements <- data[[sub_attempt_index]]
    coordinates <- parsed_color_data_last_attempt[[2*sub_attempt_index + 1]]
    x_coord <- as.numeric(coordinates$x)
    y_coord <- as.numeric(coordinates$y)
    x_movement <- round(as.numeric(movements$touch_x))
    y_movement <- round(as.numeric(movements$touch_y))
    
    # Create a spatial polygon from the coordinates
    sp_polygon <- SpatialPolygons(list(Polygons(list(Polygon(cbind(x_coord, y_coord))), ID = "1")))
    
    # Create a spatial points data frame from the movements
    sp_points <- SpatialPoints(cbind(x_movement, y_movement))
    
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
  
  points_inside <- mean(inside_points_attempts)
  points_outside <- mean(outside_points_attempts)
  crossover_counts <- mean(crossover_counts_attempts)
  crossover_durations <- mean(cross_durations_attempts)
  
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
  
  pixel_data <- mean(pixel_data_attempt)
  
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
    xlFileNames <- list.files(path = thisFolder, pattern="coloring.*\\.xlsx$", full.names=TRUE)
    child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
    imgFileNames <- list.files(path = thisFolder, pattern = "coloring.*\\.(jpg|png|gif|bmp|tiff)$", full.names = TRUE)
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
      warning(paste0("No coloring sheets found in ", thisFolder))
    }
  }, error = function(e) {
    message(paste0("Error in folder ", num_folder, ": ", e))
  }, warning = function(w) {
    message(paste0("Warning in folder ", num_folder, ": ", w))
  })
}

file.copy(from = xlFiles_processed, to = file.path(getwd(), "ColoringTask/Datasets-processed/", basename(xlFiles_processed)))
file.copy(from = imageFiles_processed, to = file.path(getwd(), "ColoringTask/Images-processed/", basename(imageFiles_processed)))

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