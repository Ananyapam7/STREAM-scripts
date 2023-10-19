# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Coloring Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'dplyr', 'purrr', 'stringr', 'magick', 'sp', 'rjson', 'jsonlite') # sudo apt-get install libmagick++-dev
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

topLevelFolder <- "/home/ananyapam/Projects/STREAM/data/START_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- (numberOfFolders)
time_taken <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))
points_inside <- rep(NA, (numberOfFolders))
points_outside <- rep(NA, (numberOfFolders))
crossover_counts <- rep(NA, (numberOfFolders))
crossover_durations <- rep(NA, (numberOfFolders))
pixel_data <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()
imageFiles_processed <- c()

for (num_folder in 1:numberOfFolders) {
  tryCatch({
  thisFolder <- listOfFolderNames[num_folder]
  xlFileNames <- list.files(path = thisFolder, pattern="coloring.*\\.xlsx$", full.names=TRUE)
  child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
  imgFileNames <- list.files(path = thisFolder, pattern = "coloring.*\\.(jpg|png|gif|bmp|tiff)$", full.names = TRUE)
  xlFiles_processed <- c(xlFiles_processed, xlFileNames)
  imageFiles_processed <- c(imageFiles_processed, imgFileNames)
  
  if (length(xlFileNames) > 0) {
    print(sprintf('Processing file %s',num_folder))
    ########################## Data Extraction #################################
    # Assuming there's only one file with 'color' in the name per num_folder
    sheet_names <- excel_sheets(xlFileNames[1])
    
    num_sheets <- length(sheet_names)
    
    data_list <- map(sheet_names, ~read_excel(xlFileNames[1], sheet = .x, col_names = FALSE))
    
    all_sub_attempts_data <- data_list[[1]]
    
    blank_rows <- which(is.na(all_sub_attempts_data$...1) & is.na(all_sub_attempts_data$...2)) # blank rows will act as separators
    
    # Extract meta-information
    meta_info <- all_sub_attempts_data[1:(blank_rows[1] - 1),]
    
    # Convert meta-info to a named list for easier access
    meta_list <- setNames(as.list(meta_info$...2), meta_info$...1)
    
    # Extract device details
    device_info <- all_sub_attempts_data[(blank_rows[1] + 1):(blank_rows[2] - 1),]
    
    # Convert device info to a named list
    device_list <- setNames(as.list(device_info$...2), device_info$...1)
    
    if(nrow(meta_info) == 0 || nrow(device_info) == 0) {
      warning(paste0("Empty meta_info or device_info in ", thisFolder))
      next # Skip to next iteration
    }
    
    # Add the time taken
    time_taken[num_folder]<- difftime(dmy_hms(sub(" (\\d{3})$", ".\\1", device_list$endTime)), dmy_hms(sub(" (\\d{3})$", ".\\1", device_list$startTime)), units = "secs")
    
    interrupted[num_folder] <- device_list$interrupted
    ############################################################################
    
    # Find the rows that start with "Sub-attempt #"
    split_rows <- which(str_detect(all_sub_attempts_data$...1, "Sub-attempt"))
    
    # If no "Sub-attempt" rows are found
    if(length(split_rows) == 0) {
      colnames(all_sub_attempts_data) <- unlist(all_sub_attempts_data[1, ])
      all_sub_attempts_data <- all_sub_attempts_data[-1, ]
      sub_attempt_data <- list(all_sub_attempts_data)
    } else {
      # Add an extra split at the end of the all_sub_attempts_data to ensure capturing the last block
      split_rows <- c(split_rows, nrow(all_sub_attempts_data) + 1)
      # Extract dataframes into a list
      sub_attempt_data <- map(1:(length(split_rows) - 1), function(i) {
        sub_data <- all_sub_attempts_data[(split_rows[i] + 2):(split_rows[i+1] - 2), ]
        colnames(sub_data) <- unlist(sub_data[1, ])
        sub_data <- sub_data[-1, ]
        return(sub_data)
      })
    }
    
    # Process the other sheets in pairs
    num_sub_attempts <- (length(data_list) - 1) / 2
    
    inside_points_attempts <- numeric(num_sub_attempts)
    outside_points_attempts <- numeric(num_sub_attempts)
    crossover_counts_attempts <- numeric(num_sub_attempts)
    cross_durations_attempts <- numeric(num_sub_attempts)
    
    attempts_data_list <- list()
    
    for(attempt_index in 1:num_sub_attempts) {
      sub_attempt_image <- data_list[[2*attempt_index]]
      sub_attempt_image_scaled <- data_list[[2*attempt_index + 1]]
      
      # Append this sub-attempt data to attempts_data_list
      attempts_data_list[[attempt_index]] <- list(
        sub_attempt_image = sub_attempt_image,
        sub_attempt_image_scaled = sub_attempt_image_scaled
      )
      
      # Fix the column names
      sub_attempt_image <- setNames(sub_attempt_image[-1, ], unlist(sub_attempt_image[1, ]))
      sub_attempt_image_scaled <- setNames(sub_attempt_image_scaled[-1, ], unlist(sub_attempt_image_scaled[1, ]))
      
      movements <- sub_attempt_data[[attempt_index]]
      coordinates <- sub_attempt_image_scaled
      
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
      inside_points_attempts[attempt_index] <- sum(!is.na(point_locations))
      outside_points_attempts[attempt_index] <- sum(is.na(point_locations))
      
      # Detect crossover points
      crossover_changes <- diff(as.numeric(!is.na(point_locations)))
      unique_crossovers <- which(crossover_changes != 0)
      crossover_counts_attempts[attempt_index] <- length(unique_crossovers)
      
      # Calculate the duration of crossovers
      if (length(unique_crossovers) > 1) {
        crossover_durations_attempts <- diff(as.numeric(movements[unique_crossovers,]$time))
      } else {
        crossover_durations_attempts <- 0
      }
      cross_durations_attempts[attempt_index] <- sum(crossover_durations_attempts)
    }
    
    points_inside[num_folder] <- mean(inside_points_attempts)
    points_outside[num_folder] <- mean(outside_points_attempts)
    crossover_counts[num_folder] <- mean(crossover_counts_attempts)
    crossover_durations[num_folder] <- mean(cross_durations_attempts)
    ############################################################################
    # Write the data as a JSON file
    json_data_list <- list(
      meta_info = meta_list,
      device_info = device_list,
      attempts = attempts_data_list # This will store all sub-attempt data
    )
    json_filename <- paste0("ColoringTask/Processed", "/child_", child_ids[num_folder], ".json")
    write_json(json_data_list, json_filename, auto_unbox = TRUE, pretty = TRUE)
    ############################################################################
  }
  else {
    warning(paste0("No coloring sheets found in ", thisFolder))
  }
  if (length(imgFileNames) > 0){
    print(sprintf('Processing file %s',num_folder))
    pixel_data_attempt <- c()
    
    for (img_file in imgFileNames){
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
      #print(pixel_counts)
      
      # Dynamically determine black and white pixel values
      black_pixel_value <- as.character(min(as.numeric(names(pixel_counts))))
      white_pixel_value <- as.character(max(as.numeric(names(pixel_counts))))
      
      #print(black_pixel_value)
      #print(white_pixel_value)
      
      # Calculate proportion of colored pixels
      all_pixels <- sum(pixel_counts)
      
      #print(all_pixels)
      #print(pixel_counts[black_pixel_value])
      #print(pixel_counts[black_pixel_value]/all_pixels)
      prop_col <- as.numeric(pixel_counts[black_pixel_value]) / all_pixels
      pixel_data_attempt <- c(pixel_data_attempt, prop_col)
    }
    pixel_data[num_folder] <- mean(pixel_data_attempt)
    ## Just an assertion to make the data processing better
    if(length(pixel_data_attempt) != num_sub_attempts){
      warning(paste0("Unexpected behaviour for attempts", thisFolder))
    }
  }
  else {
    warning(paste0("No color images found in ", thisFolder))
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
  time_taken, 
  interrupted, 
  points_inside, 
  points_outside,
  crossover_counts,
  crossover_durations, 
  pixel_data
)

head(color_task_data)
write.csv(color_task_data, "color.csv", row.names = FALSE)