# Author: Ananyapam De 
# Date: September 2023 - Processing of STREAM Motor Task Data
rm(list=ls())

# Check which packages are installed, install the missing ones, and activate all
packagelist <- c('readxl', 'zoo', 'dplyr', 'purrr', 'stringr', 'pracma')
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
rmse <- rep(NA, (numberOfFolders))
weighted_x_freq_gain <- rep(NA, (numberOfFolders))
weighted_y_freq_gain <- rep(NA, (numberOfFolders))
speed <- rep(NA, (numberOfFolders))
acceleration <- rep(NA, (numberOfFolders))
jerk <- rep(NA, (numberOfFolders))
interrupted <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()

motor_expected_columns <- c("action", "butterfly_x", "butterfly_x_dp", "butterfly_y", "butterfly_y_dp", "device_x", "device_y", "device_z", "time", "touch_pressure", "touch_size", "touch_x", "touch_x_dp", "touch_y", "touch_y_dp", "image", "fileId")

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
      colnames(attempt_data) <- motor_expected_columns
      
      attempt_data <- attempt_data[-nrow(attempt_data), ]
      return(attempt_data)
    })
  }
  return(list(metadata = metadata_list, data_block = data_block, sub_attempt_data = sub_attempt_data))
}

gradients <- function(x, y, t) {
  gradx1 <- pracma::gradient(x, t)
  grady1 <- pracma::gradient(y, t)
  
  gradx2 <- pracma::gradient(gradx1, t)
  grady2 <- pracma::gradient(grady1, t)
  
  gradx3 <- pracma::gradient(gradx2, t)
  grady3 <- pracma::gradient(grady2, t)
  
  grad1 <- sqrt(gradx1^2 + grady1^2)
  grad2 <- sqrt(gradx2^2 + grady2^2)
  grad3 <- sqrt(gradx3^2 + grady3^2)
  
  list(grad1 = grad1, grad2 = grad2, grad3 = grad3)
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

process_motor_task <- function(filepath){
  rmse <- NA
  weighted_x_freq_gain <- NA
  weighted_y_freq_gain <- NA
  speed <- NA
  acceleration <- NA
  jerk <- NA
  interrupted <- NA
  
  last_attempt_data <- parse_last_attempt_sheet(filepath)[[1]]
  #print(last_attempt_data)
  metadata <- last_attempt_data$metadata
  data <- last_attempt_data$sub_attempt_data
  sub_attempts <- length(data)
  
  interrupted <- metadata$Interrupted
  
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
    
    if ((sum(attempt_data$touch_x != 0) < 5) || (sum(attempt_data$touch_y != 0) < 5)) {
      print("Very few touch points\n")
      rmse <- NA
      weighted_x_freq_gain <- NA
      weighted_y_freq_gain <- NA
      speed <- NA
      acceleration <- NA
      jerk <- NA
    } else{
      # Average the data over unique timestamps
      averaged_data <- attempt_data %>%
        select(time, touch_x, touch_y, butterfly_x, butterfly_y, time) %>%
        mutate(across(everything(), as.numeric)) %>%
        group_by(time) %>%
        summarise(touch_x = mean(touch_x), touch_y = mean(touch_y), butterfly_x = mean(butterfly_x), butterfly_y = mean(butterfly_y))
      
      # Set the time from t=0
      averaged_data$time <- averaged_data$time - averaged_data$time[1]
      
      # Interpolate the time and set the start from t=0
      interpolated_time <- seq(from = (averaged_data$time)[1], to = (averaged_data$time)[length(averaged_data$time)], length.out = length(averaged_data$time))
      interpolated_time <- interpolated_time - interpolated_time[1]
      
      interpolated_touch_x <- c(scale(approx(averaged_data$time, averaged_data$touch_x, xout = interpolated_time)$y, scale = FALSE))
      interpolated_touch_y <- c(scale(approx(averaged_data$time, averaged_data$touch_y, xout = interpolated_time)$y, scale = FALSE))
      
      interpolated_butterfly_x <- c(scale(approx(averaged_data$time, averaged_data$butterfly_x, xout = interpolated_time)$y, scale = FALSE))
      interpolated_butterfly_y <- c(scale(approx(averaged_data$time, averaged_data$butterfly_y, xout = interpolated_time)$y, scale = FALSE))
      
      fft_touch_x <- fft(interpolated_touch_x)
      fft_touch_y <- fft(interpolated_touch_y)
      fft_butterfly_x <- fft(interpolated_butterfly_x)
      fft_butterfly_y <- fft(interpolated_butterfly_y)
      
      # Consider only one half of the FFT spectrum
      num_samples <- length(interpolated_time)
      
      abs_fft_touch_x <- abs(fft_touch_x/num_samples)[1:ceiling(num_samples/2)]
      abs_fft_touch_y <- abs(fft_touch_y/num_samples)[1:ceiling(num_samples/2)]
      abs_fft_butterfly_x <- abs(fft_butterfly_x/num_samples)[1:ceiling(num_samples/2)]
      abs_fft_butterfly_y <- abs(fft_butterfly_y/num_samples)[1:ceiling(num_samples/2)]
      
      abs_fft_touch_x <- 2 * abs_fft_touch_x[2:(length(abs_fft_touch_x)-1)]
      abs_fft_touch_y <- 2 * abs_fft_touch_y[2:(length(abs_fft_touch_y)-1)]
      abs_fft_butterfly_x <- 2 * abs_fft_butterfly_x[2:(length(abs_fft_butterfly_x)-1)]
      abs_fft_butterfly_y <- 2 * abs_fft_butterfly_y[2:(length(abs_fft_butterfly_y)-1)]
      
      ########################################################################
      # Using rollapply to compute the moving average
      avg_amp_touch_x <- rollapply(abs_fft_touch_x, width = 3, FUN = mean, align = "center", partial = TRUE)
      avg_amp_touch_y <- rollapply(abs_fft_touch_y, width = 3, FUN = mean, align = "center", partial = TRUE)
      
      x_freq_gain_vector <- avg_amp_touch_x/abs_fft_butterfly_x
      y_freq_gain_vector <- avg_amp_touch_y/abs_fft_butterfly_y
      
      x_freq_gain_final <- mean(x_freq_gain_vector)
      y_freq_gain_final <- mean(y_freq_gain_vector)
      
      weighted_x_freq_gain <- sum(abs_fft_touch_x * x_freq_gain_vector) / sum(abs_fft_touch_x)
      weighted_y_freq_gain <- sum(abs_fft_touch_y * y_freq_gain_vector) / sum(abs_fft_touch_y)
      
      # Combine data into a single data frame
      combined_data <- averaged_data %>%
        filter(touch_x != 0) %>% 
        select(touch_x, touch_y, butterfly_x, butterfly_y)
      
      # Calculate the difference between the actual and estimated values
      difference <- c(combined_data$butterfly_x - combined_data$touch_x, 
                      combined_data$butterfly_y - combined_data$touch_y)
     
      print(difference)
       
      # Calculate RMSE
      rmse <- sqrt(mean(difference^2, na.rm = TRUE))
      grads <- gradients(averaged_data$touch_x, averaged_data$touch_y, averaged_data$time)
      speed <- mean(grads$grad1, na.rm = TRUE)
      acceleration <- mean(grads$grad2, na.rm = TRUE)
      jerk <- mean(grads$grad3, na.rm = TRUE)
    }
    rmse_subattempt[attempt_index] <- rmse
    weighted_x_freq_gain_subattempt[attempt_index] <- weighted_x_freq_gain
    weighted_y_freq_gain_subattempt[attempt_index] <- weighted_y_freq_gain
    speed_subattempt[attempt_index] <- speed
    acceleration_subattempt[attempt_index] <- acceleration
    jerk_subattempt[attempt_index] <- jerk
  }
  
  rmse <- mean(rmse_subattempt, na.rm = TRUE)
  weighted_x_freq_gain <- mean(weighted_x_freq_gain_subattempt, na.rm = TRUE)
  weighted_y_freq_gain <- mean(weighted_y_freq_gain_subattempt, na.rm = TRUE)
  speed <- mean(speed_subattempt, na.rm = TRUE)
  acceleration <- mean(acceleration_subattempt, na.rm = TRUE)
  jerk <- mean(jerk_subattempt, na.rm = TRUE)
  
  return(c(rmse,
           weighted_x_freq_gain,
           weighted_y_freq_gain,
           speed,
           acceleration,
           jerk,
           interrupted))
}

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    child_ids[num_folder] <- str_extract(thisFolder, "(?<=child_id_).+")
    xlFileNames <- list.files(path = thisFolder, pattern="motor.*\\.xlsx$", full.names=TRUE)
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
    if (length(xlFileNames) > 0) {
      print(sprintf('Processing file %s',num_folder))
      ########################## Data Extraction #################################
      motor_results <- process_motor_task(xlFileNames)
      rmse[num_folder] <- motor_results[1]
      weighted_x_freq_gain[num_folder] <- motor_results[2]
      weighted_y_freq_gain[num_folder] <- motor_results[3]
      speed[num_folder] <- motor_results[4]
      acceleration[num_folder] <- motor_results[5]
      jerk[num_folder] <- motor_results[6]
      interrupted[num_folder] <- motor_results[7]
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