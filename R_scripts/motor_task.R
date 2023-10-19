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

linear_interpolation <- function(x, y, xout) {
  # Initialize an empty vector to store interpolated values
  yout <- numeric(length(xout))
  
  # For each xout value
  for (j in 1:length(xout)) {
    found <- FALSE
    for (i in 1:(length(x) - 1)) {
      if (xout[j] >= x[i] && xout[j] <= x[i + 1]) {
        # Apply the linear interpolation formula
        yout[j] <- y[i] + (xout[j] - x[i]) * (y[i + 1] - y[i]) / (x[i + 1] - x[i])
        found <- TRUE
        break
      }
    }
    # If xout[j] is out of bounds of the provided x values, we can't interpolate
    if (!found) {
      yout[j] <- NA  # NA denotes not available in R
    }
  }
  return(yout)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir.create(file.path(getwd(), "MotorTask/Datasets-processed/"), showWarnings = FALSE, recursive = T)
dir.create(file.path(getwd(), "MotorTask/Processed/"), showWarnings = FALSE, recursive = T)

topLevelFolder <- "/home/ananyapam/Projects/STREAM/data/START_sample_data"

listOfFolderNames <- list.files(path=topLevelFolder, full.names = TRUE)
numberOfFolders <- length(listOfFolderNames)

# Define arrays for results
child_ids <- (numberOfFolders)
interrupted <- rep(NA, (numberOfFolders))
rmse <- rep(NA, (numberOfFolders))
weighted_x_freq_gain <- rep(NA, (numberOfFolders))
weighted_y_freq_gain <- rep(NA, (numberOfFolders))
speed <- rep(NA, (numberOfFolders))
acceleration <- rep(NA, (numberOfFolders))
jerk <- rep(NA, (numberOfFolders))

xlFiles_processed <- c()

for (num_folder in 1:numberOfFolders) {
  tryCatch({
    thisFolder <- listOfFolderNames[num_folder]
    child_ids[num_folder] <- as.numeric(gsub("^.*child_(\\d+).*", "\\1", thisFolder))
    xlFileNames <- list.files(path = thisFolder, pattern="motor.*\\.xlsx$", full.names=TRUE)
    xlFiles_processed <- c(xlFiles_processed, xlFileNames)
    
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
      
      number_of_attempts <- length(sub_attempt_data)
      
      weighted_x_freq_gain_attempt <- numeric(number_of_attempts)
      weighted_y_freq_gain_attempt <- numeric(number_of_attempts)
      rmse_attempt <- numeric(number_of_attempts)
      speed_attempt <- numeric(number_of_attempts)
      acceleration_attempt <- numeric(number_of_attempts)
      jerk_attempt <- numeric(number_of_attempts)
      
      attempts_data_list <- list()
      
      for(attempt_index in seq(number_of_attempts)){
        sub_data <- sub_attempt_data[[attempt_index]]
        
        attempts_data_list[[attempt_index]] <- list(
          sub_attempt = sub_data
        )
        
        #if( length(intersect(as.numeric(sub_data$touch_x!=0), as.numeric(sub_data$touch_y!=0)))){
        #  cat("Very Few Touch Points\n")
        #  weighted_x_freq_gain_attempt[attempt_index] <- NA
        #  weighted_y_freq_gain_attempt[attempt_index] <- NA
        #  rmse_attempt[attempt_index] <- NA
        #  speed_attempt[attempt_index] <- NA
        #  acceleration_attempt[attempt_index] <- NA
        #  jerk_attempt[attempt_index] <- NA
        #  next
        #}
        
        # Consider only the last attempt
        if(attempt_index == number_of_attempts){
          # Ignore the impact of delay in user's response
          first_touch_index <- which(sub_data$touch_pressure != 0)[1]
          sub_data <- sub_data[first_touch_index:nrow(sub_data), ]
          
          # Average the data over unique timestamps
          averaged_data <- sub_data %>%
            select(time, touch_x, touch_y, bee_x, bee_y, time) %>%
            mutate(across(everything(), as.numeric)) %>%
            group_by(time) %>%
            summarise(touch_x = mean(touch_x), touch_y = mean(touch_y), bee_x = mean(bee_x), bee_y = mean(bee_y))
          
          # Set the time from t=0
          averaged_data$time <- averaged_data$time - averaged_data$time[1]
          
          # Interpolate the time and set the start from t=0
          interpolated_time <- seq(from = (averaged_data$time)[1], to = (averaged_data$time)[length(averaged_data$time)], length.out = length(averaged_data$time))
          interpolated_time <- interpolated_time - interpolated_time[1]
          
          interpolated_touch_x <- linear_interpolation(averaged_data$time, averaged_data$touch_x, interpolated_time)
          interpolated_touch_y <- linear_interpolation(averaged_data$time, averaged_data$touch_y, interpolated_time)
          interpolated_bee_x <- linear_interpolation(averaged_data$time, averaged_data$bee_x, interpolated_time)
          interpolated_bee_y <- linear_interpolation(averaged_data$time, averaged_data$bee_y, interpolated_time)
          
          #interpolated_touch_x <- c(scale(approx(averaged_data$time, averaged_data$touch_x, xout = interpolated_time)$y, scale = FALSE))
          #interpolated_touch_y <- c(scale(approx(averaged_data$time, averaged_data$touch_y, xout = interpolated_time)$y, scale = FALSE))
          #interpolated_bee_x <- c(scale(approx(averaged_data$time, averaged_data$bee_x, xout = interpolated_time)$y, scale = FALSE))
          #interpolated_bee_y <- c(scale(approx(averaged_data$time, averaged_data$bee_y, xout = interpolated_time)$y, scale = FALSE))
          
          fft_touch_x <- fft(interpolated_touch_x)
          fft_touch_y <- fft(interpolated_touch_y)
          fft_bee_x <- fft(interpolated_bee_x)
          fft_bee_y <- fft(interpolated_bee_y)
          
          # Consider only one half of the FFT spectrum
          num_samples <- length(interpolated_time)
          
          abs_fft_touch_x <- abs(fft_touch_x/num_samples)[1:ceiling(num_samples/2)]
          abs_fft_touch_y <- abs(fft_touch_y/num_samples)[1:ceiling(num_samples/2)]
          abs_fft_bee_x <- abs(fft_bee_x/num_samples)[1:ceiling(num_samples/2)]
          abs_fft_bee_y <- abs(fft_bee_y/num_samples)[1:ceiling(num_samples/2)]
          
          abs_fft_touch_x <- 2 * abs_fft_touch_x[2:(length(abs_fft_touch_x)-1)]
          abs_fft_touch_y <- 2 * abs_fft_touch_y[2:(length(abs_fft_touch_y)-1)]
          abs_fft_bee_x <- 2 * abs_fft_bee_x[2:(length(abs_fft_bee_x)-1)]
          abs_fft_bee_y <- 2 * abs_fft_bee_y[2:(length(abs_fft_bee_y)-1)]
          
          ########################################################################
          # Using rollapply to compute the moving average
          avg_amp_touch_x <- rollapply(abs_fft_touch_x, width = 3, FUN = mean, align = "center", partial = TRUE)
          avg_amp_touch_y <- rollapply(abs_fft_touch_y, width = 3, FUN = mean, align = "center", partial = TRUE)
          
          x_freq_gain_vector <- avg_amp_touch_x/abs_fft_bee_x
          y_freq_gain_vector <- avg_amp_touch_y/abs_fft_bee_y
          
          print(x_freq_gain_vector)
          print(y_freq_gain_vector)
          
          x_freq_gain_final <- mean(x_freq_gain_vector)
          y_freq_gain_final <- mean(y_freq_gain_vector)
          
          print(x_freq_gain_final)
          print(y_freq_gain_final)
          
          weighted_x_freq_gain <- sum(abs_fft_touch_x * x_freq_gain_vector) / sum(abs_fft_touch_x)
          weighted_y_freq_gain <- sum(abs_fft_touch_y * y_freq_gain_vector) / sum(abs_fft_touch_y)
          
          weighted_x_freq_gain_attempt[attempt_index] <- weighted_x_freq_gain
          weighted_y_freq_gain_attempt[attempt_index] <- weighted_y_freq_gain
          rmse_attempt[attempt_index] <- sqrt(mean((c(averaged_data$bee_x, averaged_data$bee_y) - c(averaged_data$touch_x, averaged_data$touch_y))^2))
          
          gradients_x <- compute_derivatives(averaged_data$touch_x, averaged_data$time)
          gradients_y <- compute_derivatives(averaged_data$touch_y, averaged_data$time)
          
          speed_attempt[attempt_index] <- mean(sqrt((gradients_x$speed)^2 + (gradients_y$speed)^2))
          acceleration_attempt[attempt_index] <- mean(sqrt((gradients_x$acceleration)^2 + (gradients_y$acceleration)^2))
          jerk_attempt[attempt_index] <- mean(sqrt((gradients_x$jerk)^2 + (gradients_y$jerk)^2))
        }
      }
      weighted_x_freq_gain[num_folder] <- mean(weighted_x_freq_gain_attempt, na.rm = TRUE)
      weighted_y_freq_gain[num_folder] <- mean(weighted_y_freq_gain_attempt, na.rm = TRUE)
      rmse[num_folder] <- mean(rmse_attempt, na.rm = TRUE)
      speed[num_folder] <- mean(speed_attempt, na.rm = TRUE)
      acceleration[num_folder] <- mean(acceleration_attempt, na.rm = TRUE)
      jerk[num_folder] <- mean(jerk_attempt, na.rm = TRUE)
      ############################################################################
      # Write the data as a JSON file
      json_data_list <- list(
        meta_info = meta_list,
        device_info = device_list,
        attempts = attempts_data_list # This will store all sub-attempt data
      )
      json_filename <- paste0("MotorTask/Processed", "/child_", child_ids[num_folder], ".json")
      write_json(json_data_list, json_filename, auto_unbox = TRUE, pretty = TRUE)
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

file.copy(from = xlFiles_processed, to = file.path(getwd(), "MotorTask/Datasets-processed/", basename(xlFiles_processed)))

motor_task_data <- data.frame(
  child_ids,
  rmse,
  weighted_x_freq_gain,
  weighted_y_freq_gain,
  speed,
  acceleration,
  interrupted,
  jerk
)

head(motor_task_data)
write.csv(motor_task_data, "motor.csv", row.names = FALSE)