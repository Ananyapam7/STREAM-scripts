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
  
  # Extract data only from these sheets using the parse_excel function
  last_attempt_data <- map(last_attempt_sheets, ~parse_excel(filepath, .x))
  
  # Return the data as a named list where names are the last attempt sheet names
  return(setNames(last_attempt_data, last_attempt_sheets))
}