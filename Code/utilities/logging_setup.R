# logging_setup.R

# Function to set up logging
setup_logging <- function() {
  # Ensure the 'logs' directory exists
  log_dir <- "Logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
  }
  
  # Set up the log file location and level
  log_file <- file.path(log_dir, "pipeline.log")
  basicConfig(level = "INFO")
  
  # Add a file handler to log to the specified file
  addHandler(writeToFile, file = log_file)
}