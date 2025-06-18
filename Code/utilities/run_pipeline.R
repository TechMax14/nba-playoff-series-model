# run_pipeline.R

# Function to run the pipeline and log messages
run_pipeline <- function() {
  loginfo("Starting the NBA Postseason Pipeline...")
  
  tryCatch({
    # Execute the pipeline
    source(here::here("code", "data_processing", "run_data_processing.R"))
    
    # Log successful completion
    loginfo("Pipeline finished successfully!")
  }, error = function(e) {
    # Log error
    logerror("Error occurred in pipeline: %s", e$message)
  })
}