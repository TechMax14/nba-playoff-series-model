# main.R

# Load necessary libraries
library(logging)
library(here)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(slider)
library(readr)
library(ggplot2)
library(scales)

# Load path definitions first
source(here("Code", "config", "paths.R"))

# Source helper functions
source(file.path(code_dir, "utilities", "logging_setup.R"))
source(file.path(code_dir, "utilities", "function_loader.R"))
source(file.path(code_dir, "utilities", "run_pipeline.R"))

# Main execution
setup_logging()
run_pipeline()
