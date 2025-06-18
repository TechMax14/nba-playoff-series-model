# utilities/paths.R

library(here)

# Base folders
data_dir <- here("Data")
code_dir <- here("Code")
output_dir <- here("Results")

# Data files
team_game_data_path <- file.path(data_dir, "team_game_data.csv")

# Outputs
temp_csv_path <- file.path(data_dir, "Built Datasets", "1temp.csv")





# /nba-postseason-pipeline
# │
# ├── /Code                 
# │   ├── /data_processing   
# │   │   ├── run_data_processing.R      # High-level pipeline execution for data processing
# │   │   ├── /training_logic            # Contains all training-related data processing
# │   │   │   ├── generate_team_metrics.R
# │   │   │   ├── generate_h2h_metrics.R
# │   │   │   ├── series_outcome_labeling.R
# │   │   │   ├── merge_and_clean_data.R
# │   │   │   └── generate_training_data.R
# │   │   ├── /predictor_logic           # Contains logic specific to predictor dataset preparation
# │   │   │   ├── predictors_utils.R
# │   │   │   └── generate_predictor_dataset.R
# │   │   ├── /metrics                   # Contains individual metric generation scripts
# │   │   │   ├── base_metrics.R
# │   │   │   ├── advanced_metrics.R
# │   │   │   ├── h2h_metrics.R
# │   │   │   └── playoff_metrics.R
# │   ├── /utilities                     # General utility functions
# │   │   ├── logging_setup.R            # Handles logging setup
# │   │   └── run_pipeline.R             # Encapsulates logic for executing the pipeline
# │   ├── /model_training                # Scripts related to model training
# │   │   └── train_model.R
# │   ├── /model_prediction              # Scripts related to model prediction
# │   │   └── run_model.R
# │   ├── /visuals                       # Visualization scripts for prediction results
# │   │   └── generate_visuals.R
# │   ├── /config                        # Configuration-related files
# │   │   ├── constants.R            # Holds constants like conference mappings
# │   │   └── paths.R                    # Centralized path variables
# │   └── main.R                         # Main file orchestrating the pipeline execution
# │
# ├── /Data                              # Folder for raw and processed data
# │   ├── team_data.csv                  # Raw team game data
# │   ├── actual_results_2023.csv        # Dataset containing actual results from the 2023 first round playoff matchups
# │   └── /Built Datasets                # Folder for any intermediate/temporary datasets
# │
# ├── /Logs                              # Folder for storing logs
# │   └── pipeline.log                   # Log file for pipeline execution
# │
# ├── /Visualizations                    # Folder for visualizations
# │   ├── 2023_playoff_predictions.png   # Model accuracy bar chart
# │   └── 2023_confidence_accuracy.png   # Confidence level accuracy
# │
# ├── /Results                           # Folder for storing output results
# │   ├── predictions.csv                # Predictions from the model
# │   └── model.Rds                      # Serialized model
# │
# └── README.md                          # Project documentation
