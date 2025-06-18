# function_loader.R

# Load centralized paths
source(here::here("Code", "config", "paths.R"))
source(file.path(code_dir, "config", "constants.R"))

# Load project functions for data processing
source(file.path(code_dir, "data_processing", "training_logic", "generate_team_metrics.R"))
source(file.path(code_dir, "data_processing", "training_logic", "generate_h2h_metrics.R"))
source(file.path(code_dir, "data_processing", "training_logic", "series_outcome_labeling.R"))
source(file.path(code_dir, "data_processing", "training_logic", "merge_and_clean_data.R"))
source(file.path(code_dir, "data_processing", "training_logic", "generate_training_data.R"))

# Load project functions for predictor logic
#source(file.path(code_dir, "data_processing", "predictor_logic", "predictor_utils.R"))
source(file.path(code_dir, "data_processing", "predictor_logic", "generate_predictor_dataset.R"))
source(file.path(code_dir, "data_processing", "predictor_logic", "build_predictor_dataset.R"))
source(file.path(code_dir, "data_processing", "predictor_logic", "prepare_predictor_dataset.R"))


# Load project functions for model training and prediction
source(file.path(code_dir, "model_training", "train_model.R"))
source(file.path(code_dir, "model_prediction", "run_model.R"))


# Load project functions for model result visualizations
source(file.path(code_dir, "visuals", "generate_visuals.R"))