rm(list = ls())
EXP_NAME <- "BART Prediction Intervals Simulations"
set.seed(331)
here::i_am(file.path("meals", "07b_bart_prediction_intervals.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
return_fit <- TRUE
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp_name <- dgp$name
print(dgp_name)

source(here::here(file.path("meals", "shared_experiments.R")))
experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(SAVE_DIR, "results", EXP_NAME, dgp$name)
) |>
  add_dgp(dgp) |> 
  add_method(bart_method) |>
  add_method(network_bart_method)
  
out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
