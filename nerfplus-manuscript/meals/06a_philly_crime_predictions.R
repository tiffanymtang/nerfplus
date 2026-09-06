rm(list = ls())
EXP_NAME <- "Philly Crime (Predictions)"
set.seed(331)
here::i_am(file.path("meals", "06a_philly_crime_predictions.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####
source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp_name <- sprintf(
  "Philly Crime (%s%s%s, %s)",
  opt$split_mode,
  ifelse(opt$include_weather, ", with weather", ""),
  ifelse(opt$weighted_network, ", weighted network", ""),
  opt$subsample
)
dgp <- create_dgp(
  load_philly_crime_data, .name = dgp_name, train_prop = opt$train_prop,
  subsample = opt$subsample, split_mode = opt$split_mode,
  include_weather = opt$include_weather, weighted = opt$weighted_network, 
  test_all = TRUE
)
data_list <- dgp$generate()

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
nlams <- 10
lambdas_netcoh <- exp(seq(log(1000), log(0.001), length.out = nlams)) * nrow(data_list$x)^2
lambdas_embed <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_raw <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_stump <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
importance_modes <- NULL
source(here::here(file.path("meals", "shared_methods.R")))

# overwrite lm_method to save data once for each rep
lm_method$method_params$return_data <- "verbose_data_out"

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
source(here::here(file.path("meals", "shared_experiments.R")))
philly_crime_experiment <- philly_crime_experiment |>
  add_dgp(dgp) |>
  remove_evaluator(permute_fi_eval$name) |>
  remove_evaluator(mdiplus_fi_eval$name)
# out <- run_experiment(philly_crime_experiment)
out <- run_experiment(
  philly_crime_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(philly_crime_experiment)
# file.remove(
#   file.path(philly_crime_experiment$get_save_dir(), "experiment_cached_params.rds")
# )
