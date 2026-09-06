rm(list = ls())
EXP_NAME <- "School Conflict"
set.seed(331)
here::i_am(file.path("meals", "05a_school_conflict.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####
source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp <- create_dgp(
  .dgp_fun = load_school_conflict_data,
  .name = sprintf("School %s", opt$schid),
  keep_schools = opt$schid,
  network_type = opt$network_type,
  response_type = opt$response_type,
  include_w1 = opt$include_w1,
  impute_mode = opt$impute_mode,
  connected = opt$connected,
  train_prop = opt$train_prop
)
data_list <- dgp$generate()

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
nlams <- 20
lambdas_netcoh <- exp(seq(log(1000), log(0.001), length.out = nlams)) * nrow(data_list$x)^2
lambdas_embed <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_raw <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_stump <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
return_data <- TRUE
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
source(here::here(file.path("meals", "shared_experiments.R")))
school_conflict_experiment <- school_conflict_experiment |>
  add_dgp(dgp)
out <- run_experiment(
  school_conflict_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(school_conflict_experiment)
# file.remove(
#   file.path(school_conflict_experiment$get_save_dir(), "experiment_cached_params.rds")
# )
