rm(list = ls())
EXP_NAME <- "Main Simulations"
here::i_am(file.path("meals", "04_outlier_simulations.R"))
set.seed(331)
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
loo <- TRUE
importance_modes <- NULL
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp_name <- dgp$name
print(dgp_name)

source(here::here(file.path("meals", "shared_experiments.R")))
outlier_experiment <- outlier_experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    outliers_scale = c(1, 2, 3, 4)
  )
out <- run_experiment(
  outlier_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(outlier_experiment)
# file.remove(
#   file.path(outlier_experiment$get_save_dir(), dgp$name, "Varying outliers_scale", "experiment_cached_params.rds")
# )
