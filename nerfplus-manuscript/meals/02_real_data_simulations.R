rm(list = ls())
EXP_NAME <- "Main Simulations"
set.seed(331)
here::i_am(file.path("meals", "02_real_data_simulations.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

num_samples <- 370  # Number of samples
num_features <- 214  # Number of features

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
source(here::here(file.path("meals", "shared_methods.R")))

# don't compute feature importance for non-NeRF+ methods
lm_method$method_params$importance_modes <- NULL
rf_method$method_params$importance_modes <- NULL
rfplus_method$method_params$importance_modes <- NULL
nerfplus_embedding_only_method$method_params$importance_modes <- NULL
nerfplus_cohesion_only_method$method_params$importance_modes <- NULL
rnc_method$method_params$importance_modes <- NULL

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp_name <- dgp$name
print(dgp_name)

source(here::here(file.path("meals", "shared_experiments.R")))
if (stringr::str_detect(dgp$name, "Autocorrelation")) {
  experiment <- experiment |>
    add_dgp(dgp) |>
    add_vary_across(
      .dgp = dgp$name,
      omega = c(0.1, 0.3, 0.5, 0.7, 0.9),
      pve = c(0.2, 0.4, 0.6, 0.8)
    )
} else {
  experiment <- experiment |>
    add_dgp(dgp) |>
    add_vary_across(
      .dgp = dgp$name,
      centroids_scale = c(0, 0.5, 1, 1.5),
      pve = c(0.2, 0.4, 0.6, 0.8)
    )
}
out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(experiment)
# file.remove(
#   file.path(experiment$get_save_dir(), dgp$name, "Varying centroids_scale-pve", "experiment_cached_params.rds")
# )
