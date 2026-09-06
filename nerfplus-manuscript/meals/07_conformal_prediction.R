rm(list = ls())
EXP_NAME <- "Conformal Simulations"
set.seed(331)
here::i_am(file.path("meals", "07_conformal_prediction.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
num_train_samples <- 300
num_test_samples <- 10000
num_samples <- num_train_samples * 2 + num_test_samples
train_prop <- num_train_samples * 2 / num_samples
source(here::here(file.path("meals", "shared_dgps.R")))

dgp <- switch(
  opt$dgp,
  linear_additive_block = linear_additive_blockwise_network_dgp,
  linear_autocorrelation = linear_network_autocorrelation_dgp,
  poly_additive_block = poly_additive_blockwise_network_dgp,
  poly_autocorrelation = poly_network_autocorrelation_dgp,
  lss_additive_block = lss_additive_blockwise_network_dgp,
  lss_autocorrelation = lss_network_autocorrelation_dgp,
  stop("Invalid dgp option")
)

#### Methods ####

num_samples <- num_train_samples
source(here::here(file.path("meals", "shared_method_params_default.R")))
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp_name <- dgp$name
print(dgp_name)

source(here::here(file.path("meals", "shared_experiments.R")))
conformal_experiment <- conformal_experiment |>
  add_dgp(dgp)
out <- run_experiment(
  conformal_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(conformal_experiment)
# file.remove(
#   file.path(conformal_experiment$get_save_dir(), dgp$name, "Varying centroids_scale-pve", "experiment_cached_params.rds")
# )
