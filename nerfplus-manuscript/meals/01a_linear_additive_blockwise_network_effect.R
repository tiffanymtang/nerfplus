rm(list = ls())
EXP_NAME <- "Main Simulations"
SAVE <- TRUE
USE_CACHED <- TRUE
CHECKPOINT_N_REPS <- 0
set.seed(331)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 100

# #### Cluster setup for parallelization (or comment out) ####
# # n_workers <- min(N_REPS, availableCores() - 1)
# n_workers <- 9
# plan(multisession, workers = n_workers)

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp <- linear_additive_blockwise_network_dgp
dgp_name <- dgp$name
print(dgp_name)

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
source(here::here(file.path("meals", "shared_experiments.R")))
experiment <- experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    centroids_scale = c(0, 0.5, 1, 1.5),
    pve = c(0.2, 0.4, 0.6, 0.8)
  )
out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
export_visualizers(experiment)
file.remove(
  file.path(experiment$get_save_dir(), dgp$name, "Varying centroids_scale-pve", "experiment_cached_params.rds")
)
file.remove(
  file.path(experiment$get_save_dir(), dgp$name, "Varying centroids_scale-pve", "viz_results.rds")
)
