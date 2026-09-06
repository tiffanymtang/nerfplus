rm(list = ls())
EXP_NAME <- "Main Simulations"
set.seed(331)
here::i_am(file.path("meals", "01b_logistic_simulations.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp_name <- dgp$name
print(dgp_name)

source(here::here(file.path("meals", "shared_experiments.R")))
experiment <- experiment |>
  add_dgp(dgp) |>
  remove_method(nerfplus_embedding_only_method$name) |>
  remove_method(nerfplus_cohesion_only_method$name) |>
  add_vary_across(
    .dgp = dgp$name,
    centroids_scale = c(0, 0.5, 1, 1.5)
  ) |>
  remove_visualizer()

out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
# export_visualizers(experiment)
# file.remove(
#   file.path(experiment$get_save_dir(), dgp$name, "Varying centroids_scale-pve", "experiment_cached_params.rds")
# )
