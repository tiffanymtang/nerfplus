# sh driver_school_conflict.sh

rm(list = ls())
library(optparse)

EXP_NAME <- "Philly Crime (Predictions)"
SAVE <- TRUE
USE_CACHED <- TRUE
TRAIN_PROP <- 0.75
CHECKPOINT_N_REPS <- 0
set.seed(331)

option_list <- list(
  make_option(
    "--subsample", type = "numeric", default = 0.01,
    help = "subsampling proportion"
  ),
  make_option(
    "--split_mode", type = "character", default = "random",
    help = "split mode: 'random', 'time', or 'location'"
  ),
  make_option(
    "--include_weather", action = "store_true", default = FALSE,
    help = "include weather covariates"
  )
)
# parse the command line options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
str(opt)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 100

# #### Cluster setup for parallelization (or comment out) ####
# # n_workers <- min(N_REPS, availableCores() - 1)
# n_workers <- 8
# plan(multisession, workers = n_workers)

#### DGPs ####
source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp_name <- sprintf(
  "Philly Crime (%s%s, %s)",
  opt$split_mode,
  ifelse(opt$include_weather, ", with weather", ""),
  opt$subsample
)
dgp <- create_dgp(
  load_philly_crime_data, .name = dgp_name, train_prop = TRAIN_PROP,
  subsample = opt$subsample, split_mode = opt$split_mode,
  include_weather = opt$include_weather, test_all = TRUE
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
export_visualizers(philly_crime_experiment)
file.remove(
  file.path(philly_crime_experiment$get_save_dir(), "experiment_cached_params.rds")
)
file.remove(
  file.path(philly_crime_experiment$get_save_dir(), "viz_results.rds")
)
