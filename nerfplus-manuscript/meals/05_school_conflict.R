# sh driver_school_conflict.sh

rm(list = ls())
library(optparse)

EXP_NAME <- "School Conflict (new)"
SAVE <- TRUE
USE_CACHED <- TRUE
TRAIN_PROP <- 0.75
CHECKPOINT_N_REPS <- 0
set.seed(331)

option_list <- list(
  make_option(
    "--schid", type = "character", default = "1",
    help = "school ID"
  ),
  make_option(
    "--network_type", type = "character", default = "A",
    help = "type of network (one of 'A', 'A1', 'A2', 'B', 'B1', 'DA', or 'DA1')"
  ),
  make_option(
    "--response_type", type = "character", default = "PNW2",
    help = "type of response variable (one of 'PNW2' or 'PNdiff')"
  ),
  make_option(
    "--include_w1", action = "store_true", default = FALSE,
    help = "include W1 in the response variable (default: FALSE)"
  ),
  make_option(
    "--impute_mode", type = "character", default = "none",
    help = "imputation mode (one of 'none', 'mean', 'median', or 'rf')"
  ),
  make_option(
    "--connected", action = "store_true", default = FALSE,
    help = "use largest connected component (default: FALSE)"
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

dgp_name <- sprintf("School %s", opt$schid)
dgp <- create_dgp(
  .dgp_fun = load_school_conflict_data,
  .name = dgp_name,
  keep_schools = opt$schid,
  network_type = opt$network_type,
  response_type = opt$response_type,
  include_w1 = opt$include_w1,
  impute_mode = opt$impute_mode,
  connected = opt$connected,
  train_prop = TRAIN_PROP
)
data_list <- dgp$generate()

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
embedding_options <- list(
  ndim = 4,
  regularization = 0.5,
  varimax = FALSE,
  center = TRUE,
  scale = TRUE
)
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
  add_dgp(dgp) |>
  set_save_dir(file.path(SAVE_DIR, "results", EXP_NAME, dgp_name))
out <- run_experiment(
  school_conflict_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
export_visualizers(school_conflict_experiment)
file.remove(
  file.path(school_conflict_experiment$get_save_dir(), "experiment_cached_params.rds")
)
file.remove(
  file.path(school_conflict_experiment$get_save_dir(), "viz_results.rds")
)
