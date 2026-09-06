library(simChef)
library(future)
library(optparse)

options(simChef.plot_theme = "vthemes")
simChef::load_all()

cat(sprintf("Experiment Name: %s\n", EXP_NAME))

# command line arguments
option_list <- list(
  make_option(
    "--dgp", type = "character", default = "linear_additive_block",
    help = "Data generating process to use for the experiment [default %default]"
  ),
  make_option(
    "--nreps", type = "integer", default = 1,
    help = "Number of repetitions for the experiment [default %default]"
  ),
  make_option(
    "--save", action = "store_true", default = FALSE,
    help = "Whether to save the experiment results"
  ),
  make_option(
    "--use_cached", action = "store_true", default = FALSE,
    help = "Whether to use cached results"
  ),
  make_option(
    "--checkpoint_n_reps", type = "integer", default = 0,
    help = "Number of repetitions between checkpoints [default %default]"
  ),
  make_option(
    "--classification", action = "store_true", default = FALSE,
    help = "Whether to use classification methods and evaluators (default: FALSE)"
  ),
  make_option(
    "--nerfplus_only", action = "store_true", default = FALSE,
    help = "Whether to run only the NeRF+ methods (default: FALSE)"
  ),
  make_option(
    "--embedding_type", type = "character", default = "laplacian",
    help = "Type of embedding to use (default: 'laplacian')"
  ),
  make_option(
    "--embedding_ndim", type = "integer", default = 2,
    help = "Number of dimensions for the embedding (default: 2)"
  ),
  make_option(
    "--embedding_reg", type = "double", default = 0.05,
    help = "Regularization parameter for network embedding (default: 0.05)"
  ),
  make_option(
    "--train_prop", type = "double", default = 0.75,
    help = "Proportion of data to use for training (between 0 and 1)"
  ),
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
  ),
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
  ),
  make_option(
    "--weighted_network", action = "store_true", default = FALSE,
    help = "whether to use a weighted network (default: FALSE)"
  )
)
# parse the command line options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
str(opt)
N_REPS <- opt$nreps
USE_CACHED <- opt$use_cached
CHECKPOINT_N_REPS <- opt$checkpoint_n_reps
if (opt$save) {
  SAVE <- c("fit", "eval")
} else {
  SAVE <- FALSE
}

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))
dgp <- switch(
  opt$dgp,
  linear_additive_block = linear_additive_blockwise_network_dgp,
  linear_autocorrelation = linear_network_autocorrelation_dgp,
  poly_additive_block = poly_additive_blockwise_network_dgp,
  poly_autocorrelation = poly_network_autocorrelation_dgp,
  lss_additive_block = lss_additive_blockwise_network_dgp,
  lss_autocorrelation = lss_network_autocorrelation_dgp,
  linear_additive_block_real = linear_additive_blockwise_network_real_data_dgp,
  linear_autocorrelation_real = linear_network_autocorrelation_real_data_dgp,
  poly_additive_block_real = poly_additive_blockwise_network_real_data_dgp,
  poly_autocorrelation_real = poly_network_autocorrelation_real_data_dgp,
  linear_additive_block_outliers = linear_additive_blockwise_network_outliers_dgp,
  linear_autocorrelation_outliers = linear_network_autocorrelation_outliers_dgp,
  poly_additive_block_outliers = poly_additive_blockwise_network_outliers_dgp,
  poly_autocorrelation_outliers = poly_network_autocorrelation_outliers_dgp,
  lss_additive_block_outliers = lss_additive_blockwise_network_outliers_dgp,
  lss_autocorrelation_outliers = lss_network_autocorrelation_outliers_dgp,
  logistic_additive_block = logistic_additive_blockwise_network_dgp,
  logistic_poly_additive_block = logistic_poly_additive_blockwise_network_dgp,
  logistic_lss_additive_block = logistic_lss_additive_blockwise_network_dgp,
  stop("Invalid dgp option")
)

# n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
n_cores <- Sys.getenv("NSLOTS")
if (n_cores != "") {
  n_cores <- as.integer(n_cores)
  print(n_cores)
  if (n_cores > 1) {
    plan(multicore, workers = n_cores)
    # plan(multisession, workers = n_cores)
  }
}

SAVE_DIR <- here::here()
cat(sprintf("Saving results to: %s\n", SAVE_DIR))

FUTURE_GLOBALS <- c(
  "generate_alphas", "get_connected_graph", "load_data",
  "preprocess_fun",  "get_grouped_features", "return_method_output",
  "evaluate_fi"
)
FUTURE_PACKAGES <- NULL
