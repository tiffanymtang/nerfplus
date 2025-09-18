library(simChef)
library(future)

options(simChef.plot_theme = "vthemes")
load_all()

cat(sprintf("Experiment Name: %s\n", EXP_NAME))

# n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
n_cores <- Sys.getenv("NSLOTS")
if (n_cores != "") {
  n_cores <- as.integer(n_cores)
  print(n_cores)
  if (n_cores > 1) {
    plan(multicore, workers = n_cores)
    # plan(multisession, workers = n_cores)
    N_REPS <- n_cores
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
