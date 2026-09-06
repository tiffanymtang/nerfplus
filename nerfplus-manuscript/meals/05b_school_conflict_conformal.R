rm(list = ls())
EXP_NAME <- "School Conflict (Conformal)"
set.seed(331)
here::i_am(file.path("meals", "05b_school_conflict_conformal.R"))
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
  train_prop = 1
)
data_list <- dgp$generate()

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
nlams <- 20
lambdas_netcoh <- exp(seq(log(1000), log(0.001), length.out = nlams)) * nrow(data_list$x)^2
lambdas_embed <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_raw <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
lambdas_stump <- exp(seq(log(1000), log(0.001), length.out = nlams)) * ncol(data_list$x)
importance_modes <- NULL
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
source(here::here(file.path("meals", "shared_experiments.R")))

conformal_out <- furrr::future_map(
  1:nrow(data_list$x),
  function(i) {
    train_idxs <- setdiff(1:nrow(data_list$x), i)
    data_list_i <- data_list
    data_list_i$x_test <- data_list$x[i, , drop = FALSE]
    data_list_i$y_test <- data_list$y[i, , drop = FALSE]
    data_list_i$x <- data_list$x[train_idxs, , drop = FALSE]
    data_list_i$y <- data_list$y[train_idxs, , drop = FALSE]
    data_list_i$A <- data_list$A[train_idxs, train_idxs]
    data_list_i$A_full <- data_list$A_full[c(train_idxs, i), c(train_idxs, i)]
    conformal_out <- tryCatch(
      {
        nerfplus_conformal_method$fit(data_list_i)
      },
      error = function(e) NULL
    )
    return(conformal_out)
  },
  .options = furrr::furrr_options(seed = 331)
)
conformal_df <- purrr::list_rbind(conformal_out)
OUT_DIR <- file.path(
  SAVE_DIR, "results", EXP_NAME,
  sprintf("%s_%s", opt$embedding_type, opt$embedding_ndim),
  sprintf("School %s", opt$schid)
)
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}
saveRDS(
  conformal_df,
  file = file.path(OUT_DIR, "conformal_results.rds")
)
