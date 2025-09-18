rm(list = ls())
EXP_NAME <- "LOO Simulations"
SAVE <- TRUE
USE_CACHED <- FALSE
CHECKPOINT_N_REPS <- 0
set.seed(331)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 1

#### DGPs ####

source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp <- lss_network_autocorrelation_dgp
dgp_name <- dgp$name
print(dgp_name)

#### Methods ####

source(here::here(file.path("meals", "shared_method_params_default.R")))
importance_modes <- NULL
source(here::here(file.path("meals", "shared_methods.R")))

#### Run Experiment ####
# fit NeRF+ with CV to get appropriate CV hyperparameters
data_list <- dgp$generate()
nerfplus_out <- nerfplus_method$fit(data_list, return_fit = TRUE)
nerfplus_fit <- nerfplus_out$fit[[1]]

best_cv_params <- nerfplus_fit$best_cv_params[1:ntrees_cv, ] |>
  dplyr::summarise(
    dplyr::across(
      tidyselect::starts_with("lambda"),
      ~ table(.x) |>
        which.max() |>
        names() |>
        as.numeric()
    )
  )

# fit NeRF+ with LOO
out <- nerfplus_loo_method_fun(
  x = data_list$x,
  y = data_list$y,
  x_test = data_list$x_test,
  y_test = data_list$y_test,
  A = data_list$A,
  A_full = data_list$A_full,
  # max_n = 2, # for testing
  classification = FALSE,
  include_netcoh = TRUE,
  lambda_netcoh = best_cv_params$lambda_netcoh[[1]],
  lambda_embed = best_cv_params$lambda_embed[[1]],
  lambda_raw = best_cv_params$lambda_raw[[1]],
  lambda_stump = best_cv_params$lambda_stump[[1]],
  lambda_l = best_cv_params$lambda_l[[1]],
  embedding = nerfplus_method$method_params$embedding,
  embedding_options = nerfplus_method$method_params$embedding_options
)

if (!dir.exists(file.path(SAVE_DIR, "results", EXP_NAME, dgp$name))) {
  dir.create(file.path(SAVE_DIR, "results", EXP_NAME, dgp$name), recursive = TRUE)
}
saveRDS(out, file = file.path(SAVE_DIR, "results", EXP_NAME, dgp$name, "results.rds"))
