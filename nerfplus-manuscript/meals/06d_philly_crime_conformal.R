rm(list = ls())
EXP_NAME <- "Philly Crime (Conformal)"
set.seed(331)
here::i_am(file.path("meals", "06d_philly_crime_conformal.R"))
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####
source(here::here(file.path("meals", "shared_dgp_params_default.R")))
source(here::here(file.path("meals", "shared_dgps.R")))

dgp_name <- sprintf(
  "Philly Crime (%s%s%s, %s)",
  opt$split_mode,
  ifelse(opt$include_weather, ", with weather", ""),
  ifelse(opt$weighted_network, ", weighted network", ""),
  opt$subsample
)
dgp <- create_dgp(
  load_philly_crime_data, .name = dgp_name, train_prop = opt$train_prop,
  subsample = opt$subsample, split_mode = opt$split_mode,
  include_weather = opt$include_weather, weighted = opt$weighted_network,
  test_all = TRUE
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

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
source(here::here(file.path("meals", "shared_experiments.R")))

conformal_out <- nerfplus_conformal_method$fit(data_list)
conformal_out |>
  tidyr::unnest(c(y_test, predictions)) |>
  dplyr::mutate(
    is_in_ci = (y_test >= lower_bound) & (y_test <= upper_bound)
  ) |>
  dplyr::summarise(
    coverage = mean(is_in_ci),
    .groups = "drop"
  )

OUT_DIR <- file.path(
  SAVE_DIR, "results", EXP_NAME,
  sprintf("%s_%s", opt$embedding_type, opt$embedding_ndim),
  sprintf(
    "Philly Crime (%s%s%s, %s)",
    opt$split_mode,
    ifelse(opt$include_weather, ", with weather", ""),
    ifelse(opt$weighted_network, ", weighted network", ""),
    opt$subsample
  )
)
if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}
saveRDS(
  conformal_out,
  file = file.path(OUT_DIR, "conformal_results.rds")
)
