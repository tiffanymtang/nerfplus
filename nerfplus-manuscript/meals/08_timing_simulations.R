# Rscript meals/08_timing_simulations.R --nreps 50 --save --dgp linear_additive_block

rm(list = ls())
EXP_NAME <- "Timing Simulations"
set.seed(331)
here::i_am(file.path("meals", "08_timing_simulations.R"))
source(here::here(file.path("meals", "setup.R")))

if (!dir.exists(file.path(SAVE_DIR, "results", EXP_NAME))) {
  dir.create(file.path(SAVE_DIR, "results", EXP_NAME), recursive = TRUE)
}

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

nerfplus_method <- create_method(
  .method_fun = nerfplus_method_fun,
  .name = "NeRF+",
  classification = classification,
  include_netcoh = TRUE,
  lambda_netcoh = 1,
  lambda_embed = 1,
  lambda_raw = 1,
  lambda_stump = 1,
  embedding = embedding,
  embedding_options = embedding_options,
  importance_modes = importance_modes,
  importance_options = importance_options
)

source(here::here(file.path("meals", "shared_experiments.R")))

methods_list <- list(
  nerfplus_method,
  network_bart_method
)
names(methods_list) <- purrr::map_chr(methods_list, ~ .x$name)

run_with_timeout <- function(method_fun, dgp_out, timeout = 3600) {
  p <- callr::r_bg(
    func = function(method_fun, dgp_out) {
      setwd(here::here())
      simChef::load_all()
      method_fun$fit(dgp_out)
    },
    args = list(method_fun = method_fun, dgp_out = dgp_out)
  )

  on.exit({
    if (p$is_alive()) p$kill_tree()
  }, add = TRUE)

  start_time <- Sys.time()
  while (p$is_alive() &&
         as.numeric(difftime(Sys.time(), start_time, units = "secs")) < timeout) {
    Sys.sleep(1)
  }

  if (p$is_alive()) {
    p$kill_tree()
    return(NULL)
  }

  p$get_result()
}

timing_df <- NULL
for (method_name in names(methods_list)) {
  method_fun <- methods_list[[method_name]]
  for (n in c(200, 300, 500, 1000, 1500)) {
    for (rep in 1:N_REPS) {
      cat(sprintf("Running %s with n=%d, rep=%d\n", method_name, n, rep))
      dgp_out <- dgp$generate(n = n)
      fit <- run_with_timeout(method_fun, dgp_out, timeout = 3600)
      if (is.null(fit)) {
        print(sprintf("Method %s failed to fit for n=%d in 1hr", method_name, n))
        break
      }
      timing_df <- rbind(
        timing_df,
        data.frame(
          method = method_name,
          n = n,
          rep = rep,
          elapsed_time = fit$time_elapsed[[1]]
        )
      )
    }
    saveRDS(timing_df, file.path(SAVE_DIR, "results", EXP_NAME, "results.rds"))
  }
}
