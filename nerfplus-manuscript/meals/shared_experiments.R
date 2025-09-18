experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  ### non-network-assisted baseline methods
  add_method(lm_method) |>
  add_method(rf_method) |>
  add_method(rfplus_method) |>
  add_method(bart_method) |>
  ### network-assisted baseline methods
  add_method(rnc_method) |>
  add_method(network_bart_method) |>
  ### NeRF+ methods
  add_method(nerfplus_method) |>
  add_method(nerfplus_embedding_only_method) |>
  add_method(nerfplus_cohesion_only_method) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval) |>
  ### Visualizers
  add_visualizer(pred_err_plot) |>
  add_visualizer(permute_fi_plot) |>
  add_visualizer(mdiplus_fi_plot)

outlier_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  add_method(nerfplus_method) |>
  add_visualizer(influence_outliers_plot)

school_conflict_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  ### non-network-assisted baseline methods
  add_method(lm_method) |>
  add_method(rf_method) |>
  add_method(rfplus_method) |>
  add_method(bart_method) |>
  ### network-assisted baseline methods
  add_method(rnc_method) |>
  add_method(network_bart_method) |>
  ### NeRF+ methods
  add_method(nerfplus_method) |>
  add_method(nerfplus_embedding_only_method) |>
  add_method(nerfplus_cohesion_only_method) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval) |>
  ### Visualizers
  add_visualizer(school_conflict_pred_err_plot) |>
  add_visualizer(school_conflict_permute_fi_plot) |>
  add_visualizer(school_conflict_mdiplus_fi_plot) |>
  add_visualizer(school_conflict_lfi_plot)

philly_crime_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME, dgp_name)
) |>
  ### non-network-assisted baseline methods
  add_method(lm_method) |>
  add_method(rf_method) |>
  add_method(rfplus_method) |>
  add_method(bart_method) |>
  ### network-assisted baseline methods
  add_method(rnc_method) |>
  add_method(network_bart_method) |>
  ### NeRF+ methods
  add_method(nerfplus_method) |>
  add_method(nerfplus_embedding_only_method) |>
  add_method(nerfplus_cohesion_only_method) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval)
