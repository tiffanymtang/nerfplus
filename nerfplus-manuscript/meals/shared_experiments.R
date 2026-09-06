experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(
    SAVE_DIR, "results", EXP_NAME,
    sprintf("%s_%s_%s", opt$embedding_type, opt$embedding_ndim, opt$embedding_reg)
  )
) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval) |>
  ### Visualizers
  add_visualizer(pred_err_plot) |>
  add_visualizer(permute_fi_plot) |>
  add_visualizer(mdiplus_fi_plot)

if (opt$nerfplus_only) {
  experiment <- experiment |>
    ### NeRF+ methods
    add_method(nerfplus_method) |>
    add_method(nerfplus_embedding_only_method) |>
    add_method(nerfplus_cohesion_only_method)
} else {
  experiment <- experiment |>
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
    add_method(nerfplus_cohesion_only_method)
}

outlier_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  add_method(nerfplus_method) |>
  add_visualizer(influence_outliers_plot)

school_conflict_experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(
    SAVE_DIR, "results", EXP_NAME,
    sprintf("%s_%s", opt$embedding_type, opt$embedding_ndim),
    sprintf("School %s", opt$schid)
  )
) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval) |>
  ### Visualizers
  add_visualizer(school_conflict_pred_err_plot) |>
  add_visualizer(school_conflict_permute_fi_plot) |>
  add_visualizer(school_conflict_mdiplus_fi_plot) |>
  add_visualizer(school_conflict_lfi_plot)

if (opt$nerfplus_only) {
  school_conflict_experiment <- school_conflict_experiment |>
    ### NeRF+ methods
    add_method(nerfplus_method) |>
    add_method(nerfplus_embedding_only_method) |>
    add_method(nerfplus_cohesion_only_method)
} else {
  school_conflict_experiment <- school_conflict_experiment |>
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
    add_method(nerfplus_cohesion_only_method)
}

philly_crime_experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(
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
) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  add_evaluator(permute_fi_eval) |>
  add_evaluator(mdiplus_fi_eval)

if (opt$nerfplus_only) {
  philly_crime_experiment <- philly_crime_experiment |>
    ### NeRF+ methods
    add_method(nerfplus_method) |>
    add_method(nerfplus_embedding_only_method) |>
    add_method(nerfplus_cohesion_only_method)
} else {
  philly_crime_experiment <- philly_crime_experiment |>
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
    add_method(nerfplus_cohesion_only_method)
}

conformal_experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(SAVE_DIR, "results", EXP_NAME, dgp$name)
) |>
  add_method(nerfplus_conformal_method) |>
  add_evaluator(conformal_eval) |>
  add_visualizer(conformal_coverage_plot)

bamdt_experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(
    SAVE_DIR, "results", EXP_NAME,
    sprintf("%s_%s_%s", opt$embedding_type, opt$embedding_ndim, opt$embedding_reg)
  )
) |>
  #### Methods
  add_method(bamdt_method) |>
  add_method(bamdt_method2) |>
  add_method(bamdt_method3) |>
  add_method(network_bart_method) |>
  add_method(nerfplus_method) |>
  ### Evaluators
  add_evaluator(pred_err_eval) |>
  ### Visualizers
  add_visualizer(pred_err_plot)
