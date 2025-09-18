method_levels <- c(
  "NeRF+",
  "RNC",
  "Network BART",
  "RF+",
  "Linear Regression",
  "BART",
  "RF"
)
method_levels_all <- c(
  "NeRF+",
  "NeRF+ (Cohesion Only)",
  "NeRF+ (Embedding Only)",
  "RNC",
  "Network BART",
  "RF+",
  "Linear Regression",
  "BART",
  "RF"
)
COLORS <- c(
  "NeRF+" = "black",
  "NeRF+ (Cohesion Only)" = "#714F9D",
  "NeRF+ (Embedding Only)" = "#FF9301",
  "RNC" = "#DE68A1",
  "Network BART" = "#68A65E",
  "RF+" = "black",
  "Linear Regression" = "#DE68A1",  
  "BART" = "#68A65E",
  "RF" = "grey"
)
LINETYPES <- c(
  "NeRF+" = "solid",
  "NeRF+ (Cohesion Only)" = "solid",
  "NeRF+ (Embedding Only)" = "solid",
  "RNC" = "solid",
  "Network BART" = "solid",
  "RF+" = "dashed",
  "Linear Regression" = "dashed",
  "BART" = "dashed",
  "RF" = "dashed"
)
NETWORK_COLORS <- c("gray", "#57B2C8")

if (stringr::str_detect(dgp$name, "Blockwise")) {
  vary_param_lab <- expression(
    bold(paste("Network Effect (", eta, ")"))
  )
} else {
  vary_param_lab <- expression(
    bold(paste("Network Effect (", omega, ")"))
  )
}

#### Prediction Visualizers ####
pred_err_plot <- create_visualizer(
  .viz_fun = plot_pred_err_wrapper,
  .name = 'Prediction Accuracy Plot',
  eval_name = 'Prediction Accuracy',
  keep_methods = method_levels,
  show = c("point", "line", "ribbon"),
  metrics = yardstick::metric_set(yardstick::rsq),
  x_str = ".vary_params1",
  y_str = "mean_pred_err",
  linetype_str = ".method_name",
  facet_formula = .metric ~ .vary_params2,
  facet_type = "grid",
  plot_by = NULL,
  err_sd_str = "se_pred_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 1.1
  ),
  facet_args = list(labeller = ggplot2::label_both),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(
      x = vary_param_lab,
      y = "Mean Test R-squared", 
      linetype = "Method"
    ),
    vthemes::theme_vmodern(size_preset = "large"),
    ggplot2::theme(
      strip.text.y = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(1.25, "cm")
    )
  ),
  .doc_options = list(width = 12, height = 4)
)

school_conflict_pred_err_plot <- create_visualizer(
  .viz_fun = plot_pred_err_wrapper,
  .name = 'Prediction Accuracy Plot',
  eval_name = 'Prediction Accuracy',
  keep_methods = method_levels,
  show = c("bar", "errorbar"),
  metrics = yardstick::metric_set(yardstick::rmse),
  err_sd_str = "se_pred_err",
  errorbar_args = list(width = 0.5),
  facet_formula = NULL,
  add_ggplot_layers = list(
    ggplot2::geom_text(
      ggplot2::aes(
        x = .method_name, y = mean_pred_err, label = sprintf("%.3f", mean_pred_err)
      ),
      color = "white", vjust = 1.5
    ),
    ggplot2::labs(
      x = "Method",
      y = "Mean Test RMSE"
    ),
    vthemes::theme_vmodern(size_preset = "medium", x_text_angle = TRUE)
  ),
  .doc_options = list(width = 6, height = 5)
)

philly_crime_pred_err_plot <- create_visualizer(
  .viz_fun = plot_pred_err_wrapper,
  .name = 'Prediction Accuracy Plot',
  eval_name = 'Prediction Accuracy',
  keep_methods = method_levels_all,
  show = c("point", "line", "ribbon"),
  metrics = yardstick::metric_set(yardstick::rsq),
  x_str = "subsample",
  y_str = "mean_pred_err",
  linetype_str = ".method_name",
  facet_formula = ~ tract_groups,
  facet_type = "grid",
  plot_by = NULL,
  err_sd_str = "se_pred_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 1.1
  ),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(
      x = "Training Proportion",
      y = "R-squared", 
      linetype = "Method"
    ),
    vthemes::theme_vmodern(size_preset = "large"),
    ggplot2::theme(
      strip.text.y = ggplot2::element_blank(),
      legend.key.width = ggplot2::unit(1.25, "cm")
    )
  ),
  .doc_options = list(width = 12, height = 4)
)

#### Global Feature Importance Visualizers ####
permute_fi_plot <- create_visualizer(
  .viz_fun = plot_feature_importance_wrapper,
  .name = 'Permutation Feature Importances Plot',
  eval_name = 'Permutation Feature Importances',
  keep_methods = method_levels,
  feature_col = feature_col,
  err_sd_str = "se_feature_importance",
  color_str = ".vary_params1",
  fill_str = ".vary_params1",
  facet_formula = .method_name ~ .,
  facet_args = list(
    scales = "free_y",
    labeller = function(labels) {
      lapply(labels, function(x) gsub(" ", "\n", x))
    }
  ),
  plot_by = "pve",
  add_ggplot_layers = list(
    ggplot2::scale_x_discrete(
      labels = c(
        ".network" = expression(paste("Network (", alpha, ", Z)")),
        ".alpha" = expression(paste("Network (", alpha, ")")),
        ".embed" = "Network (Z)"
      )
    ),
    ggplot2::labs(
      x = "Feature", 
      y = "Permutation Importance",
      fill = vary_param_lab,
      color = vary_param_lab
    ),
    vthemes::theme_vmodern(size_preset = "medium"),
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1
      )
    )
  ),
  .doc_options = list(width = 12, height = 26)
)

school_conflict_features <- c(
  ".network" = expression(paste("Network (", alpha, ", Z)")),
  ".alpha" = expression(paste("Network (", alpha, ")")),
  ".embed" = "Network (Z)",
  "PNW1" = "Beginning Friendliness",
  "GRC" = "Grade",
  "GENC" = "Gender",
  "ETHA" = "Ethnicity - Asian",
  "ETHB" = "Ethnicity - Black",
  "ETHH" = "Ethnicity - Hispanic",
  "ETHW" = "Ethnicity - White",
  "LIVEWB" = "Live w/ both parents",
  "RETURN" = "Returning student",
  "TREAT" = "Treatment"
)
school_conflict_permute_fi_plot <- create_visualizer(
  .viz_fun = plot_feature_importance_wrapper,
  .name = 'Permutation Feature Importances Plot',
  eval_name = 'Permutation Feature Importances',
  keep_methods = method_levels,
  feature_col = feature_col,
  feature_order = names(school_conflict_features),
  fill_str = ".network_group",
  color_str = ".network_group",
  err_sd_str = "se_feature_importance",
  errorbar_args = list(width = 0.5),
  facet_formula = .method_name ~ .,
  facet_args = list(
    scales = "free_y",
    labeller = function(labels) {
      lapply(labels, function(x) gsub(" ", "\n", x))
    }
  ),
  add_ggplot_layers = list(
    ggplot2::scale_x_discrete(
      labels = school_conflict_features
    ),
    ggplot2::scale_color_manual(values = NETWORK_COLORS),
    ggplot2::scale_fill_manual(values = NETWORK_COLORS),
    ggplot2::labs(
      x = "Feature", 
      y = "Permutation Importance"
    ),
    vthemes::theme_vmodern(size_preset = "medium"),
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1
      )
    ),
    ggplot2::guides(color = "none", fill = "none")
  ),
  .doc_options = list(width = 6.5, height = 8)
)

philly_crime_features <- c(
  ".network" = expression(paste("Network (", alpha, ", Z)")),
  ".alpha" = expression(paste("Network (", alpha, ")")),
  ".embed" = "Network (Z)",
  "time" = "Time",
  "MeanAvgTemperature" = "Temperature",
  "TotalPrecipitation" = "Total Precipitation",
  "NumberofDaysPrecipitation" = "Precipitation Days",
  "TotalSnowfall" = "Total Snowfall"
)
philly_crime_permute_fi_plot <- school_conflict_permute_fi_plot$clone()
philly_crime_permute_fi_plot$viz_params$feature_order <- names(philly_crime_features)
philly_crime_permute_fi_plot$viz_params$add_ggplot_layers <- c(
  philly_crime_permute_fi_plot$viz_params$add_ggplot_layers,
  ggplot2::scale_x_discrete(
    labels = philly_crime_features
  )
)

mdiplus_fi_plot <- permute_fi_plot$clone()
mdiplus_fi_plot$name <- 'MDI+ Feature Importances Plot'
mdiplus_fi_plot$viz_params$eval_name <- 'MDI+ Feature Importances'
mdiplus_fi_plot$viz_params$add_ggplot_layers <- c(
  mdiplus_fi_plot$viz_params$add_ggplot_layers,
  list(ggplot2::labs(y = "MDI+ Importance"))
)

school_conflict_mdiplus_fi_plot <- school_conflict_permute_fi_plot$clone()
school_conflict_mdiplus_fi_plot$name <- 'MDI+ Feature Importances Plot'
school_conflict_mdiplus_fi_plot$viz_params$eval_name <- 'MDI+ Feature Importances'
school_conflict_mdiplus_fi_plot$viz_params$add_ggplot_layers <- c(
  school_conflict_mdiplus_fi_plot$viz_params$add_ggplot_layers,
  list(ggplot2::labs(y = "MDI+ Importance"))
)

philly_crime_mdiplus_fi_plot <- school_conflict_mdiplus_fi_plot$clone()
philly_crime_mdiplus_fi_plot$viz_params$feature_order <- names(philly_crime_features)
philly_crime_mdiplus_fi_plot$viz_params$add_ggplot_layers <- c(
  philly_crime_mdiplus_fi_plot$viz_params$add_ggplot_layers,
  ggplot2::scale_x_discrete(
    labels = philly_crime_features
  )
)

#### Local Importance Visualizer ####
school_conflict_lfi_plot <- create_visualizer(
  .viz_fun = plot_lfi,
  .name = "Local Network Importance Plot",
  keep_method = "NeRF+",
  mode = ".network",
  point_size = 6,
  fill_var = "GRC",
  fill_label = "Grade",
  fill_values = c("#e8f8e5", "#ececec", "#dbeef4", "#fcebfd"),
  seed = 1234,
  .doc_options = list(width = 16, height = 8)
)

philly_crime_lfi_plot <- create_visualizer(
  .viz_fun = plot_lfi,
  .name = "Local Network Cohesion Importance Plot",
  keep_method = "NeRF+",
  mode = ".alpha",
  point_size = 4,
  seed = 123456,
  .doc_options = list(width = 20, height = 8)
)

#### Sample Influence Visualizer ####
influence_outliers_plot <- create_visualizer(
  .viz_fun = plot_influence_outliers,
  .name = "Sample Influence with Outliers Plot"
)