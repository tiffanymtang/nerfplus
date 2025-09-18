#### Prediction Evaluators ####
nested_pred_cols <- NULL
true_pred_col <- "y_test"
est_pred_col <- "predictions"

pred_err_eval <- create_evaluator(
  .eval_fun = summarize_pred_err,
  .name = 'Prediction Accuracy',
  nested_cols = nested_pred_cols,
  truth_col = true_pred_col,
  estimate_col = est_pred_col,
  custom_summary_funs = list(
    "se_pred_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

philly_crime_pred_err_eval <- create_evaluator(
  .eval_fun = summarize_pred_err,
  .name = 'Prediction Accuracy',
  nested_cols = nested_pred_cols,
  truth_col = true_pred_col,
  estimate_col = est_pred_col,
  group_cols = c("tract_groups", "subsample"),
  custom_summary_funs = list(
    "se_pred_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

#### Global Feature Importance Evaluators ####
nested_feature_cols <- "importance"
feature_col <- "var"
permute_col <- "permute"
mdiplus_col <- "mdi+"

permute_fi_eval <- create_evaluator(
  .eval_fun = summarize_feature_importance_with_null,
  .name = 'Permutation Feature Importances',
  nested_cols = nested_feature_cols,
  feature_col = feature_col,
  imp_col = permute_col,
  custom_summary_funs = list(
    "se_feature_importance" = function(x) {
      sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
    }
  )
)

mdiplus_fi_eval <- permute_fi_eval$clone()
mdiplus_fi_eval$name <- 'MDI+ Feature Importances'
mdiplus_fi_eval$eval_params$imp_col <- mdiplus_col
