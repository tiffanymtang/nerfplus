#' Summarize feature importances
#'
#' @description This function summarizes the feature importance results,
#'   ignoring runs where the feature importance is NULL.
#'
#' @param fit_results A tibble containing the fit results.
#' @param vary_params A character vector of parameters that vary across runs.
#' @param nested_cols A character vector of column names that are nested within
#'   fit results.
#' @param feature_col The name of the column containing feature names.
#' @param imp_col The name of the column containing importance values.
#' @param ... Additional arguments passed to the summarization function.
#'
#' @return A tibble summarizing the feature importance results.
summarize_feature_importance_with_null <- function(fit_results,
                                                   vary_params = NULL,
                                                   nested_cols = NULL,
                                                   feature_col,
                                                   imp_col, ...) {
  if (!is.null(nested_cols)) {
    fit_results <- fit_results[!sapply(fit_results[[nested_cols]], is.null), ]
  } else {
    fit_results <- fit_results[!sapply(fit_results[[feature_col]], is.null), ]
  }
  summarize_feature_importance(
    fit_results = fit_results, vary_params = vary_params,
    nested_cols = nested_cols, feature_col = feature_col, imp_col = imp_col, ...
  )
}


#' Evaluate prediction error
summarize_pred_err_wrapper <- function(fit_results, vary_params = NULL, ...) {
  if (all(unique(fit_results$y_test[[1]]) %in% c(0, 1))) {
    fit_results <- fit_results |>
      dplyr::rowwise() |>
      dplyr::mutate(
        y_test = list(factor(y_test, levels = c(1, 0))),
        prob_predictions = list(predictions),
        predictions = list(factor(as.numeric(predictions >= 0.5), levels = c(1, 0)))
      ) |>
      dplyr::ungroup()
    eval_summary <- simChef::summarize_pred_err(
      fit_results = fit_results, vary_params = vary_params,
      prob_cols = "prob_predictions", ...
    )
  } else {
    eval_summary <- simChef::summarize_pred_err(
      fit_results = fit_results, vary_params = vary_params, ...
    )
  }
  return(eval_summary)
}


#' Evaluate conformal coverage
summarize_conformal_coverage <- function(fit_results, vary_params = NULL) {
  id_cols <- c(".rep", ".dgp_name", ".method_name", vary_params)
  group_cols <- setdiff(id_cols, ".rep")
  eval_tib <- fit_results |>
    tidyr::unnest(c(y_test, predictions)) |>
    dplyr::mutate(
      is_in_ci = (y_test >= lower_bound) & (y_test <= upper_bound)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      coverage = mean(is_in_ci),
      ci_width = mean(upper_bound - lower_bound),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))
  coverage_summary <- simChef::eval_summarizer(
    eval_tib, eval_id = "coverage", value_col = "coverage",
    custom_summary_funs = list(
      "se_coverage" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
    )
  )
  width_summary <- simChef::eval_summarizer(
    eval_tib, eval_id = "ci_width", value_col = "ci_width",
    custom_summary_funs = list(
      "se_ci_width" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
    )
  )
  eval_summary <- dplyr::left_join(
    coverage_summary, width_summary, by = group_cols
  )
  return(eval_summary)
}
