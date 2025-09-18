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
