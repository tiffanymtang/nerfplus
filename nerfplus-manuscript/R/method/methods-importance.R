#' Evaluate feature importance
#'
#' @param object A fitted model object.
#' @param x A data frame or matrix used to compute the feature importances.
#' @param y A vector of target values. Not needed if `methods = "local"`.
#' @param A_full An adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order).
#' @param nodeids (Optional) vector of node IDs of length n.
#'   If provided, node IDs indicate the rows of A, corresponding to each
#'   sample. If not provided, the rows of A are assumed to be in the same order
#'   as the rows of x and y.
#' @param x_means A named vector of means for each feature in `x`. If `NULL`,
#'   the means are computed from `x`.
#' @param classification Logical indicating whether the model is for
#'   classification.
#' @param methods A character vector of methods to use for feature importance
#'   calculation. Options are "permute", "mdi+", and "local".
#' @param grouped_features A named list of features to group together for
#'   feature importance calculation. Each element should be a character vector
#'   containing the names of features to group. If `NULL`, no grouping is done.
#' @param B Number of samples for permutation-based feature
#'   importance. Ignored if `method` is not `"permute"`.
#' @param metric A function to compute the metric used for feature importance.
#'   Defaults to R-squared for linear models and AUROC for logistic models.
#'   Ignored if `method` is `"local"`.
#'
#' @return A list containing two data frames:
#' - `global`: A data frame with global feature importance scores.
#' - `local`: A data frame with local feature importance scores (if applicable).
evaluate_fi <- function(object, x, y = NULL, A_full = NULL, nodeids = NULL,
                        x_means = NULL,
                        classification = FALSE,
                        methods = c("permute", "mdi+", "local"),
                        grouped_features = NULL,
                        B = 10,
                        metric = NULL) {
  methods <- match.arg(methods, several.ok = TRUE)
  if (is.null(metric)) {
    if (!classification) {
      metric <- nerfplus:::rsq_narm_vec
    } else {
      metric <- yardstick::roc_auc_vec
    }
  }

  if (is.null(x_means)) {
    x_means <- apply(x, 2, mean)
  }

  fi_out <- purrr::map(
    methods,
    function(method) {
      if (identical(method, "mdi+")) {
        fi_out <- nerfplus:::tree_mdiplus_fi(
          object, x = x, y = y, A_full = A_full,
          nodeids = nodeids, x_means = x_means,
          metric = metric, grouped_features = grouped_features
        )
      } else if (identical(method, "permute")) {
        fi_out <- nerfplus:::tree_permute_fi(
          object, x = x, y = y, A_full = A_full,
          nodeids = nodeids, metric = metric,
          grouped_features = grouped_features, B = B
        )
      } else if (identical(method, "local")) {
        fi_out <- nerfplus:::tree_local_fi(
          object, x = x, A_full = A_full,
          nodeids = nodeids, x_means = x_means,
          grouped_features = grouped_features
        )
      }
      if (method != "local") {
        global_fi <- tidyr::pivot_longer(
          fi_out, cols = tidyselect::everything(),
          names_to = "var", values_to = method
        )
        local_fi <- NULL
      } else {
        global_fi <- NULL
        local_fi <- fi_out
      }
      return(list(global = global_fi, local = local_fi))
    }
  ) |>
    setNames(methods)

  global_fi <- purrr::map(fi_out, "global") |>
    purrr::compact() |>
    purrr::reduce(dplyr::left_join, by = "var")
  local_fi <- purrr::map(fi_out, "local") |>
    purrr::compact()
  return(list(global = global_fi, local = local_fi))
}
