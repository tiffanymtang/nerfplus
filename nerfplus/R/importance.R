#' Compute feature importances for a NeRF+ model
#'
#' @description This function computes global and local feature importances
#'   given a fitted NeRF+ model.
#'
#' @param object A fitted NeRF+ model object.
#' @param x A data frame or matrix used to compute the feature importances.
#' @param x_embed An optional data frame or matrix of network embeddings
#'   corresponding to the samples in `x`. Only needed if training embeddings
#'   were manually inputted.
#' @param y A vector of responses.
#' @param A_full An adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order)
#' @param nodeids (Optional) vector of node IDs of length n.
#'   If provided, node IDs indicate the rows of A, corresponding to each
#'   sample. If not provided, the rows of A are assumed to be in the same order
#'   as the rows of x and y.
#' @param method A character string indicating the method to use for computing
#'   feature importances. Options are:
#'   - `"permute"`: Permutation-based global feature importance.
#'   - `"mdi+"`: MDI+ global feature importance.
#'   - `"local"`: Local feature importance.
#' @param B Number of samples for permutation-based feature
#'   importance. Ignored if `method` is not `"permute"`.
#' @param metric A function to compute the metric used for global feature
#'   importances. Defaults to R-squared for regression and AUROC for
#'   classification. Ignored if `method` is `"local"`.
#'
#' @returns If `method` is `"permute"` or `"mdi+"`, a tibble with two columns:
#' - `var`: The name of the feature.
#' - `importance`: The computed feature importance score.
#' If `method` is `"local"`, an n x p
#'
#' @examples
#' \donttest{
#' data(example_data)
#' nerfplus_out <- nerfplus(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1,
#'   lambda_embed = 0.1,
#'   lambda_raw = 2,
#'   lambda_stump = 3,
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#' fi_out <- get_feature_importances(
#'   nerfplus_out, x = example_data$xtest, y = example_data$ytest,
#'   A_full = example_data$A_full, method = "mdi+"
#' )
#' }
#'
#' @export
get_feature_importances <- function(object, x, x_embed = NULL, y = NULL, A_full,
                                    nodeids = NULL,
                                    method = c("permute", "mdi+", "local"),
                                    B = 10, metric = NULL) {
  method <- match.arg(method)
  rf_fit <- object$rf_fit
  nerfplus_fits <- object$nerfplus_fits
  tree_infos <- object$tree_infos
  pre_rf_preprocessing_info <- object$pre_rf_preprocessing_info
  include_raw <- object$model_info$include_raw
  unordered_factors <- object$unordered_factors
  family <- object$model_info$family
  if (is.null(metric)) {
    if (family == "linear") {
      metric <- rsq_narm_vec
    } else if (family == "logistic") {
      metric <- yardstick::roc_auc_vec
    }
  }
  if (identical(method, "permute")) {
    permute_idxs <- purrr::map(1:B, ~ sample(1:nrow(x)))
    orig_preds <- stats::predict(
      object, x = x, x_embed = x_embed, A_full = A_full, nodeids = nodeids
    )
  }

  x <- apply_pre_rf_preprocessing(
    pre_rf_preprocessing_info, x = x, x_embed = x_embed, A_full = A_full,
    nodeids = nodeids
  )
  x_numeric <- apply_post_rf_preprocessing(rf_fit, x)

  node_preds <- stats::predict(
    rf_fit, x, type = "terminalNodes", num.threads = 1
  )$predictions |>
    as.data.frame()
  forest_paths <- get_forest_paths(tree_infos)

  fi_scores_ls <- purrr::pmap(
    list(
      tree_fit = nerfplus_fits,
      tree_info = tree_infos,
      tree_node_preds = node_preds,
      tree_paths = forest_paths
    ),
    function(tree_fit, tree_info, tree_node_preds, tree_paths) {
      psi <- apply_psi(
        x = x_numeric,
        tree_info = tree_info,
        tree_paths = tree_paths,
        node_preds = tree_node_preds,
        unordered_factors = unordered_factors,
        psi_unique_values = tree_fit$preprocessing_info$psi_unique_values
      )
      x_augmented <- apply_augmentation(
        x = x,
        psi = psi,
        tree_info = tree_info,
        include_raw = include_raw,
        dummy_fit = tree_fit$preprocessing_info$dummy_fit
      )
      if (ncol(x_augmented) == 0) {
        x_augmented <- cbind(
          x_augmented,
          matrix(1, nrow = nrow(x_augmented), ncol = 1)
        )
      }
      grouped_features <- get_grouped_tree_features(
        colnames(x), colnames(x_augmented), tree_info
      )

      # get feature importance
      if (identical(method, "mdi+")) {
        x_train_means <- tree_fit$preprocessing_info$x_train_means
        fi_out <- tree_mdiplus_fi(
          tree_object = tree_fit, x = x_augmented, y = y, A_full = A_full,
          nodeids = nodeids,
          x_means = x_train_means[colnames(x_augmented)],
          metric = metric, grouped_features = grouped_features
        )
      } else if (identical(method, "permute")) {
        fi_out <- tree_permute_fi(
          tree_object = tree_fit, x = x_augmented, y = y, A_full = A_full,
          nodeids = nodeids,
          metric = metric, grouped_features = grouped_features,
          B = permute_idxs, return_preds = TRUE
        )
      } else if (identical(method, "local")) {
        x_train_means <- tree_fit$preprocessing_info$x_train_means
        fi_out <- tree_local_fi(
          tree_object = tree_fit, x = x_augmented, A_full = A_full,
          nodeids = nodeids,
          x_means = x_train_means[colnames(x_augmented)],
          grouped_features = grouped_features
        )
      }
      return(fi_out)
    }
  )

  if (identical(method, "permute")) {
    orig_score <- metric(truth = y, estimate = orig_preds)
    fi_scores <- purrr::map(
      purrr::list_transpose(fi_scores_ls),
      function(var_ls) {
        score <- purrr::map_dbl(
          purrr::list_transpose(var_ls),
          function(.x) {
            preds <- purrr::reduce(.x, `+`) / length(.x)
            return(orig_score - metric(truth = y, estimate = preds))
          }
        ) |>
          mean()
      }
    ) |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "var", values_to = "importance"
      )
  } else if (identical(method, "mdi+")) {
    fi_scores <- fi_scores_ls |>
      purrr::list_rbind() |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::everything(), ~ mean(.x, na.rm = TRUE)
        )
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = "var", values_to = "importance"
      )
  } else if (identical(method, "local")) {
    fi_scores <- purrr::reduce(fi_scores_ls, `+`) / length(fi_scores_ls)
  }
  return(fi_scores)
}


#' Compute permutation importance for a tree in NeRF+
#'
#' @inheritParams get_feature_importances
#' @param tree_object A fitted tree object from NeRF+.
#' @param grouped_features A list of features to group together for permutation
#'   importance. Each element of the list should be a character vector of
#'   feature names. If `NULL`, each feature is treated as a separate group.
#'   Typically, this is the output of `get_grouped_tree_features()`.
#' @param B Number of bootstrap samples for permutation-based feature
#'   importance. Alternatively, a list of permutation indices can be
#'   passed to `B`, where each element is a vector of indices for permuting
#'   features.
#' @param return_preds Logical indicating whether to return the predictions
#'   for each permutation. If `TRUE`, the function returns a list of predictions
#'   for each permutation. If `FALSE`, it returns the computed feature
#'   importance scores.
#'
#' @returns If `return_preds` is `FALSE`, a tibble with two columns:
#' - `var`: The name of the feature.
#' - `importance`: The computed feature importance score.
#' If `return_preds` is `TRUE`, a list where each element corresponds to a
#'   feature group and contains a list of predictions for each permutation.
#'
#' @keywords internal
tree_permute_fi <- function(tree_object, x, y, A_full = NULL, nodeids = NULL,
                            metric, grouped_features = NULL, B = 10,
                            return_preds = FALSE) {
  if (!is.list(B)) {
    permute_idxs <- purrr::map(1:B, ~ sample(1:nrow(x)))
  } else {
    permute_idxs <- B
  }

  if (is.null(grouped_features)) {
    grouped_features <- as.list(colnames(x))
    names(grouped_features) <- colnames(x)
  }
  if ("rnc" %in% class(tree_object)) {
    grouped_features[[".alpha"]] <- ".alpha"
    if (".embed" %in% names(grouped_features)) {
      grouped_features[[".network"]] <- c(
        ".alpha", grouped_features[[".embed"]]
      )
    }
  }

  orig_preds <- predict_tree(
    tree_object, x = x, A_full = A_full, nodeids = nodeids
  )
  if ("rnc" %in% class(tree_object)) {
    alpha <- stats::predict(tree_object, x, A_full, nodeids)$alpha
    beta <- tree_object$beta
  } else {
    alpha <- NULL
    beta <- NULL
  }

  fi_preds <- purrr::map(
    grouped_features,
    function(features) {
      if (is.null(features)) {
        return(purrr::map(permute_idxs, ~ orig_preds))
      }
      cov_partial_preds <- purrr::map(
        permute_idxs,
        function(permute_idx) {
          alpha_mod <- alpha
          x_mod <- x
          if (".alpha" %in% features) {
            alpha_mod <- alpha[permute_idx]
          }
          x_features <- setdiff(features, ".alpha")
          if (length(x_features) > 0) {
            x_mod[, x_features] <- x_mod[permute_idx, x_features]
          }
          partial_preds <- predict_tree(
            tree_object, x = x_mod, A_full = A_full, nodeids = nodeids,
            alpha = alpha_mod, beta = beta
          )
          return(partial_preds)
        }
      )
    }
  )

  if (return_preds) {
    return(fi_preds)
  }

  orig_score <- metric(truth = y, estimate = orig_preds)
  global_fis <- purrr::map(
    fi_preds,
    function(partial_preds) {
      score <- purrr::map_dbl(
        partial_preds, ~ orig_score - metric(truth = y, estimate = .x)
      ) |>
        mean()
    }
  ) |>
    tibble::as_tibble()

  return(global_fis)
}


#' Compute MDI+ feature importance for a tree in NeRF+
#'
#' @inheritParams get_feature_importances
#' @inheritParams tree_permute_fi
#' @param x_means A named vector of means for each feature in `x`. If `NULL`,
#'   the means are computed from `x`.
#'
#' @returns A tibble with two columns:
#' - `var`: The name of the feature.
#' - `importance`: The computed feature importance score.
#'
#' @keywords internal
tree_mdiplus_fi <- function(tree_object, x, y, A_full = NULL, nodeids = NULL,
                            metric, x_means = NULL, grouped_features = NULL) {
  if (is.null(x_means)) {
    x_means <- apply(x, 2, mean)
  }
  if (is.null(grouped_features)) {
    grouped_features <- as.list(colnames(x))
    names(grouped_features) <- colnames(x)
  }
  if ("rnc" %in% class(tree_object)) {
    grouped_features[[".alpha"]] <- ".alpha"
    if (".embed" %in% names(grouped_features)) {
      grouped_features[[".network"]] <- c(
        ".alpha", grouped_features[[".embed"]]
      )
    }
  }

  if ("rnc" %in% class(tree_object)) {
    alpha <- stats::predict(tree_object, x, A_full, nodeids)$alpha
    alpha_mean <- mean(tree_object$alpha)
    beta <- tree_object$beta
  } else {
    alpha <- NULL
    alpha_mean <- NULL
    beta <- NULL
  }

  global_fis <- purrr::map(
    grouped_features,
    function(features) {
      if (is.null(features)) {
        x_means_mat <- matrix(x_means, nrow = 1)
        colnames(x_means_mat) <- names(x_means)
        partial_preds <- predict_tree(
          tree_object, x = x_means_mat, A_full = A_full, nodeids = nodeids,
          alpha = alpha_mean, beta = beta
        ) |>
          rep(nrow(x))
      } else {
        mean_nodes <- setdiff(colnames(x), features)
        x_mod <- x
        x_mod[, mean_nodes] <- matrix(
          x_means[mean_nodes],
          nrow = nrow(x), ncol = length(mean_nodes), byrow = TRUE
        )
        if (".alpha" %in% features) {
          alpha_mod <- alpha
        } else {
          alpha_mod <- alpha_mean
        }
        partial_preds <- predict_tree(
          tree_object, x = x_mod, A_full = A_full, nodeids = nodeids,
          alpha = alpha_mod, beta = beta
        )
      }
      score <- metric(truth = y, estimate = partial_preds)
      return(score)
    }
  ) |>
    tibble::as_tibble()

  return(global_fis)
}


#' Compute local feature importance for a tree in NeRF+
#'
#' @inheritParams get_feature_importances
#' @inheritParams tree_permute_fi
#' @inheritParams tree_mdiplus_fi
#'
#' @returns A tibble with n rows and p columns, where n is the number of samples
#'   and p is the number of features. Each column corresponds to a feature,
#'   and each row corresponds to a sample. The values represent the local
#'   feature importance scores for each feature and sample.
#'
#' @keywords internal
tree_local_fi <- function(tree_object, x, A_full = NULL, nodeids = NULL,
                          x_means = NULL, grouped_features = NULL) {
  if (is.null(x_means)) {
    x_means <- apply(x, 2, mean)
  }
  if (is.null(grouped_features)) {
    grouped_features <- as.list(colnames(x))
    names(grouped_features) <- colnames(x)
  }
  if ("rnc" %in% class(tree_object)) {
    grouped_features[[".alpha"]] <- ".alpha"
    if (".embed" %in% names(grouped_features)) {
      grouped_features[[".network"]] <- c(
        ".alpha", grouped_features[[".embed"]]
      )
    }
  }

  if ("rnc" %in% class(tree_object)) {
    alpha <- stats::predict(tree_object, x, A_full, nodeids)$alpha
    alpha_mean <- mean(tree_object$alpha)
    beta <- tree_object$beta
  } else {
    alpha <- NULL
    alpha_mean <- NULL
    beta <- as.matrix(stats::coef(tree_object)[-1])
  }

  local_fis <- purrr::map(
    grouped_features,
    function(features) {
      local_score <- rep(0, nrow(x))
      if (".alpha" %in% features) {
        local_score <- alpha - alpha_mean
      }
      x_features <- setdiff(features, ".alpha")
      if (length(x_features) > 0) {
        keep_idxs <- match(x_features, colnames(x))
        xbeta <- as.matrix(x[, keep_idxs, drop = FALSE]) %*% beta[keep_idxs]
        xbeta_mean <- sum(x_means[keep_idxs] * beta[keep_idxs])
        local_score <- local_score + xbeta - xbeta_mean
      }
      return(c(local_score))
    }
  ) |>
    tibble::as_tibble()
  return(local_fis)
}


#' @keywords internal
get_grouped_tree_features <- function(orig_colnames, aug_colnames, tree_info) {
  # to avoid no visible binding note
  splitvarName <- NULL
  nodeID <- NULL

  if (any(stringr::str_starts(orig_colnames, ".embed"))) {
    out_colnames <- c(
      orig_colnames[!stringr::str_starts(orig_colnames, ".embed")],
      ".embed"
    )
  } else {
    out_colnames <- orig_colnames
  }
  grouped_features <- purrr::map(
    out_colnames,
    function(j) {
      if (j == ".embed") {
        j_nodes <- tree_info |>
          dplyr::filter(stringr::str_starts(splitvarName, ".embed")) |>
          dplyr::pull(nodeID)
        if (length(j_nodes) == 0) {
          return(NULL)
        } else {
          j_nodes <- paste0(".node", j_nodes)
        }
        xj_features <- aug_colnames[stringr::str_starts(aug_colnames, ".embed")]
      } else {
        j_nodes <- tree_info |>
          dplyr::filter(splitvarName == !!j) |>
          dplyr::pull(nodeID)
        if (length(j_nodes) == 0) {
          return(NULL)
        } else {
          j_nodes <- paste0(".node", j_nodes)
        }
        xj_features <- aug_colnames[
          stringr::str_detect(aug_colnames, paste0("^", j, "$")) |
            stringr::str_detect(aug_colnames, paste0("^", j, "\\.\\..*"))
        ]
      }
      c(xj_features, j_nodes)
    }
  ) |>
    stats::setNames(out_colnames)
  return(grouped_features)
}
