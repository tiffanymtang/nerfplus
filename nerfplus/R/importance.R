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
  if ((family == "logistic") && (!is.factor(y))) {
    y <- factor(y, levels = c(1, 0))
  }
  if (identical(method, "permute")) {
    if (is.list(B)) {
      permute_idxs <- B
    } else {
      permute_idxs <- purrr::map(seq_len(B), ~ sample(seq_len(nrow(x))))
    }
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
  )$predictions
  if (is.null(dim(node_preds))) {
    node_preds <- matrix(node_preds, ncol = 1)
  }
  forest_paths <- attr(object, "forest_paths", exact = TRUE)
  if (is.null(forest_paths)) {
    forest_paths <- get_forest_paths(tree_infos)
  }

  ntrees <- length(nerfplus_fits)
  laplacian_cache <- new.env(parent = emptyenv())
  solve_cache <- new.env(parent = emptyenv())
  if (identical(method, "mdi+")) {
    fi_sums <- NULL
    fi_counts <- NULL
  } else if (identical(method, "local")) {
    fi_sum <- NULL
  } else {
    pred_sums <- NULL
  }

  for (tree_id in seq_len(ntrees)) {
    tree_fit <- nerfplus_fits[[tree_id]]
    tree_info <- tree_infos[[tree_id]]
    psi <- apply_psi(
      x = x_numeric,
      tree_info = tree_info,
      tree_paths = forest_paths[[tree_id]],
      node_preds = node_preds[, tree_id],
      unordered_factors = unordered_factors,
      psi_unique_values = tree_fit$preprocessing_info$psi_unique_values,
      as_matrix = TRUE
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
    grouped_features <- get_cached_grouped_tree_features(
      orig_colnames = colnames(x),
      aug_colnames = colnames(x_augmented),
      tree_info = tree_info,
      preprocessing_info = tree_fit$preprocessing_info
    )
    L_full <- get_cached_rnc_laplacian(tree_fit, A_full, laplacian_cache)

    if (identical(method, "mdi+")) {
      x_train_means <- tree_fit$preprocessing_info$x_train_means
      fi_out <- tree_mdiplus_fi(
        tree_object = tree_fit, x = x_augmented, y = y, A_full = A_full,
        nodeids = nodeids,
        x_means = x_train_means[colnames(x_augmented)],
        metric = metric, grouped_features = grouped_features,
        L_full = L_full, solve_cache = solve_cache
      )
      fi_vec <- unlist(fi_out[1, ], use.names = TRUE)
      if (is.null(fi_sums)) {
        fi_sums <- stats::setNames(numeric(length(fi_vec)), names(fi_vec))
        fi_counts <- stats::setNames(integer(length(fi_vec)), names(fi_vec))
      }
      new_names <- setdiff(names(fi_vec), names(fi_sums))
      if (length(new_names) > 0) {
        fi_sums <- c(fi_sums, stats::setNames(numeric(length(new_names)), new_names))
        fi_counts <- c(fi_counts, stats::setNames(integer(length(new_names)), new_names))
      }
      valid_names <- names(fi_vec)[!is.na(fi_vec)]
      fi_sums[valid_names] <- fi_sums[valid_names] + fi_vec[valid_names]
      fi_counts[valid_names] <- fi_counts[valid_names] + 1L
    } else if (identical(method, "permute")) {
      fi_out <- tree_permute_fi(
        tree_object = tree_fit, x = x_augmented, y = y, A_full = A_full,
        nodeids = nodeids,
        metric = metric, grouped_features = grouped_features,
        B = permute_idxs, return_preds = TRUE,
        L_full = L_full, solve_cache = solve_cache
      )
      if (is.null(pred_sums)) {
        pred_sums <- lapply(
          fi_out,
          function(.x) {
            lapply(.x, function(.y) numeric(length(.y)))
          }
        )
      }
      for (feature_name in names(fi_out)) {
        if (is.null(pred_sums[[feature_name]])) {
          pred_sums[[feature_name]] <- lapply(
            fi_out[[feature_name]],
            function(.y) numeric(length(.y))
          )
        }
        for (permute_id in seq_along(fi_out[[feature_name]])) {
          pred_sums[[feature_name]][[permute_id]] <-
            pred_sums[[feature_name]][[permute_id]] + fi_out[[feature_name]][[permute_id]]
        }
      }
    } else {
      x_train_means <- tree_fit$preprocessing_info$x_train_means
      fi_out <- tree_local_fi(
        tree_object = tree_fit, x = x_augmented, A_full = A_full,
        nodeids = nodeids,
        x_means = x_train_means[colnames(x_augmented)],
        grouped_features = grouped_features,
        L_full = L_full, solve_cache = solve_cache
      )
      fi_mat <- as.matrix(fi_out)
      if (is.null(fi_sum)) {
        fi_sum <- matrix(
          0, nrow = nrow(fi_mat), ncol = ncol(fi_mat),
          dimnames = dimnames(fi_mat)
        )
      }
      fi_sum <- fi_sum + fi_mat
    }
  }

  if (identical(method, "permute")) {
    orig_score <- metric(truth = y, estimate = orig_preds)
    fi_scores <- purrr::map_dbl(
      pred_sums,
      function(feature_pred_sums) {
        purrr::map_dbl(
          feature_pred_sums,
          function(.x) {
            preds <- .x / ntrees
            orig_score - metric(truth = y, estimate = preds)
          }
        ) |>
          mean()
      }
    ) |>
      tibble::enframe(name = "var", value = "importance")
  } else if (identical(method, "mdi+")) {
    importance <- fi_sums / fi_counts
    importance[fi_counts == 0] <- NaN
    fi_scores <- tibble::tibble(
      var = names(importance),
      importance = unname(importance)
    )
  } else if (identical(method, "local")) {
    fi_scores <- as.data.frame(fi_sum / ntrees, check.names = FALSE)
  }
  return(fi_scores)
}


#' @keywords internal
get_cached_grouped_tree_features <- function(orig_colnames, aug_colnames,
                                             tree_info,
                                             preprocessing_info = NULL) {
  grouped_features <- preprocessing_info$grouped_features
  if (
    !is.null(grouped_features) &&
      identical(preprocessing_info$x_aug_colnames, aug_colnames)
  ) {
    return(grouped_features)
  }
  get_grouped_tree_features(orig_colnames, aug_colnames, tree_info)
}


#' @keywords internal
predict_rnc_for_importance <- function(tree_object, x, A_full = NULL,
                                       nodeids = NULL, L_full = NULL,
                                       solve_cache = NULL) {
  if (identical(tree_object$family, "logistic")) {
    predict_rnc_logistic_with_laplacian(
      tree_object, x, A_full, nodeids, L_full, solve_cache
    )
  } else {
    predict_rnc_linear_with_laplacian(
      tree_object, x, A_full, nodeids, L_full, solve_cache
    )
  }
}


#' @keywords internal
rnc_response_from_eta <- function(eta, family) {
  if (identical(family, "logistic")) {
    1 / (1 + exp(-eta))
  } else {
    eta
  }
}


#' @keywords internal
rnc_eta <- function(tree_object, x, alpha, beta = tree_object$beta) {
  c(tree_object$intercept + alpha + as.matrix(x) %*% beta)
}


#' @keywords internal
rnc_mdiplus_scores_fast <- function(tree_object, x, y, metric, x_means,
                                    grouped_features, alpha, alpha_mean,
                                    beta) {
  x_mat <- as.matrix(x)
  beta <- c(beta)
  x_means <- c(x_means)
  n <- nrow(x_mat)
  x_mean_eta <- sum(x_means * beta)
  out <- lapply(
    grouped_features,
    function(features) {
      alpha_mod <- alpha_mean
      x_delta <- 0
      if (!is.null(features)) {
        if (".alpha" %in% features) {
          alpha_mod <- alpha
        }
        x_features <- setdiff(features, ".alpha")
        keep_idxs <- match(x_features, colnames(x_mat))
        keep_idxs <- keep_idxs[!is.na(keep_idxs)]
        if (length(keep_idxs) > 0) {
          x_delta <- c(
            (x_mat[, keep_idxs, drop = FALSE] -
              matrix(
                x_means[keep_idxs],
                nrow = n, ncol = length(keep_idxs), byrow = TRUE
              )) %*% beta[keep_idxs]
          )
        }
      }
      eta <- tree_object$intercept + alpha_mod + x_mean_eta + x_delta
      if (length(eta) == 1) {
        eta <- rep(eta, n)
      }
      partial_preds <- rnc_response_from_eta(eta, tree_object$family)
      metric(truth = y, estimate = partial_preds)
    }
  )
  tibble::as_tibble(out)
}


#' @keywords internal
rnc_permute_preds_fast <- function(tree_object, x, grouped_features,
                                   permute_idxs, alpha, beta) {
  x_mat <- as.matrix(x)
  beta <- c(beta)
  alpha <- c(alpha)
  eta <- rnc_eta(tree_object, x_mat, alpha = alpha, beta = beta)
  orig_preds <- rnc_response_from_eta(eta, tree_object$family)

  lapply(
    grouped_features,
    function(features) {
      if (is.null(features)) {
        return(lapply(permute_idxs, function(.x) orig_preds))
      }
      has_alpha <- ".alpha" %in% features
      x_features <- setdiff(features, ".alpha")
      keep_idxs <- match(x_features, colnames(x_mat))
      keep_idxs <- keep_idxs[!is.na(keep_idxs)]
      lapply(
        permute_idxs,
        function(permute_idx) {
          eta_mod <- eta
          if (has_alpha) {
            eta_mod <- eta_mod + alpha[permute_idx] - alpha
          }
          if (length(keep_idxs) > 0) {
            eta_mod <- eta_mod + c(
              (x_mat[permute_idx, keep_idxs, drop = FALSE] -
                x_mat[, keep_idxs, drop = FALSE]) %*% beta[keep_idxs]
            )
          }
          rnc_response_from_eta(eta_mod, tree_object$family)
        }
      )
    }
  )
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
                            return_preds = FALSE, L_full = NULL,
                            solve_cache = NULL) {
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

  if ("rnc" %in% class(tree_object)) {
    pred_out <- predict_rnc_for_importance(
      tree_object, x = x, A_full = A_full, nodeids = nodeids,
      L_full = L_full, solve_cache = solve_cache
    )
    orig_preds <- c(pred_out$y)
    alpha <- c(pred_out$alpha)
    beta <- tree_object$beta
  } else {
    orig_preds <- predict_tree(
      tree_object, x = x, A_full = A_full, nodeids = nodeids
    )
    alpha <- NULL
    beta <- NULL
  }

  if ("rnc" %in% class(tree_object)) {
    fi_preds <- rnc_permute_preds_fast(
      tree_object = tree_object,
      x = x,
      grouped_features = grouped_features,
      permute_idxs = permute_idxs,
      alpha = alpha,
      beta = beta
    )
  } else {
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
  }

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
                            metric, x_means = NULL, grouped_features = NULL,
                            L_full = NULL, solve_cache = NULL) {
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
    alpha <- predict_tree(
      tree_object, x = x, A_full = A_full, nodeids = nodeids,
      L_full = L_full, solve_cache = solve_cache, type = "alpha"
    )
    alpha_mean <- mean(tree_object$alpha)
    beta <- tree_object$beta
    return(
      rnc_mdiplus_scores_fast(
        tree_object = tree_object,
        x = x,
        y = y,
        metric = metric,
        x_means = x_means,
        grouped_features = grouped_features,
        alpha = alpha,
        alpha_mean = alpha_mean,
        beta = beta
      )
    )
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
                          x_means = NULL, grouped_features = NULL,
                          L_full = NULL, solve_cache = NULL) {
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
    alpha <- predict_tree(
      tree_object, x = x, A_full = A_full, nodeids = nodeids,
      L_full = L_full, solve_cache = solve_cache, type = "alpha"
    )
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
  embed_orig <- startsWith(orig_colnames, ".embed")
  if (any(embed_orig)) {
    out_colnames <- c(
      orig_colnames[!embed_orig],
      ".embed"
    )
  } else {
    out_colnames <- orig_colnames
  }

  splitvars <- as.character(tree_info$splitvarName)
  node_ids <- tree_info$nodeID
  split_ok <- !is.na(splitvars)
  splitvars <- splitvars[split_ok]
  node_ids <- node_ids[split_ok]

  grouped_features <- vector("list", length(out_colnames))
  names(grouped_features) <- out_colnames
  for (j_idx in seq_along(out_colnames)) {
    j <- out_colnames[[j_idx]]
    if (identical(j, ".embed")) {
      node_match <- startsWith(splitvars, ".embed")
      if (!any(node_match)) {
        grouped_features[j_idx] <- list(NULL)
        next
      }
      xj_features <- aug_colnames[startsWith(aug_colnames, ".embed")]
    } else {
      node_match <- splitvars == j
      if (!any(node_match)) {
        grouped_features[j_idx] <- list(NULL)
        next
      }
      xj_features <- aug_colnames[
        grepl(paste0("^", j, "$"), aug_colnames) |
          grepl(paste0("^", j, "\\.\\..*"), aug_colnames)
      ]
    }
    grouped_features[[j_idx]] <- c(xj_features, paste0(".node", node_ids[node_match]))
  }
  return(grouped_features)
}
