#' Shared arguments for NeRF+ utility functions
#'
#' @name shared_args_util
#' @param tree_info Output of [ranger::treeInfo()] for a single tree.
#' @param tree_infos List of size ntrees with each entry being the output of
#'   [ranger::treeInfo()].
#' @param tree_paths List of size ntrees with paths for each tree; typically the
#'   output of [get_forest_paths()].
#' @param node_preds Matrix of terminal node predictions for each tree in the
#'   random forest; typically, output from
#'   `predict(rf_fit, x, type = "terminalNodes")$predictions`, where
#'   `rf_fit` is a fitted `ranger` object and `x` is the data frame or matrix.
#' @param unordered_factors Vector of column names corresponding to unordered
#'   factor variables in the data.
#' @param inbag_counts List of size ntrees with inbag counts for each tree;
#'   typically the output from `rf_fit$inbag.counts`, where `rf_fit` is a
#'   fitted `ranger` object. Ignored if `normalize = FALSE`.
#' @param include_raw Logical indicating whether to include the raw covariates
#'   in the NeRF+ model. Default is `TRUE`.
#'
#' @keywords internal
NULL

#' Preprocessing helper functions prior to fitting the RF in NeRF+ models
#'
#' @name nerfplus_preprocessing
#' @description These functions fit or apply preprocessing steps prior to
#'   fitting RF in NeRF+ models. Specifically, this function standardizes the
#'   numeric features to have mean 0 and SD 1 (if `standardize = TRUE`) and
#'   augments the data with (standardized) network embeddings (if `embedding`
#'   is specified).
#'
#' @inheritParams nerfplus
#' @param standardize Logical indicating whether to standardize numeric
#'   features in `x` to have mean 0 and SD 1. Defaults to `TRUE`.
#' @param preprocess_fit Output of `fit_pre_rf_preprocessing()` to be applied to
#'   new data.
#' @param x_embed Optional embedding data frame or matrix, whose rows are
#'   aligned with those in `x`. If provided, it will be used to augment the
#'   input `x` data. Only needed if training embeddings were manually inputted.
#' @param A_full An adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order).
#'
#' @returns For `fit_pre_rf_preprocessing()`, a list containing the following
#'   components:
#' - `x`: The preprocessed data frame
#' - `standardize_x`: Logical indicating whether the raw data was standardized
#' - `x_center_factors`: Named numeric vector of means used for standardization
#' - `x_scale_factors`: Named numeric vector of standard deviations used for
#'   standardization
#' - `embedding`: The method used for embedding
#' - `embedding_fit`: The output of the network embedding fit
#' - `embed_center_factors`: Named numeric vector of means used for
#'   standardizing the embeddings
#' - `embed_scale_factors`: Named numeric vector of standard deviations used for
#'   standardizing the embeddings
#' - `nodeids`: The node IDs provided for the embeddings.
#'
#' For `apply_pre_rf_preprocessing()`, a data frame with the preprocessed data.
#'
#' @keywords internal
fit_pre_rf_preprocessing <- function(x, A = NULL,
                                     standardize = TRUE,
                                     embedding = NULL,
                                     embedding_options = list(
                                       ndim = 2,
                                       regularization = 0.5,
                                       varimax = FALSE,
                                       center = FALSE,
                                       scale = FALSE
                                     ),
                                     nodeids = NULL) {
  x_center_factors <- NULL
  x_scale_factors <- NULL
  if (standardize) {
    numeric_colnames <- names(which(sapply(x, is.numeric)))
    if (length(numeric_colnames) > 0) {
      x_cont <- scale(x[, numeric_colnames, drop = FALSE])
      x_center_factors <- attr(x_cont, "scaled:center")
      x_scale_factors <- attr(x_cont, "scaled:scale")
      x[, numeric_colnames] <- x_cont
    }
  }
  embedding_fit <- NULL
  x_embed <- NULL
  embedding_method <- NULL
  embed_center_factors <- NULL
  embed_scale_factors <- NULL
  if (!is.null(embedding)) {
    if (is.character(embedding)) {
      embedding_fit <- fit_network_embedding(
        A,
        ndim = embedding_options$ndim,
        method = embedding,
        regularization = embedding_options$regularization,
        varimax_rotation = embedding_options$varimax
      )
      x_embed <- do.call(cbind, embedding_fit$X)
      if (!is.null(nodeids)) {
        x_embed <- x_embed[nodeids, , drop = FALSE]
      }
      embedding_method <- embedding_fit$method
      embed_colnames <- colnames(x_embed)
    } else {
      x_embed <- embedding
      embedding_method <- "manual"
      embed_colnames <- paste0(".embed", 1:ncol(x_embed))
    }
    if (embedding_options$scale) {
      # scale so that first embedding component has SD 1; all others are
      # scaled relative to their eigenvalues
      x_embed <- scale(
        x_embed,
        center = embedding_options$center,
        scale = stats::sd(x_embed[, 1])^2 / apply(x_embed, 2, stats::sd)
      )
    } else {
      x_embed <- scale(
        x_embed,
        center = embedding_options$center,
        scale = FALSE
      )
    }
    embed_center_factors <- attr(x_embed, "scaled:center")
    embed_scale_factors <- attr(x_embed, "scaled:scale")
    colnames(x_embed) <- embed_colnames
    x <- cbind(x, x_embed)
  }
  if (is.matrix(x)) {
    x <- data.frame(x, check.names = FALSE)
  }
  return(list(
    x = x,
    standardize_x = standardize,
    x_center_factors = x_center_factors,
    x_scale_factors = x_scale_factors,
    embedding = embedding_method,
    embedding_fit = embedding_fit,
    embed_center_factors = embed_center_factors,
    embed_scale_factors = embed_scale_factors,
    nodeids = nodeids
  ))
}


#' @rdname nerfplus_preprocessing
#' @keywords internal
apply_pre_rf_preprocessing <- function(preprocess_fit,
                                       x, x_embed = NULL, A_full,
                                       nodeids = NULL) {
  if (preprocess_fit$standardize_x) {
    numeric_colnames <- names(preprocess_fit$x_center_factors)
    if (length(numeric_colnames) > 0) {
      x_cont <- scale(
        x[, numeric_colnames, drop = FALSE],
        center = preprocess_fit$x_center_factors,
        scale = preprocess_fit$x_scale_factors
      )
      x[, numeric_colnames] <- x_cont
    }
  }
  if (!is.null(preprocess_fit$embedding)) {
    if (identical(preprocess_fit$embedding, "manual")) {
      if (is.null(x_embed)) {
        stop("x_embed must be provided for a user-provided training embedding.")
      }
      embed_colnames <- paste0(".embed", 1:ncol(x_embed))
    } else {
      n_test <- nrow(A_full) - nrow(preprocess_fit$embedding_fit$X[[1]])
      n_train <- nrow(preprocess_fit$embedding_fit$X[[1]])
      if (n_test == 0) { # assuming that the training data was inputted
        x_embed <- preprocess_fit$embedding_fit$X
        x_embed <- do.call(cbind, x_embed)
        if (!is.null(nodeids)) {
          x_embed <- x_embed[nodeids, , drop = FALSE]
        }
      } else {
        x_embed <- oos_network_embedding(
          embedding_fit = preprocess_fit$embedding_fit,
          A = A_full[1:n_train, 1:n_train],
          A_full = A_full
        )$X
        x_embed <- do.call(cbind, x_embed)
        if (!is.null(nodeids)) {
          x_embed_train <- preprocess_fit$embedding_fit$X
          x_embed_train <- do.call(cbind, x_embed_train)
          x_embed <- rbind(x_embed_train, x_embed)[nodeids, , drop = FALSE]
        }
      }
      embed_colnames <- colnames(x_embed)
    }
    if (is.null(preprocess_fit$embed_center_factors)) {
      embed_center_factors <- FALSE
    } else {
      embed_center_factors <- preprocess_fit$embed_center_factors
    }
    if (is.null(preprocess_fit$embed_scale_factors)) {
      embed_scale_factors <- FALSE
    } else {
      embed_scale_factors <- preprocess_fit$embed_scale_factors
    }
    x_embed <- scale(
      x_embed, center = embed_center_factors, scale = embed_scale_factors
    )
    colnames(x_embed) <- embed_colnames
    x <- cbind(x, x_embed)
  }
  if (is.matrix(x)) {
    x <- data.frame(x, check.names = FALSE)
  }
  return(x)
}


#' Postprocessing helper functions after fitting the RF in NeRF+ models
#'
#' @description This function applies some processing steps to the data after
#'   fitting the RF in NeRF+ models. Specifically, it converts factor levels
#'   to numeric values based on the levels stored in the RF model.
#'
#' @param object A `ranger` object that has been fitted to the data.
#' @param x A data frame or matrix containing the data to be processed.
#'
#' @returns A data frame or matrix with the same structure as `x`, but with
#'   factor levels converted to numeric values based on the levels stored in
#'   `object`.
#'
#' @keywords internal
apply_post_rf_preprocessing <- function(object, x) {
  factor_levels <- purrr::compact(object$forest$covariate.levels)
  if (length(factor_levels) > 0) {
    x <- x |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(names(factor_levels)),
          ~ as.numeric(
            factor(
              as.character(.x),
              levels = factor_levels[[as.character(dplyr::cur_column())]]
            )
          )
        )
      )
  }
  return(x)
}


#' Extract all root-to-leaf paths in a forest.
#'
#' @inheritParams shared_args_util
#'
#' @returns A list of size ntrees with each entry being a list of decision
#'   paths in each tree.
#'
#' @keywords internal
get_forest_paths <- function(tree_infos) {
  purrr::map(tree_infos, ~get_tree_paths(.x))
}


#' Extract all root-to-leaf paths in a tree.
#'
#' @inheritParams shared_args_util
#'
#' @returns A list of decision paths in a single tree.
#' @keywords internal
get_tree_paths <- function(tree_info) {
  terminal_node_ids <- tree_info$nodeID[tree_info$terminal]
  inner_tree_info <- tree_info[!tree_info$terminal, ]
  tree_paths <- get_tree_paths_cpp(
    terminal_node_ids = terminal_node_ids,
    left_child_ids = inner_tree_info$leftChild,
    right_child_ids = inner_tree_info$rightChild,
    node_ids = inner_tree_info$nodeID
  ) |>
    stats::setNames(as.character(terminal_node_ids))
  return(tree_paths)
}


#' @keywords internal
get_unnormalized_psi <- function(x, tree_info, tree_paths, node_preds,
                                 unordered_factors = NULL) {
  # to avoid no visible binding note
  terminal <- NULL

  x_mat <- as.matrix(x)
  x_colnames_df <- tibble::tibble(
    name = colnames(x),
    idx = 0:(length(colnames(x)) - 1)
  )

  inner_tree_info <- tree_info |>
    dplyr::filter(!terminal) |>
    dplyr::left_join(x_colnames_df, by = c("splitvarName" = "name"))

  if (length(unordered_factors) == 0) {
    psi <- extract_psi_cpp(
      x = x_mat,
      node_preds = node_preds,
      tree_paths = tree_paths,
      node_ids = inner_tree_info$nodeID,
      split_vars = inner_tree_info$idx,
      split_vals = inner_tree_info$splitval
    )
  } else {
    psi <- extract_psi_chr_cpp(
      x = x_mat,
      node_preds = node_preds,
      tree_paths = tree_paths,
      node_ids = inner_tree_info$nodeID,
      split_vars = inner_tree_info$idx,
      split_vals = inner_tree_info$splitval,
      unordered_factors = as.integer(
        which(colnames(x) %in% unordered_factors) - 1
      )
    )
  }

  psi <- psi |>
    as.data.frame() |>
    tibble::as_tibble() |>
    stats::setNames(paste0(".node", inner_tree_info$nodeID))

  return(psi)
}


#' Get the Psi matrix in NeRF+ containing decision stump features
#'
#' @name nerfplus_psi
#' @description These helper functions fit or apply a previously fitted
#'   Psi mapping (i.e., decision stump feature mapping) to data.
#'
#' @inheritParams shared_args_util
#' @param x A data frame or matrix containing the data to feed through decision
#'   tree to obtain the Psi (i.e., decision stump features) matrix.
#' @param normalize Logical indicating whether to normalize the Psi matrix by
#'   number of training samples in each child node. Defaults to `FALSE`.
#' @param psi_unique_values A named list of unique values for each Psi feature,
#'   typically obtained from the output of `fit_psi()`.
#'
#' @returns For `fit_psi()`, a list of two:
#' - `psi`: A data frame containing the Psi matrix, where each column
#'   corresponds to a decision stump feature
#' - `psi_unique_values`: A named list of unique values for each Psi feature
#'   (only returned if `normalize = TRUE`).
#' For `apply_psi()`, a data frame containing the Psi matrix, where each
#'   column corresponds to a decision stump feature.
#'
#' @keywords internal
fit_psi <- function(x, tree_info, tree_paths, node_preds,
                    unordered_factors = NULL,
                    normalize = FALSE, inbag_counts = NULL) {

  psi <- get_unnormalized_psi(
    x = x,
    tree_info = tree_info,
    tree_paths = tree_paths,
    node_preds = node_preds,
    unordered_factors = unordered_factors
  )

  psi_unique_values <- NULL
  if (normalize) {
    if (is.null(inbag_counts)) {
      inbag_counts <- rep(1, nrow(psi))
    }
    psi_unique_values <- purrr::map(
      psi,
      ~ c(
        -sqrt(sum((.x == 1) * inbag_counts) / sum((.x == -1) * inbag_counts)),
        sqrt(sum((.x == -1) * inbag_counts) / sum((.x == 1) * inbag_counts))
      )
    )
    psi <- purrr::map2(
      .x = psi, .y = psi_unique_values,
      ~ dplyr::case_when(
        .x == -1 ~ .y[1],
        .x == 1 ~ .y[2],
        TRUE ~ 0
      )
    ) |>
      as.data.frame()
  }

  return(list(psi = psi, psi_unique_values = psi_unique_values))
}


#' @rdname nerfplus_psi
#' @keywords internal
apply_psi <- function(x, tree_info, tree_paths, node_preds,
                      unordered_factors = NULL, psi_unique_values = NULL) {

  psi <- get_unnormalized_psi(
    x = x,
    tree_info = tree_info,
    tree_paths = tree_paths,
    node_preds = node_preds,
    unordered_factors = unordered_factors
  )

  if (!is.null(psi_unique_values)) {
    psi <- purrr::map2(
      .x = psi, .y = psi_unique_values[colnames(psi)],
      ~ dplyr::case_when(
        .x == -1 ~ .y[1],
        .x == 1 ~ .y[2],
        TRUE ~ 0
      )
    ) |>
      as.data.frame()
  }

  return(psi)
}


#' Augment Psi matrix in NeRF+ with raw features
#'
#' @name nerfplus_augmentation
#' @description These helper functions augment the Psi matrix in NeRF+ with raw
#'   features. Moreover, the raw features are dummy-coded if they are
#'   categorical.
#'
#' @inheritParams shared_args_util
#' @param x A data frame or matrix containing the raw features.
#' @param psi A data frame or matrix containing the Psi matrix, where each
#'   column corresponds to a decision stump feature.
#' @param dummy_fit A previously fitted dummy coding model; typically the output
#'   of `fit_dummy_code()`. If `include_raw = TRUE`, this is used to dummy-code
#'   raw features.
#'
#' @returns For `fit_augmentation()`, a list containing:
#' - `x`: A matrix containing the augmented data, which includes both the raw
#'   features (if `include_raw = TRUE`) and the Psi features.
#' - `dummy_fit`: The fitted dummy coding model, which dummy-codes categorical
#'   features.
#' For `apply_augmentation()`, a matrix containing the augmented data,
#'   which includes both the raw features (if `include_raw = TRUE`) and the Psi
#'   features. If `include_raw = FALSE`, only the Psi features are returned.
#'
#' @keywords internal
fit_augmentation <- function(x, psi, tree_info = NULL, include_raw = TRUE) {
  # to avoid no visible binding note
  splitvarName <- NULL

  dummy_fit <- NULL
  if (include_raw) {
    splitvars <- tree_info |>
      dplyr::filter(!is.na(splitvarName)) |>
      dplyr::pull(splitvarName) |>
      unique()
    x_splitvar <- x[, splitvars, drop = FALSE]
    dummy_out <- fit_dummy_code(x_splitvar)
    dummy_fit <- dummy_out$dummy_fit
    x_splitvar <- dummy_out$x
    x_aug <- as.matrix(cbind(x_splitvar, psi))
  } else {
    x_aug <- as.matrix(psi)
  }
  out <- list(
    x = x_aug,
    dummy_fit = dummy_fit
  )
  return(out)
}


#' @rdname nerfplus_augmentation
#' @keywords internal
apply_augmentation <- function(x, psi, tree_info = NULL, include_raw = TRUE,
                               dummy_fit = NULL) {
  # placeholder to avoid no visible binding note
  splitvarName <- NULL

  if (include_raw) {
    splitvars <- tree_info |>
      dplyr::filter(!is.na(splitvarName)) |>
      dplyr::pull(splitvarName) |>
      unique()
    x_splitvar <- x[, splitvars, drop = FALSE]
    x_splitvar <- apply_dummy_code(dummy_fit, x_splitvar)
    x_aug <- as.matrix(cbind(x_splitvar, psi))
  } else {
    x_aug <- as.matrix(psi)
  }
  return(x_aug)
}


#' Dummy code categorical features
#'
#' @name dummy_code
#' @param x A data frame or matrix containing the data to be dummy-coded.
#' @param dummy_fit A previously fitted dummy coding model; typically the
#'   output of `fit_dummy_code()`.
#'
#' @returns For `fit_dummy_code()`, a list containing:
#' - `dummy_fit`: The fitted dummy coding model, which can be used to
#'   dummy-code new data.
#' - `x`: A data frame or matrix containing the dummy-coded data.
#' For `apply_dummy_code()`, a data frame or matrix containing the
#'   dummy-coded data.
#'
#' @keywords internal
fit_dummy_code <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  dummy_var_transform <- NULL
  if (any(sapply(x, class) %in% c("factor", "character"))) {
    dummy_var_transform <- caret::dummyVars(
      ~ ., data = x, fullRank = TRUE, sep = ".."
    )
    x <- stats::predict(dummy_var_transform, x)
  }
  return(list(dummy_fit = dummy_var_transform, x = x))
}


#' @rdname dummy_code
#' @keywords internal
apply_dummy_code <- function(dummy_fit, x) {
  if (!is.null(dummy_fit)) {
    x <- stats::predict(dummy_fit, x)
  }
  return(x)
}


#' @keywords internal
get_lambda_x <- function(lambda_embed, lambda_raw, lambda_stump, x, psi,
                         include_raw = TRUE, scale = TRUE) {
  # if (scale) {
  #   n_embed <- sum(stringr::str_starts(colnames(x), ".embed"))
  #   lambda_embed <- lambda_embed / n_embed
  #   lambda_stump <- lambda_stump / ncol(psi)
  #   if (include_raw) {
  #     lambda_raw <- lambda_raw / (ncol(x) - ncol(psi))
  #   }
  # }
  if (include_raw) {
    embed_idxs <- which(stringr::str_starts(colnames(x), ".embed"))
    lambda_x <- rep(lambda_raw, length.out = ncol(x) - ncol(psi))
    lambda_x[embed_idxs] <- lambda_embed
    lambda_x <- c(lambda_x, rep(lambda_stump, length.out = ncol(psi)))
  } else {
    lambda_x <- rep(lambda_stump, length.out = ncol(psi))
  }
  return(lambda_x)
}


#' @keywords internal
invert_lambda_x <- function(data, x) {
  # to avoid no visible binding note
  lambda_x <- NULL

  embed_idxs <- stringr::str_starts(colnames(x), ".embed")
  data <- data |>
    dplyr::mutate(
      lambda_embed = purrr::map_dbl(lambda_x, ~ .x[embed_idxs][1]),
      lambda_raw = purrr::map_dbl(lambda_x, ~ .x[!embed_idxs][1]),
      lambda_stump = purrr::map_dbl(lambda_x, ~ .x[length(.x)])
    ) |>
    dplyr::select(-lambda_x)
  return(data)
}


#' @keywords internal
invert_penalty_factor <- function(data, x) {
  # to avoid no visible binding note
  penalty_factor <- NULL

  embed_idxs <- stringr::str_starts(colnames(x), ".embed")
  # train_idxs <- 1:nrow(x)
  data <- data |>
    dplyr::mutate(
      netcoh = purrr::map_dbl(penalty_factor, ~ .x[1]),
      embed = purrr::map_dbl(penalty_factor, ~ .x[-1][embed_idxs][1]),
      raw = purrr::map_dbl(penalty_factor, ~ .x[-1][!embed_idxs][1]),
      stump = purrr::map_dbl(penalty_factor, ~ .x[length(.x)])
    )
  return(data)
}


#' @keywords internal
glmnet_wrapper <- function(x, y, family, cv = FALSE, ...) {
  if (ncol(x) == 1) {
    train_df <- dplyr::bind_cols(x, .y = y)
    if (family == "binomial") {
      fit <- stats::glm(.y ~ ., data = train_df, family = "binomial")
    } else if (family == "gaussian") {
      fit <- stats::lm(.y ~ ., data = train_df)
    }
  } else {
    if (cv) {
      fit <- glmnet::cv.glmnet(x = x, y = y, family = family, ...)
    } else {
      fit <- glmnet::glmnet(x = x, y = y, family = family, ...)
    }
  }
  return(fit)
}


#' Evaluate r-squared with NA handling
#'
#' @description This function computes the R-squared value and returns 0
#'   (instead of NA) if the estimate is a constant vector.
#'
#' @param truth A numeric vector of true values.
#' @param estimate A numeric vector of estimated values.
#' @param ... Additional arguments passed to [yardstick::rsq_vec()]
#'
#' @returns A numeric value representing the R-squared value, or 0 if the
#'   estimate is a constant vector.
#'
#' @keywords internal
rsq_narm_vec <- function(truth, estimate, ...) {
  if (length(unique(estimate)) == 1) {
    return(0)
  } else {
    return(yardstick::rsq_vec(truth = truth, estimate = estimate, ...))
  }
}


#' @keywords internal
add_by_row <- function(mat, vec) {
  sweep(mat, 2, vec, "+")
}


#' @keywords internal
multiply_by_row <- function(mat, vec) {
  sweep(mat, 2, vec, "*")
}
