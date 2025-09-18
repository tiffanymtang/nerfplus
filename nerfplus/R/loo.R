#' Compute Leave-One-Out (LOO) predictions and parameter changes for a NeRF+ model
#'
#' @description This function computes leave-one-out (LOO) predictions and parameter changes
#'   for a fitted NeRF+ model. For each sample, it computes the model's predictions and
#'   parameter estimates when that sample is left out of the training data. This is useful
#'   for assessing model stability and identifying influential observations.
#'
#' @param object A fitted NeRF+ model object.
#' @param x A data frame or matrix containing the training data.
#' @param x_embed An optional data frame or matrix of network embeddings corresponding
#'   to the training samples. Only needed if training embeddings were manually inputted.
#' @param y A vector of responses for the training data.
#' @param A An adjacency matrix representing the network structure for the training nodes.
#' @param xtest An optional data frame or matrix containing the test data.
#' @param xtest_embed An optional data frame or matrix of network embeddings corresponding
#'   to the test samples.
#' @param ytest An optional vector of responses for the test data.
#' @param A_full An optional adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order).
#' @param nodeids An optional vector of node IDs of length n. If provided, node IDs
#'   indicate the rows of A corresponding to each sample. If not provided, the rows
#'   of A are assumed to be in the same order as the rows of x and y.
#' @param nodeids_test An optional vector of node IDs for the test data.
#' @param metric An optional function to compute the metric used for evaluating
#'   prediction changes. If NULL, defaults to RMSE for linear models and AUROC
#'   for logistic models.
#' @param return_all Logical indicating whether to return all intermediate results
#'   for each tree in the forest. Defaults to FALSE.
#'
#' @returns A list containing:
#' \item{loo_params}{A list of LOO parameter estimates for each tree.}
#' \item{change_alphas}{Matrix of changes in alpha parameters when each sample is left out.}
#' \item{mean_change_alphas}{Mean change in alpha parameters across all trees.}
#' \item{mean_change_betas}{Mean change in beta parameters across all trees.}
#' \item{loo_preds}{Matrix of LOO predictions, where each column corresponds to
#'   predictions when a different sample is left out.}
#' \item{loo_preds_test}{Matrix of LOO predictions on test data (if provided).}
#' \item{mean_change_error_test}{Mean change in prediction error on test data
#'   across all trees (if provided).}
#' \item{mean_change_preds_test}{Mean change in predictions on test data across
#'   all trees (if provided).}
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
#' loo_out <- get_loo(
#'   nerfplus_out,
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   xtest = example_data$xtest, ytest = example_data$ytest,
#'   A_full = example_data$A_full
#' )
#' }
#'
#' @export
get_loo <- function(object, x, x_embed = NULL, y, A = NULL,
                    xtest = NULL, xtest_embed = NULL, ytest = NULL, A_full = NULL,
                    nodeids = NULL, nodeids_test = NULL,
                    metric = NULL, return_all = FALSE) {
  if (!is.null(nodeids)) {
    if (!identical(nodeids, 1:nrow(A))) {
      stop("Non-default nodeids is not yet implemented.")
    }
  }
  if (!is.null(nodeids_test)) {
    if (!identical(nodeids_test, (nrow(A) + 1):nrow(A_full))) {
      stop("Non-default nodeids_test is not yet implemented.")
    }
  }

  if (object$model_info$family == "linear") {
    metric <- function(truth, estimate, ...) -yardstick::rmse_vec(truth, estimate, ...)
  } else if (object$model_info$family == "logistic") {
    metric <- function(truth, estimate, ...) {
      yardstick::roc_auc_vec(factor(truth, levels = c(1, 0)), estimate, ...)
    }
  }

  rf_fit <- object$rf_fit
  nerfplus_fits <- object$nerfplus_fits
  tree_infos <- object$tree_infos
  pre_rf_preprocessing_info <- object$pre_rf_preprocessing_info
  include_raw <- object$model_info$include_raw
  unordered_factors <- object$unordered_factors
  inbag_counts <- rf_fit$inbag.counts
  sample_split <- object$model_info$sample_split

  n <- nrow(x)
  ntrees <- length(nerfplus_fits)

  yhats <- stats::predict(
    object, x = x, x_embed = x_embed, A_full = A, return_all = TRUE,
    nodeids = nodeids
  )
  # if (!is.null(nodeids)) {
  #   yhats <- stats::predict(
  #     object, x = x, x_embed = x_embed, A_full = A, return_all = TRUE,
  #     nodeids = nodeids
  #   )
  # } else {
  #   yhats <- stats::predict(
  #     object, x = x, x_embed = x_embed, A_full = A, return_all = TRUE,
  #     nodeids = 1:nrow(x)
  #   )
  # }
  if (!is.null(xtest)) {
    yhats_test <- stats::predict(
      object, x = xtest, x_embed = xtest_embed, A_full = A_full,
      return_all = TRUE, nodeids = nodeids_test
    )
  } else {
    yhats_test <- yhats # placeholder for pmap
  }

  x <- apply_pre_rf_preprocessing(
    pre_rf_preprocessing_info,
    x = x, x_embed = x_embed, A_full = A,
    nodeids = nodeids
  )
  x_numeric <- apply_post_rf_preprocessing(rf_fit, x)
  if (!is.null(xtest)) {
    xtest <- apply_pre_rf_preprocessing(
      pre_rf_preprocessing_info,
      x = xtest, x_embed = xtest_embed, A_full = A_full,
      nodeids = nodeids_test
    )
    xtest_numeric <- apply_post_rf_preprocessing(rf_fit, xtest)
  }

  node_preds <- stats::predict(
    rf_fit, x, type = "terminalNodes", num.threads = 1
  )$predictions |>
    as.data.frame()
  if (!is.null(xtest)) {
    node_preds_test <- stats::predict(
      rf_fit, xtest, type = "terminalNodes", num.threads = 1
    )$predictions |>
      as.data.frame()
  } else {
    node_preds_test <- node_preds # placeholder for pmap
  }
  forest_paths <- get_forest_paths(tree_infos)

  loo_out_ls <- purrr::pmap(
    list(
      tree_fit = nerfplus_fits,
      tree_info = tree_infos,
      tree_node_preds = node_preds,
      tree_node_preds_test = node_preds_test,
      tree_paths = forest_paths,
      tree_yhat = yhats,
      tree_yhat_test = yhats_test,
      tree_inbag_counts = inbag_counts
    ),
    function(tree_fit, tree_info, tree_node_preds, tree_node_preds_test,
             tree_paths, tree_yhat, tree_yhat_test, tree_inbag_counts) {
      # get psi matrix
      psi <- apply_psi(
        x = x_numeric,
        tree_info = tree_info,
        tree_paths = tree_paths,
        node_preds = tree_node_preds,
        unordered_factors = unordered_factors,
        psi_unique_values = tree_fit$preprocessing_info$psi_unique_values
      )

      # augment psi matrix with original x features
      x_augmented <- apply_augmentation(
        x = x,
        psi = psi,
        tree_info = tree_info,
        include_raw = include_raw,
        dummy_fit = tree_fit$preprocessing_info$dummy_fit
      )

      # apply to test data if provided
      if (!is.null(xtest)) {
        psi_test <- apply_psi(
          x = xtest_numeric,
          tree_info = tree_info,
          tree_paths = tree_paths,
          node_preds = tree_node_preds_test,
          unordered_factors = unordered_factors,
          psi_unique_values = tree_fit$preprocessing_info$psi_unique_values
        )
        xtest_augmented <- apply_augmentation(
          x = xtest,
          psi = psi_test,
          tree_info = tree_info,
          include_raw = include_raw,
          dummy_fit = tree_fit$preprocessing_info$dummy_fit
        )
      }

      # if no splits in tree, add placeholder/intercept column to augmented x
      if (ncol(x_augmented) == 0) {
        x_augmented <- cbind(
          x_augmented,
          matrix(1, nrow = nrow(x_augmented), ncol = 1)
        )
        if (!is.null(xtest)) {
          xtest_augmented <- cbind(
            xtest_augmented,
            matrix(1, nrow = nrow(xtest_augmented), ncol = 1)
          )
        }
      }

      # subset to inbag or oob samples if specified
      if (sample_split == "inbag") {
        keep_idxs <- tree_inbag_counts > 0
      } else if (sample_split == "oob") {
        keep_idxs <- tree_inbag_counts == 0
      } else {
        keep_idxs <- rep(TRUE, length(tree_inbag_counts))
      }
      x_train <- x_augmented[keep_idxs, , drop = FALSE]
      y_train <- y[keep_idxs]
      tree_yhat <- tree_yhat[keep_idxs]
      if (is.null(nodeids)) {
        A_train <- A[keep_idxs, keep_idxs]
        nodeids_train <- NULL
      } else {
        A_train <- A
        nodeids_train <- nodeids[keep_idxs]
      }

      # TODO: add support for nodeids
      # rows = parameters; columns i = results when sample i was left out
      tree_fit$alpha <- tree_fit$alpha[keep_idxs]
      loo_params <- get_tree_loo_params(
        tree_object = tree_fit,
        x = x_train,
        y = y_train,
        yhat = tree_yhat,
        A = A_train
      )

      # rows = samples; columns i = results when sample i was left out
      if ("rnc" %in% class(tree_fit)) {
        loo_preds <- tree_fit$intercept + loo_params$alpha_loo + x_train %*% loo_params$beta_loo
      } else {
        loo_preds <- add_by_row(
          x_train %*% loo_params$beta_loo, loo_params$alpha_loo
        )
      }
      if (!is.null(xtest)) {
        if ("rnc" %in% class(tree_fit)) {
          D <- diag(rowSums(A_full))
          L <- D - A_full + diag(rep(tree_fit$lambda_l, nrow(A_full)))
          train_idx <- (1:n)[keep_idxs]
          valid_idx <- (n + 1):nrow(A_full)
          L22 <- Matrix::Matrix(L[valid_idx, valid_idx], sparse = TRUE)
          L21 <- L[valid_idx, train_idx]
          alpha_loo <- loo_params$alpha_loo
          diag(alpha_loo) <- 0
          alpha_test <- solve(L22, -L21 %*% alpha_loo, sparse = TRUE)
          loo_preds_test <- tree_fit$intercept + alpha_test + xtest_augmented %*% loo_params$beta_loo
        } else {
          loo_preds_test <- add_by_row(
            xtest_augmented %*% loo_params$beta_loo, loo_params$alpha_loo
          )
        }
      } else {
        loo_preds_test <- NULL
      }

      if ("rnc" %in% class(tree_fit)) {
        alpha <- c(tree_fit$alpha)
        beta <- c(tree_fit$beta)
        theta <- c(alpha, beta)
      } else {
        theta <- as.matrix(stats::coef(tree_fit))
        alpha <- theta[1]
        beta <- theta[-1]
      }
      change_alphas <- abs(loo_params$alpha_loo - alpha)
      diag(change_alphas) <- NA
      change_betas <- abs(loo_params$beta_loo - beta)

      change_error_train <- sapply(
        1:sum(keep_idxs),
        function(i) {
          metric(y[keep_idxs][-i], tree_yhat[-i]) -
            metric(y[keep_idxs][-i], loo_preds[-i, i])
        }
      )
      change_preds_train <- tree_yhat - loo_preds
      diag(change_preds_train) <- NA
      mean_change_preds_train <- colMeans(abs(change_preds_train), na.rm = TRUE)
      if (!is.null(xtest)) {
        change_error_test <- metric(ytest, tree_yhat_test) -
          apply(loo_preds_test, 2, function(.y) metric(ytest, .y))
        change_preds_test <- tree_yhat_test - loo_preds_test
        mean_change_preds_test <- colMeans(abs(change_preds_test))
      } else {
        change_error_test <- NULL
        change_preds_test <- NULL
        mean_change_preds_test <- NULL
      }

      # fill in for samples not used in tree
      alpha_loo <- matrix(NA, nrow = n, ncol = n)
      alpha_loo[keep_idxs, keep_idxs] <- loo_params$alpha_loo
      loo_params$alpha_loo <- alpha_loo
      beta_loo <- matrix(NA, nrow = ncol(x_augmented), ncol = n)
      beta_loo[, keep_idxs] <- loo_params$beta_loo
      loo_params$beta_loo <- beta_loo
      loo_preds_full <- matrix(NA, nrow = n, ncol = n)
      loo_preds_full[keep_idxs, keep_idxs] <- loo_preds
      change_alphas_full <- matrix(NA, nrow = n, ncol = n)
      change_alphas_full[keep_idxs, keep_idxs] <- change_alphas
      change_betas_full <- matrix(NA, nrow = ncol(x_augmented), ncol = n)
      change_betas_full[, keep_idxs] <- change_betas
      change_error_train_full <- rep(NA, n)
      change_error_train_full[keep_idxs] <- change_error_train
      change_preds_train_full <- matrix(NA, nrow = n, ncol = n)
      change_preds_train_full[keep_idxs, keep_idxs] <- change_preds_train
      mean_change_preds_train_full <- rep(NA, n)
      mean_change_preds_train_full[keep_idxs] <- mean_change_preds_train
      if (!is.null(xtest)) {
        loo_preds_test_full <- matrix(NA, nrow = nrow(xtest), ncol = n)
        loo_preds_test_full[, keep_idxs] <- loo_preds_test
        change_error_test_full <- rep(NA, n)
        change_error_test_full[keep_idxs] <- change_error_test
        change_preds_test_full <- matrix(NA, nrow = nrow(xtest), ncol = n)
        change_preds_test_full[, keep_idxs] <- change_preds_test
        mean_change_preds_test_full <- rep(NA, n)
        mean_change_preds_test_full[keep_idxs] <- mean_change_preds_test
      } else {
        loo_preds_test_full <- NULL
        change_error_test_full <- NULL
        change_preds_test_full <- NULL
        mean_change_preds_test_full <- NULL
      }

      loo_out <- list(
        loo_params = loo_params,
        loo_preds = loo_preds_full,
        loo_preds_test = loo_preds_test_full,
        change_alphas = change_alphas_full,
        change_betas = change_betas_full,
        # mean_change_alphas = mean_change_alphas,
        # mean_change_betas = mean_change_betas,
        change_error_train = change_error_train_full,
        change_preds_train = change_preds_train_full,
        mean_change_preds_train = mean_change_preds_train_full,
        change_error_test = change_error_test_full,
        change_preds_test = change_preds_test_full,
        mean_change_preds_test = mean_change_preds_test_full
      )
      return(loo_out)
    }
  )

  loo_params_ls <- purrr::map(loo_out_ls, "loo_params")
  change_alphas <- purrr::map(loo_out_ls, "change_alphas") |>
    abind::abind(along = 3) |>
    apply(1:2, mean, na.rm = TRUE)
  mean_change_alphas <- colMeans(change_alphas, na.rm = TRUE)
  mean_change_betas <- purrr::map(
    loo_out_ls, ~ colMeans(.x$change_betas, na.rm = TRUE)
  ) |>
    abind::abind(along = 2) |>
    apply(1, mean, na.rm = TRUE)

  loo_preds_ls <- purrr::map(loo_out_ls, "loo_preds")
  loo_preds <- abind::abind(loo_preds_ls, along = 3) |>
    apply(1:2, mean, na.rm = TRUE)
  forest_yhat <- abind::abind(yhats, along = 2) |>
    apply(1, mean, na.rm = TRUE)

  if (!is.null(xtest)) {
    loo_preds_test_ls <- purrr::map(loo_out_ls, "loo_preds_test")
    loo_preds_test <- abind::abind(loo_preds_test_ls, along = 3) |>
      apply(1:2, mean, na.rm = TRUE)
    forest_yhat_test <- abind::abind(yhats_test, along = 2) |>
      apply(1, mean, na.rm = TRUE)

    mean_change_error_test <- abs(
      metric(ytest, forest_yhat_test) -
        apply(loo_preds_test, 2, function(.y) metric(ytest, .y))
    )
    change_preds_test <- forest_yhat_test - loo_preds_test
    mean_change_preds_test <- colMeans(abs(change_preds_test))
  } else {
    loo_preds_test <- NULL
    mean_change_error_test <- NULL
    change_preds_test <- NULL
    mean_change_preds_test <- NULL
  }

  out <- list(
    loo_params = loo_params_ls,
    change_alphas = change_alphas,
    mean_change_alphas = mean_change_alphas,
    mean_change_betas = mean_change_betas,
    loo_preds = loo_preds,
    loo_preds_test = loo_preds_test,
    mean_change_error_test = mean_change_error_test,
    mean_change_preds_test = mean_change_preds_test
  )

  if (return_all) {
    out <- c(out, loo_out_ls)
  }
  return(out)
}


#' @keywords internal
get_tree_loo_params <- function(tree_object, x, y, yhat, A = NULL) {
  # TODO: add support for logsitic regression models
  if ("rnc" %in% class(tree_object)) {
    n <- nrow(x)
    p <- ncol(x)
    xtil <- cbind(diag(n), x)

    alpha <- tree_object$alpha
    beta <- tree_object$beta
    theta <- c(alpha, beta)
    lambda_l <- tree_object$lambda_l
    lambda_netcoh <- tree_object$lambda_netcoh / (nrow(x)^2)
    lambda_x <- tree_object$lambda_x / ncol(x)

    L <- diag(rowSums(A)) - A + diag(rep(lambda_l, n))
    B <- matrix(0, nrow = n + p, ncol = n + p)
    B[1:n, 1:n] <- diag(n) + lambda_netcoh * L
    B[1:n, (n + 1):(n + p)] <- x
    B[(n + 1):(n + p), 1:n] <- t(x)
    B[(n + 1):(n + p), (n + 1):(n + p)] <- t(x) %*% x + diag(lambda_x)
    Binv <- solve(B)

    Binvk_xk <- sapply(
      1:n, function(k) sum(Binv[, k] * xtil[k, ])
    )
    Binvk_x <- sapply(
      1:n, function(k) Binv[, k] * Binvk_xk[k] / Binv[k, k]
    )
    Binv_x <- sapply(
      1:n, function(k) Binv %*% xtil[k, ]
    )
    Bkinv_x <- Binv_x - Binvk_x

    h <- rowSums(xtil * t(Bkinv_x))
    resid <- c(yhat - y)

    term1 <- multiply_by_row(Bkinv_x, resid / (1 - h))
    term2b <- multiply_by_row(Bkinv_x, Binvk_xk / (1 - h))
    term2 <- multiply_by_row(Binv[, 1:n] + term2b, alpha / diag(Binv)[1:n])
    theta_loo <- c(theta) + term1 - term2
    diag(theta_loo) <- 0
    diag(theta_loo) <- -rowSums(L * t(theta_loo[1:n, ])) / diag(L)
    # # checking equivalence
    # theta_loo_true <- matrix(0, nrow = n, ncol = n + p)
    # for (k in 1:n) {
    #   Bkinv <- Binv - 1 / Binv[k, k] * outer(Binv[, k], Binv[, k])
    #   hk <- c(t(xtil[k, ]) %*% Bkinv %*% xtil[k, ])
    #   yhatk <- yhat[k]
    #   yk <- y[k]
    #   term1 <- theta
    #   term2 <- Bkinv %*% xtil[k, ] / (1 - hk) * (yhatk - yk)
    #   term3 <- alpha[k] / Binv[k, k] * (
    #     Binv[, k] + Bkinv %*% xtil[k, ] %*% t(xtil[k, ]) %*% Binv[, k] / (1 - hk)
    #   )
    #   theta_loo_true[k, ] <- term1 + term2 - term3
    #   theta_loo_true[k, k] <- -1 / L[k, k] *
    #     sum(L[k, -k] * theta_loo_true[k, 1:n][-k])
    # }
    # all.equal(t(theta_loo_true), theta_loo)

    alpha_loo <- theta_loo[1:n, , drop = FALSE]
    beta_loo <- theta_loo[(n + 1):(n + p), , drop = FALSE]
  } else {
    xtil <- cbind(1, x)
    p <- ncol(xtil)
    if (any(c("glmnet", "cv.glmnet") %in% class(tree_object))) {
      lambda <- tree_object$lambda
    } else {
      lambda <- 0
    }
    theta <- as.matrix(stats::coef(tree_object))
    Binv_x <- solve(t(xtil) %*% xtil + lambda * diag(p), t(xtil))
    h <- rowSums(xtil * t(Binv_x))
    resid <- c(yhat - y)
    theta_loo <- c(theta) + multiply_by_row(Binv_x, resid / (1 - h))
    # # checking equivalence
    # n <- nrow(xtil)
    # theta_loo_true <- matrix(0, nrow = n, ncol = p)
    # for (k in 1:n) {
    #   Binv <- solve(t(xtil) %*% xtil + lambda * diag(p))
    #   hk <- c(t(xtil[k, ]) %*% Binv %*% xtil[k, ])
    #   yhatk <- yhat[k]
    #   yk <- y[k]
    #   theta_loo_true[k, ] <- theta + Binv %*% xtil[k, ] / (1 - hk) * (yhatk - yk)
    # }
    # all.equal(t(theta_loo_true), theta_loo)

    alpha_loo <- theta_loo[1, , drop = FALSE]
    beta_loo <- theta_loo[-1, , drop = FALSE]
  }
  return(list(alpha_loo = alpha_loo, beta_loo = beta_loo))
}
