#' Fit regression with network cohesion with cross-validation wrapper
#'
#' @description This function fits a regression model with network cohesion
#'   regularization with a cross-validation wrapper. This code has been adapted
#'   from the `netcoh` R package with minor tweaks for increased speed and
#'   flexibility. Currently, it can handle linear and logistic regression only.
#'
#' @inheritParams shared_args
#' @param lambdas_x Vector or list of regularization parameters for the (linear)
#'   covariates. Default is 0 (i.e., no regularization).
#' @param lambda_grid (Optional) A data frame with columns `lambda_netcoh`,
#'   `lambda_x`, and `lambda_l`, where each row specifies a different set of
#'   regularization parameters to try. If `NULL`, a grid of regularization
#'   parameters will be generated based upon all possible combination of the
#'   provided `lambdas_netcoh`, `lambdas_x`, and `lambdas_l`.
#'
#' @returns A list with the following components:
#' - `lambda_grid`: A data frame with the regularization parameters used in the
#'   cross-validation.
#' - `best_params`: A data frame with the best regularization parameters
#'   found during cross-validation.
#' - `cv_errs`: A matrix of cross-validation errors, where each row corresponds
#'   to a set of regularization parameters and each column corresponds to a
#'   fold. If regression, the RMSE is returned. If classification, the AUROC is
#'   returned.
#' - `cv_means`: A vector of mean cross-validation errors for each set of
#'   regularization parameters.
#' - `cv_sds`: A vector of standard deviations of cross-validation errors for
#'   each set of regularization parameters.
#' - Other components from the fitted model if `refit = TRUE` (see output of
#'   `rnc()`).
#'
#' @examples
#' data(example_data)
#'
#' # fit RNC with CV
#' cv_fit <- rnc_cv(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambdas_netcoh = c(0, 0.1, 1),
#'   lambdas_x = c(0.1, 0.5)
#' )
#'
#' # fit RNC with CV, specifying lambda grid manually (equivalent to above)
#' lambda_grid <- expand.grid(
#'   lambda_netcoh = c(0, 0.1, 1),
#'   lambda_x = c(0.1, 0.5),
#'   lambda_l = 0.05
#' )
#' cv_fit <- rnc_cv(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_grid = lambda_grid
#' )
#'
#' @export
rnc_cv <- function(x, y, A, nodeids = NULL,
                   lambdas_netcoh, lambdas_x = 0, lambdas_l = 0.05,
                   lambda_grid = NULL,
                   family = c("linear", "logistic"),
                   cv = 5, cv_foldids = NULL, refit = TRUE,
                   newton_maxit = 50, newton_tol = 1e-4, verbose = FALSE) {
  family <- match.arg(family)
  n <- nrow(x)
  if (!isTRUE(all.equal(A, t(A)))) {
    warning("A is not symmetric....")
  }
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!isTRUE(length(dim(y)) == 2)) {
    y <- matrix(y, ncol = 1)
  }
  if ((nrow(y) != n) || (nrow(x) != n)) {
    stop("Dimensions do not match!")
  }

  # set up cv
  if (is.null(cv_foldids)) {
    if (cv == -1) {
      K <- n
      cv_foldids <- 1:K
    } else {
      K <- cv
      cv_foldids <- sample(rep(1:K, n), n)
      cv_foldids <- purrr::map(1:K, ~ which(cv_foldids == .x))
    }
  } else {
    K <- length(cv_foldids)
  }

  # set up hyperparameters
  if (is.null(lambda_grid)) {
    if (!is.list(lambdas_x)) {
      lambdas_x <- purrr::map(lambdas_x, ~ rep(.x, ncol(x)))
    }
    lambda_grid <- tidyr::expand_grid(
      lambda_netcoh = lambdas_netcoh,
      lambda_x = lambdas_x,
      lambda_l = lambdas_l
    )
  } else {
    if (!is.list(lambda_grid$lambda_x)) {
      lambda_grid$lambda_x <- purrr::map(
        lambda_grid$lambda_x, ~ rep(.x, ncol(x))
      )
    }
  }

  # run CV
  n_params <- nrow(lambda_grid)
  cv_errs <- matrix(0, nrow = n_params, ncol = K)
  L_unreg <- diag(rowSums(A)) - A
  for (k in 1:K) {
    valid_idx <- sort(cv_foldids[[k]])
    train_idx <- setdiff(1:n, valid_idx)
    if (is.null(nodeids)) {
      L <- L_unreg[train_idx, train_idx]
      L22 <- L_unreg[valid_idx, valid_idx]
      L21 <- L_unreg[valid_idx, train_idx]
    } else {
      L <- L_unreg
      L22 <- NULL
      L21 <- NULL
    }
    if (identical(family, "linear")) {
      cv_fit <- rnc_linear_path(
        x = x[train_idx, , drop = FALSE],
        y = y[train_idx, , drop = FALSE],
        L = L,
        x_test = x[valid_idx, , drop = FALSE],
        y_test = y[valid_idx, , drop = FALSE],
        L22 = L22,
        L21 = L21,
        nodeids = nodeids[train_idx],
        nodeids_test = nodeids[valid_idx],
        lambda_grid = lambda_grid
      )
    } else if (identical(family, "logistic")) {
      if (!is.null(nodeids)) {
        stop("nodeids not yet implemented for logistic regression.")
      }
      cv_fit <- rnc_logistic_path(
        x = x[train_idx, , drop = FALSE],
        y = y[train_idx, , drop = FALSE],
        A = A[train_idx, train_idx],
        x_test = x[valid_idx, , drop = FALSE],
        y_test = y[valid_idx, , drop = FALSE],
        A_full = A[c(train_idx, valid_idx), c(train_idx, valid_idx)],
        nodeids = nodeids[train_idx],
        nodeids_test = nodeids[valid_idx],
        lambda_grid = lambda_grid,
        newton_maxit = newton_maxit, newton_tol = newton_tol,
      )
    }
    cv_errs[, k] <- cv_fit$cv_errs
  }

  cv_means <- rowMeans(cv_errs)
  cv_sds <- apply(cv_errs, 1, stats::sd)
  if (identical(family, "linear")) {
    # optimizing mse
    best_param_idx <- which(cv_means == min(cv_means, na.rm = TRUE))[1]
  } else if (identical(family, "logistic")) {
    # optimizing auroc or bernoulli log-likelihood
    best_param_idx <- which(cv_means == max(cv_means, na.rm = TRUE))[1]
  }
  best_params <- cv_fit$lambda_grid[best_param_idx, ]
  best_lambda_netcoh <- best_params$lambda_netcoh[[1]]
  best_lambda_x <- best_params$lambda_x[[1]]
  best_lambda_l <- best_params$lambda_l[[1]]

  if (refit) {
    out <- rnc(
      x = x, y = y, A = A, nodeids = nodeids,
      lambda_netcoh = best_lambda_netcoh,
      lambda_x = best_lambda_x,
      lambda_l = best_lambda_l,
      family = family,
      newton_maxit = newton_maxit, newton_tol = newton_tol
    )
  } else {
    out <- list()
  }

  out[["lambda_grid"]] <- cv_fit$lambda_grid
  out[["best_params"]] <- best_params
  out[["cv_errs"]] <- cv_errs
  out[["cv_means"]] <- cv_means
  out[["cv_sds"]] <- cv_sds
  return(out)
}


#' Fit regularization path for linear regression with network cohesion
#' @keywords internal
rnc_linear_path <- function(x, y, L, x_test = NULL, y_test = NULL,
                            L22 = NULL, L21 = NULL, lambda_grid,
                            nodeids = NULL, nodeids_test = NULL, ...) {
  # to avoid no visible binding note
  lambda_x <- NULL

  n <- nrow(x)
  p <- ncol(x)
  lambda_grid <- lambda_grid |>
    dplyr::group_by(lambda_netcoh, lambda_l) |>
    dplyr::summarise(
      lambda_x = list(purrr::map(lambda_x, ~ .x)),
      .groups = "keep"
    )

  # center y
  y_mean <- mean(y)
  y <- y - y_mean
  y_test <- y_test - y_mean

  results <- list()
  for (i in 1:nrow(lambda_grid)) {
    lambda_netcoh <- lambda_grid$lambda_netcoh[[i]] / (n^2)
    lambda_l <- lambda_grid$lambda_l[[i]]
    lambdas_x <- purrr::map(lambda_grid$lambda_x[[i]], ~ .x / p)

    L_reg <- L + diag(rep(lambda_l, nrow(L)))
    if (is.null(nodeids)) {
      if (is.null(x_test)) {
        results[[i]] <- rnc_solver_path(
          X = x, Y = y, L = L_reg,
          lambda_netcoh = lambda_netcoh, lambdas_x = lambdas_x
        )
      } else {
        L22_reg <- L22 + diag(rep(lambda_l, nrow(L22)))
        results[[i]] <- rnc_solver_path_predict(
          X = x, Y = y, L = L_reg,
          X_test = x_test, Y_test = y_test, L22 = L22_reg, L21 = L21,
          lambda_netcoh = lambda_netcoh, lambdas_x = lambdas_x,
          return_fit = TRUE
        )
      }
    } else {
      x_alpha <- as.matrix(Matrix::sparseMatrix(i = 1:n, j = nodeids, x = 1))
      keep_nodeids <- sort(unique(nodeids))
      x_alpha <- x_alpha[, keep_nodeids]
      L_train <- L_reg[keep_nodeids, keep_nodeids]
      rnc_out <- rnc_nodeids_solver_path(
        X = x, Y = y, L = L_train,
        lambda_netcoh = lambda_netcoh, lambdas_x = lambdas_x,
        X_alpha = x_alpha
      )
      if (length(keep_nodeids) < nrow(L_reg)) {
        rm_nodeids <- setdiff(1:nrow(L_reg), keep_nodeids)
        L22 <- Matrix::Matrix(L_reg[rm_nodeids, rm_nodeids], sparse = TRUE)
        L21 <- L_reg[rm_nodeids, keep_nodeids]
        valid_alpha <- solve(L22, -L21 %*% rnc_out$alpha_nodeids, sparse = TRUE)
        alpha_all <- matrix(0, nrow = nrow(L_reg), ncol = ncol(valid_alpha))
        alpha_all[keep_nodeids, ] <- rnc_out$alpha_nodeids
        alpha_all[rm_nodeids, ] <- valid_alpha
      } else {
        alpha_all <- rnc_out$alpha_nodeids
      }
      yhat_test <- alpha_all[nodeids_test, ] + x_test %*% rnc_out$beta
      errs <- apply(yhat_test, 2, function(yhat) sqrt(mean((y_test - yhat)^2)))
      results[[i]] <- list(
        alpha = rnc_out$alpha_nodeids,
        beta = rnc_out$beta,
        errs = errs
      )
    }
  }

  alphas <- purrr::map(results, "alpha") |>
    purrr::reduce(cbind)
  betas <- purrr::map(results, "beta") |>
    purrr::reduce(cbind)
  cv_errs <- purrr::map(results, ~ c(.x$errs)) |>
    purrr::reduce(c)
  lambda_grid <- lambda_grid |>
    tidyr::unnest(lambda_x) |>
    dplyr::ungroup()
  out <- list(
    lambda_grid = lambda_grid,
    intercept = y_mean,
    alpha = alphas,
    beta = betas,
    cv_errs = cv_errs
  )
  return(out)
}


#' Fit regularization path for logistic regression with network cohesion
#' @keywords internal
rnc_logistic_path <- function(x, y, A, x_test = NULL, y_test = NULL,
                              A_full = NULL, lambda_grid,
                              nodeids = NULL, nodeids_test = NULL,
                              newton_maxit = 50, newton_tol = 1e-4, ...) {
  n <- nrow(x)
  p <- ncol(x)
  results <- list()
  for (i in 1:nrow(lambda_grid)) {
    lambda_netcoh <- lambda_grid$lambda_netcoh[[i]]
    lambda_x <- lambda_grid$lambda_x[[i]]
    lambda_l <- lambda_grid$lambda_l[[i]]

    log_fit <- rnc(
      x = x, y = y, A = A, nodeids = nodeids,
      lambda_netcoh = lambda_netcoh, lambda_x = lambda_x, lambda_l = lambda_l,
      family = "logistic", newton_maxit = newton_maxit, newton_tol = newton_tol,
      ...
    )
    if (!is.null(x_test)) {
      log_preds <- stats::predict(
        log_fit, x = x_test, A_full = A_full, nodeids = nodeids_test
      )
      log_fit$errs <- yardstick::roc_auc_vec(
        factor(y_test, levels = c(1, 0)), c(log_preds$y)
      )
    }
    results[[i]] <- log_fit
  }

  intercepts <- purrr::map_dbl(results, "intercept")
  alphas <- purrr::map(results, "alpha") |>
    purrr::reduce(cbind)
  betas <- purrr::map(results, "beta") |>
    purrr::reduce(cbind)
  cv_errs <- purrr::map(results, ~ c(.x$errs)) |>
    purrr::reduce(c)
  out <- list(
    lambda_grid = lambda_grid,
    intercept = intercepts,
    alpha = alphas,
    beta = betas,
    cv_errs = cv_errs
  )
  return(out)
}


