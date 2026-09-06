#' Predict method for rnc (regression with network cohesion) objects
#'
#' @param object An object of class `rnc` containing the fitted model.
#' @param x A numeric matrix or data frame of predictors (features) to make
#'   predictions for; size n x p.
#' @param A_full An adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order)
#' @param nodeids (Optional) vector of node IDs of length n.
#'   If provided, node IDs indicate the rows of A_full, corresponding to each
#'   sample. If not provided, the rows of A_full are assumed to be in the order
#'   of (x_train, x).
#' @param ... Not used.
#'
#' @returns A list of two:
#' - `y`: A numeric vector of predictions for the response variable.
#' - `alpha`: A numeric vector of node-specific effects (intercepts).
#'
#' @examples
#' data(example_data)
#'
#' # demonstrate prediction for RNC fit
#' rnc_fit <- rnc(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1, lambda_x = 0.5
#' )
#' predicted_out <- predict(
#'   rnc_fit, x = example_data$xtest, A_full = example_data$A_full
#' )
#'
#' # demonstrate prediction for RNC CV fit
#' cv_fit <- rnc_cv(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambdas_netcoh = c(0, 0.1, 1),
#'   lambdas_x = c(0.1, 0.5)
#' )
#' predicted_out <- predict(
#'   cv_fit, x = example_data$xtest, A_full = example_data$A_full
#' )
#'
#' @export
predict.rnc <- function(object, x, A_full, nodeids = NULL, ...) {
  if (identical(object$family, "linear")) {
    out <- predict_rnc_linear(
      object = object, x = x, A_full = A_full, nodeids = nodeids
    )
  } else if (identical(object$family, "logistic")) {
    out <- predict_rnc_logistic(
      object = object, x = x, A_full = A_full, nodeids = nodeids
    )
  } else {
    stop(sprintf("family = %s has not yet been implemented.", object$family))
  }
  return(out)
}


#' Predict method for rnc linear regression objects
#'
#' @inheritParams predict.rnc
#' @keywords internal
predict_rnc_linear <- function(object, x, A_full, nodeids = NULL) {
  predict_rnc_linear_with_laplacian(
    object = object, x = x, A_full = A_full, nodeids = nodeids,
    L_full = NULL, solve_cache = NULL
  )
}


#' @keywords internal
predict_rnc_linear_with_laplacian <- function(object, x, A_full, nodeids = NULL,
                                              L_full = NULL,
                                              solve_cache = NULL) {
  n <- nrow(x)
  n_alpha <- nrow(A_full)
  p <- ncol(x)

  intercept <- object$intercept
  alpha <- object$alpha
  beta <- object$beta
  lambda_l <- object$lambda_l
  n_train <- object$nalpha_train

  if (n_alpha < n_train) {
    stop("Dimension mismatch!")
  } else if ((n_alpha == n_train) && !any(is.na(alpha))) {  # assume A_train == A_full
    if (is.null(nodeids)) {
      yhat <- alpha + as.matrix(x) %*% beta
    } else {
      alpha <- alpha[nodeids]
      yhat <- alpha + as.matrix(x) %*% beta
    }
    out <- list(
      y = yhat,
      alpha = alpha
    )
  } else {
    if (is.null(L_full)) {
      L_full <- make_laplacian(A_full, lambda_l = lambda_l)
    }

    train_idx <- which(!is.na(alpha))
    if (n_alpha == n_train) {
      valid_idx <- which(is.na(alpha))
    } else {
      if (!is.null(nodeids)) {
        valid_idx <- c(which(is.na(alpha)), (n_train + 1):n_alpha)
      } else {
        valid_idx <- (n_train + 1):n_alpha
      }
    }

    solve_entry <- get_cached_rnc_prediction_solve(
      L_full = L_full,
      train_idx = train_idx,
      valid_idx = valid_idx,
      n_train = n_train,
      n_alpha = n_alpha,
      lambda_l = lambda_l,
      nodeids = nodeids,
      alpha = alpha,
      solve_cache = solve_cache
    )
    if (is.null(solve_entry)) {
      L22 <- Matrix::Matrix(
        L_full[valid_idx, valid_idx, drop = FALSE],
        sparse = TRUE
      )
      L21 <- L_full[valid_idx, train_idx, drop = FALSE]
      valid_alpha <- as.matrix(
        Matrix::solve(L22, -L21 %*% alpha[train_idx])
      )
    } else {
      valid_alpha <- as.matrix(
        Matrix::solve(
          solve_entry$factor,
          -solve_entry$L21 %*% alpha[train_idx]
        )
      )
    }
    if (!is.null(nodeids) || (n_alpha == n_train)) {
      alpha_all <- rep(NA, n_alpha)
      alpha_all[train_idx] <- alpha[train_idx]
      alpha_all[valid_idx] <- valid_alpha
      if (!is.null(nodeids)) {
        valid_alpha <- alpha_all[nodeids]
      } else {
        valid_alpha <- alpha_all
      }
    }
    yhat <- valid_alpha + as.matrix(x) %*% beta
    out <- list(
      y = yhat,
      alpha = valid_alpha
    )
  }
  out$y <- intercept + out$y
  return(out)
}


#' Predict method for rnc logistic regression objects
#'
#' @inheritParams predict.rnc
#' @keywords internal
predict_rnc_logistic <- function(object, x, A_full, nodeids = NULL) {
  out <- predict_rnc_logistic_with_laplacian(
    object = object, x = x, A_full = A_full, nodeids = nodeids,
    L_full = NULL, solve_cache = NULL
  )
  return(out)
}


#' @keywords internal
predict_rnc_logistic_with_laplacian <- function(object, x, A_full, nodeids = NULL,
                                                L_full = NULL,
                                                solve_cache = NULL) {
  out <- predict_rnc_linear_with_laplacian(
    object, x, A_full, nodeids, L_full, solve_cache
  )
  out$y <- as.matrix(1 / (1 + exp(-out$y)))
  return(out)
}


#' @keywords internal
get_cached_rnc_prediction_solve <- function(L_full, train_idx, valid_idx,
                                            n_train, n_alpha, lambda_l,
                                            nodeids, alpha, solve_cache) {
  if (is.null(solve_cache) || !is.null(nodeids)) {
    return(NULL)
  }
  if (length(train_idx) != n_train || !identical(train_idx, seq_len(n_train))) {
    return(NULL)
  }
  if (!identical(valid_idx, (n_train + 1):n_alpha)) {
    return(NULL)
  }
  if (any(is.na(alpha[train_idx]))) {
    return(NULL)
  }

  key <- paste(n_train, n_alpha, format(lambda_l, digits = 17), sep = ":")
  if (!exists(key, envir = solve_cache, inherits = FALSE)) {
    L22 <- Matrix::Matrix(
      L_full[valid_idx, valid_idx, drop = FALSE],
      sparse = TRUE
    )
    factor <- tryCatch(
      Matrix::Cholesky(L22, LDL = FALSE, Imult = 0),
      error = function(e) NULL
    )
    if (is.null(factor)) {
      return(NULL)
    }
    assign(
      key,
      list(
        factor = factor,
        L21 = L_full[valid_idx, train_idx, drop = FALSE]
      ),
      envir = solve_cache
    )
  }
  get(key, envir = solve_cache, inherits = FALSE)
}
