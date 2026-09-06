#' Fit regression with network cohesion
#'
#' @description This function fits a regression model with network cohesion
#'   regularization. This code has been adapted from the `netcoh` R package with
#'   minor tweaks for increased speed and flexibility. Currently, it can handle
#'   linear and logistic regression only.
#'
#' @inheritParams shared_args
#' @param nodedegrees (Optional) A vector of node degrees. If provided, it is
#'   used to compute the graph Laplacian. If not provided, it is computed from
#'   the adjacency matrix `A`.
#' @param lambda_x (Optional) Regularization parameter for the (linear)
#'   covariates. Default is 0 (i.e., no regularization).
#'
#' @returns A list with the following components:
#' - `alpha`: n x 1 matrix of coefficients corresponding to the individual node
#'   effects
#' - `beta`: p x 1 matrix of coefficients corresponding to the covariates
#' - `lambda_netcoh`: Regularization parameter for the network cohesion term.
#' - `lambda_x`: Regularization parameter for the covariates.
#' - `lambda_l`: Regularization parameter for the graph Laplacian.
#' - `nalpha_train`: Number of distinct nodes in the training set (i.e., number
#'   of rows in `A`).
#' - `family`: The type of model fitted, either "linear" or "logistic".
#'
#' @examples
#' data(example_data)
#' rnc_fit <- rnc(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1, lambda_x = 0.5
#' )
#'
#' @export
rnc <- function(x, y, A, nodedegrees = NULL, nodeids = NULL,
                lambda_netcoh, lambda_x = 0, lambda_l = 0.05,
                family = c("linear", "logistic"), low_dim = NULL,
                init = NULL, newton_maxit = 50, newton_tol = 1e-4,
                verbose = FALSE) {
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

  if (identical(family, "linear")) {
    result <- rnc_linear(
      x = x, y = y, A = A, nodedegrees = nodedegrees, nodeids = nodeids,
      lambda_netcoh = lambda_netcoh / (n^2),
      lambda_x = lambda_x / ncol(x),
      lambda_l = lambda_l,
      low_dim = low_dim
    )
  } else if (identical(family, "logistic")) {
    result <- rnc_logistic(
      x = x, y = y, A = A, nodedegrees = nodedegrees, nodeids = nodeids,
      lambda_netcoh = lambda_netcoh / (n^2),
      lambda_x = lambda_x / ncol(x),
      lambda_l = lambda_l,
      init = init, newton_maxit = newton_maxit, newton_tol = newton_tol,
      verbose = verbose
    )
  }
  out <- list(
    intercept = result$intercept,
    alpha = result$alpha,
    beta = result$beta,
    lambda_netcoh = lambda_netcoh,
    lambda_x = lambda_x,
    lambda_l = lambda_l,
    nalpha_train = nrow(A),
    family = family
  )
  class(out) <- "rnc"
  return(out)
}


#' @keywords internal
rnc_linear_cached_factor <- function(x, y, A, lambda_netcoh, lambda_x = 0,
                                     lambda_l = 0.05,
                                     factor_cache = NULL) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (!isTRUE(length(dim(y)) == 2)) {
    y <- matrix(y, ncol = 1)
  }
  n <- nrow(x)
  p <- ncol(x)
  if ((nrow(y) != n) || (nrow(A) != n) || (ncol(A) != n)) {
    return(rnc(
      x = x, y = y, A = A, lambda_netcoh = lambda_netcoh,
      lambda_x = lambda_x, lambda_l = lambda_l, family = "linear",
      low_dim = FALSE
    ))
  }
  if (is.null(factor_cache)) {
    factor_cache <- new.env(parent = emptyenv())
  }

  cached_fit <- rnc_linear_scaled_cached_factor(
    x = x,
    y = y,
    A = A,
    lambda_netcoh = lambda_netcoh / n^2,
    lambda_x = lambda_x / p,
    lambda_l = lambda_l,
    factor_cache = factor_cache
  )

  if (is.null(cached_fit)) {
    cached_fit <- rnc(
      x = x, y = y, A = A, lambda_netcoh = lambda_netcoh,
      lambda_x = lambda_x, lambda_l = lambda_l, family = "linear",
      low_dim = FALSE
    )
  } else {
    cached_fit$lambda_netcoh <- lambda_netcoh
    cached_fit$lambda_x <- lambda_x
    cached_fit$lambda_l <- lambda_l
    cached_fit$nalpha_train <- nrow(A)
    cached_fit$family <- "linear"
    class(cached_fit) <- "rnc"
  }
  cached_fit
}


#' @keywords internal
get_cached_rnc_linear_factor <- function(A, lambda_netcoh, lambda_l, n,
                                         factor_cache) {
  get_cached_rnc_linear_scaled_factor(
    A = A,
    nodedegrees = NULL,
    lambda_netcoh = lambda_netcoh / n^2,
    lambda_l = lambda_l,
    n = n,
    factor_cache = factor_cache
  )
}


#' @keywords internal
rnc_linear_scaled_cached_factor <- function(x, y, A, nodedegrees = NULL,
                                            lambda_netcoh, lambda_x = 0,
                                            lambda_l = 0.05,
                                            factor_cache = NULL) {
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(factor_cache)) {
    factor_cache <- new.env(parent = emptyenv())
  }

  tryCatch(
    {
      factor_entry <- get_cached_rnc_linear_scaled_factor(
        A = A,
        nodedegrees = nodedegrees,
        lambda_netcoh = lambda_netcoh,
        lambda_l = lambda_l,
        n = n,
        factor_cache = factor_cache
      )
      if (is.null(factor_entry)) {
        stop("Unable to factor network matrix.")
      }

      y_mean <- mean(y)
      y_centered <- y - y_mean
      solved <- as.matrix(Matrix::solve(factor_entry$factor, cbind(y_centered, x)))
      inv_y <- solved[, 1, drop = FALSE]
      inv_x <- solved[, -1, drop = FALSE]

      beta_lhs <- crossprod(x, x - inv_x)
      diag(beta_lhs) <- diag(beta_lhs) + rep(lambda_x, length.out = p)
      beta_rhs <- crossprod(x, y_centered - inv_y)
      beta <- solve(beta_lhs, beta_rhs)
      alpha <- inv_y - inv_x %*% beta

      list(
        intercept = y_mean,
        alpha = alpha,
        beta = beta
      )
    },
    error = function(e) NULL
  )
}


#' @keywords internal
get_cached_rnc_linear_scaled_factor <- function(A, nodedegrees = NULL,
                                                lambda_netcoh, lambda_l, n,
                                                factor_cache) {
  key <- paste(
    n,
    format(lambda_netcoh, digits = 17),
    format(lambda_l, digits = 17),
    sep = ":"
  )
  if (!exists(key, envir = factor_cache, inherits = FALSE)) {
    L <- make_sparse_laplacian(A, lambda_l = lambda_l, nodedegrees = nodedegrees)
    K <- Matrix::Diagonal(n = n) + lambda_netcoh * L
    factor <- tryCatch(
      Matrix::Cholesky(K, LDL = FALSE, Imult = 0),
      error = function(e) NULL
    )
    if (is.null(factor)) {
      return(NULL)
    }
    assign(key, list(factor = factor), envir = factor_cache)
  }
  get(key, envir = factor_cache, inherits = FALSE)
}


#' Fit linear regression with network cohesion
#'
#' @inheritParams rnc
#' @keywords internal
rnc_linear <- function(x, y, A, nodedegrees = NULL, nodeids = NULL,
                       lambda_netcoh, lambda_x = 0, lambda_l = 0.05,
                       low_dim = NULL) {
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(low_dim)) {
    low_dim <- p <= (n / 5)
  }

  if (is.null(nodeids) && !low_dim) {
    out <- rnc_linear_scaled_cached_factor(
      x = x,
      y = y,
      A = A,
      nodedegrees = nodedegrees,
      lambda_netcoh = lambda_netcoh,
      lambda_x = lambda_x,
      lambda_l = lambda_l
    )
    if (!is.null(out)) {
      return(out)
    }
  }

  L <- make_laplacian(A, lambda_l = lambda_l, nodedegrees = nodedegrees)
  W <- diag(1, nrow = n)
  H <- diag(lambda_x, nrow = p)

  # center y
  y_mean <- mean(y)
  y <- y - y_mean

  if (is.null(nodeids)) {
    if (low_dim) {
      rnc_fun <- rnc_solver_naive
    } else {
      rnc_fun <- rnc_solver
    }
    out <- rnc_fun(
      X = x, Y = y, L = L, H = H, W = W, lambda_netcoh = lambda_netcoh
    )
  } else {
    if (!low_dim && !identical(W, diag(nrow(W)))) {
      rnc_fun <- rnc_nodeids_solver
    } else {
      rnc_fun <- rnc_nodeids_solver_naive
    }
    x_alpha <- as.matrix(Matrix::sparseMatrix(i = 1:n, j = nodeids, x = 1))
    keep_nodeids <- sort(unique(nodeids))
    out <- rnc_fun(
      X = x, Y = y, L = L[keep_nodeids, keep_nodeids],
      H = H, W = W, X_alpha = x_alpha[, keep_nodeids, drop = FALSE],
      lambda_netcoh = lambda_netcoh
    )
    alpha <- rep(NA, nrow(A))
    alpha[keep_nodeids] <- out$alpha
    out$alpha <- alpha
  }
  out$intercept <- y_mean
  return(out)
}


#' Fit logistic regression with network cohesion
#'
#' @inheritParams rnc
#' @keywords internal
rnc_logistic <- function(x, y, A, nodedegrees = NULL, nodeids = NULL,
                         lambda_netcoh, lambda_x = 0, lambda_l = 0.05,
                         init = NULL, newton_maxit = 50, newton_tol = 1e-4,
                         verbose = FALSE) {
  if (!is.null(nodeids)) {
    stop("nodeids is not yet supported for logistic regression.")
  }
  L <- make_laplacian(A, lambda_l = lambda_l, nodedegrees = nodedegrees)
  rnc_logistic_scaled_laplacian(
    x = x, y = y, L = L,
    lambda_netcoh = lambda_netcoh,
    lambda_x = lambda_x,
    init = init,
    newton_maxit = newton_maxit,
    newton_tol = newton_tol,
    verbose = verbose
  )
}


#' Fit scaled logistic regression with a precomputed network Laplacian
#'
#' @keywords internal
rnc_logistic_scaled_laplacian <- function(x, y, L, lambda_netcoh,
                                          lambda_x = 0, init = NULL,
                                          newton_maxit = 50,
                                          newton_tol = 1e-4,
                                          verbose = FALSE) {
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(init)) {
    init <- matrix(0, nrow = n + p, ncol = 1)
  }

  # add intercept (if x != constant column vector)
  if ((ncol(x) != 1) || (length(unique(x)) != 1)) {
    include_intercept <- TRUE
    init <- rbind(mean(y), init)
    x <- cbind(1, x)
  } else {
    include_intercept <- FALSE
  }

  H <- diag(lambda_x, nrow = p)
  if (include_intercept) {
    H_intercept <- matrix(0, nrow = p + 1, ncol = p + 1)
    H_intercept[-1, -1] <- H
    H <- H_intercept
  }
  out <- rnc_logistic_solver(
    X = x, Y = y, L = L, H = H, lambda_netcoh = lambda_netcoh,
    theta_init = init, maxit = newton_maxit, tol = newton_tol,
    verbose = verbose
  )
  if (include_intercept) {
    out$intercept <- out$beta[1]
    out$beta <- out$beta[-1]
  } else {
    out$intercept <- 0
  }
  return(out)
}
