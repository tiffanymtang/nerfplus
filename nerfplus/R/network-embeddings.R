#' Wrapper to fit various network embedding methods
#'
#' @param A Adjacency matrix.
#' @param ndim Number of dimensions in embedding.
#' @param method Embedding type(s), at least one of "adjacency", "laplacian", or
#'   score".
#' @param regularization Regularization parameter for the adjacency matrix.
#' @param varimax_rotation Whether to apply varimax rotation to the embedding.
#'
#' @returns A list containing the following components:
#' - `X`: A list of matrices, each corresponding to a method in `method`, where
#'   each matrix has `ndim` columns representing the embedding.
#' - `eval_ls`: A list of eigenvalues corresponding to each method in `method`.
#' - `method`: The method(s) used for embedding.
#' - `regularization`: The regularization parameter used for the adjacency
#'   matrix.
#'
#' @examples
#' data(example_data)
#' embedding_fit <- fit_network_embedding(
#'   example_data$A, ndim = 2, method = "laplacian"
#' )
#'
#' @export
fit_network_embedding <- function(A,
                                  ndim = 2,
                                  method = c("adjacency", "laplacian", "score"),
                                  regularization = 0.5,
                                  varimax_rotation = FALSE) {
  method <- match.arg(method, several.ok = TRUE)
  A_rowsums <- rowSums(A)
  regularization <- regularization * mean(A_rowsums) / nrow(A)
  A <- A + regularization
  X_embed_ls <- list()
  eval_ls <- list()
  for (m in method) {
    if (m == "adjacency") {
      svd_out <- irlba::irlba(A, nv = ndim)
      evec <- svd_out$u
      eval <- svd_out$d
    } else if (m == "laplacian") {
      d <- 1 / sqrt(A_rowsums)
      if (any(is.infinite(d))) {
        warning("Infinite values in network embedding, setting to 0.")
        d[is.infinite(d)] <- 0
      }
      L <- t(t(d * A) * d)
      svd_out <- irlba::irlba(L, nv = ndim + 1)
      evec <- svd_out$u[, -1, drop = FALSE]
      eval <- svd_out$d[-1]
    } else if (m == "score") {
      svd_out <- irlba::irlba(A, nv = ndim + 1)
      evec <- svd_out$u[, -1, drop = FALSE] / svd_out$u[, 1]
      eval <- svd_out$d[-1]
    }
    X_embed <- evec %*% diag(sqrt(eval), nrow = ndim, ncol = ndim)
    if (varimax_rotation && (ndim > 1)) {
      varimax_out <- stats::varimax(X_embed)
      X_embed <- cbind(varimax_out$loadings)
    }
    colnames(X_embed) <- sprintf(".embed_%s%s", m, 1:ndim)
    X_embed_ls[[m]] <- X_embed
    eval_ls[[m]] <- eval
  }
  embedding <- list(
    X = X_embed_ls,
    eval_ls = eval_ls,
    method = method,
    regularization = regularization
  )
  return(embedding)
}


#' Out-of-sample network embedding method
#'
#' @description
#' This function takes in out-of-sample points (which were not available at
#' training time) and embeds them in a previously trained network embedding
#' space.
#'
#' @param embedding_fit Previously trained network embedding; should be
#'   output of `fit_network_embedding()`.
#' @param A Adjacency matrix corresponding to the training data points.
#' @param A_full Adjacency matrix representing the full set of nodes (training +
#'   testing nodes in that order).
#'
#' @returns A list of two:
#' - `X`: A list of matrices, each corresponding to a method in
#'   `embedding_fit$method`, where each matrix has `ndim` columns representing
#'   the embedding for the out-of-sample points.
#' - `method`: The method(s) used for embedding.
#'
#' @examples
#' data(example_data)
#' embedding_fit <- fit_network_embedding(
#'   example_data$A, ndim = 2, method = "laplacian"
#' )
#' oos_embedding <- oos_network_embedding(
#'   embedding_fit, A = example_data$A, A_full = example_data$A_full
#' )
#'
#' @export
oos_network_embedding <- function(embedding_fit, A, A_full) {
  n_train <- nrow(A)
  X <- embedding_fit$X
  eval_ls <- embedding_fit$eval_ls
  method <- embedding_fit$method
  regularization <- embedding_fit$regularization
  A_full <- A_full + regularization
  X_oos_ls <- list()
  for (m in method) {
    X_embed <- X[[m]]
    eval <- eval_ls[[m]]
    if (m %in% c("adjacency", "score")) {
      Y <- A_full[1:n_train, (n_train + 1):nrow(A_full)]
      X_oos <- t(Y) %*% X_embed %*% diag(1 / eval)
    } else if (m == "laplacian") {
      d_full <- 1 / sqrt(rowSums(A_full))
      d_full[is.infinite(d_full)] <- 0
      L_full <- t(t(d_full * A_full) * d_full)
      Y <- L_full[1:n_train, (n_train + 1):nrow(A_full), drop = FALSE]
      norm_vec <- colSums(Y)
      if (any(norm_vec == 0)) {
        warning("Zero norm in out-of-sample embedding, setting to 1.")
        norm_vec[norm_vec == 0] <- 1
      }
      X_oos <- t(Y) %*% X_embed %*% diag(1 / eval)
      X_oos <- t(t(X_oos) / norm_vec)
    }
    colnames(X_oos) <- colnames(X_embed)
    X_oos_ls[[m]] <- X_oos
  }
  oos_embedding <- list(
    X = X_oos_ls,
    method = method
  )
  return(oos_embedding)
}
