#' Generate individual node effects (alphas) for a block model.
#'
#' @param block_ids Vector of block IDs for each node.
#' @param scale Scale of the centroids for each block.
#'
#' @returns A vector of alphas corresponding to each node in `block_ids`.
generate_alphas <- function(block_ids, scale) {
  K <- length(unique(block_ids))
  n <- length(block_ids)
  centroids <- seq(-scale, scale, length.out = K)
  alphas <- centroids[block_ids]
  return(alphas)
}


#' Get the largest connected component of a graph.
#'
#' @param A Adjacency matrix of the graph.
#' @param X Feature matrix where rows correspond to samples and columns to
#'   features.
#' @param y Response vector corresponding to the samples in `X`.
#'
#' @returns A list containing the adjacency matrix `A`, feature matrix `X`,
#'   response vector `y`, and the indices of the samples in the largest
#'   connected component.
get_connected_graph <- function(A, X, y) {
  names(y) <- rownames(X)
  g <- igraph::graph_from_adjacency_matrix(A > 0, mode = "undirected")
  cc <- igraph::components(g)
  largest_cc <- which.max(cc$csize)
  keep_sample_idxs <- names(cc$membership[cc$membership == largest_cc])
  A <- A[keep_sample_idxs, keep_sample_idxs]
  X <- X[keep_sample_idxs, , drop = FALSE]
  y <- y[keep_sample_idxs]
  names(y) <- NULL
  return(list(A = A, X = X, y = y, keep_sample_idxs = keep_sample_idxs))
}


#' Load and split data for a DGP.
#'
#' @param X Feature matrix where rows correspond to samples and columns to
#'   features.
#' @param y Response vector corresponding to the samples in `X`.
#' @param A Adjacency matrix of the graph.
#' @param alphas Optional vector of individual node effects (alphas).
#' @param block_ids Optional vector of block IDs corresponding to the nodes in
#'   `A`.
#' @param train_prop Proportion of data to use for training (default is 0.8).
#' @param connected Logical indicating if the training data should be restricted
#'   to the largest connected component of the graph (default is FALSE).
#'
#' @returns A list containing the training and test data, the adjacency matrix
#'   `A`, the full adjacency matrix `A_full`, and optionally the alphas and
#'   block IDs if provided.
load_data <- function(X, y, A, alphas = NULL, block_ids = NULL,
                      train_prop = 0.8, connected = FALSE) {

  # helper variables / setup
  X <- as.data.frame(X)
  n <- nrow(X)
  if (is.null(rownames(A))) {
    rownames(A) <- 1:nrow(A)
    colnames(A) <- 1:nrow(A)
  }

  # error checking
  if (train_prop < 0 || train_prop > 1) {
    stop("train_prop must be between 0 and 1")
  }
  if (length(y) != n) {
    stop("Length of y must match number of rows in X")
  }

  # split data
  if (train_prop < 1) {
    train_ids <- sample(seq(n), size = round(n * train_prop), replace = FALSE)
  } else {
    train_ids <- seq(n)
  }
  out <- list(
    x = X[train_ids, , drop = FALSE],
    y = y[train_ids],
    x_test = X[-train_ids, , drop = FALSE],
    y_test = y[-train_ids]
  )

  # get A
  train_idx <- rownames(out$x)
  test_idx <- rownames(out$x_test)
  out$A <- A[train_idx, train_idx]

  # restrict training to largest connected component
  if (connected) {
    names(out$y) <- rownames(out$x)
    train_connected_out <- get_connected_graph(out$A, out$x, out$y)
    keep_train_idxs <- train_connected_out$keep_sample_idxs
    rm_train_idxs <- setdiff(rownames(out$x), keep_train_idxs)
    out$A <- train_connected_out$A
    out$x_test <- rbind(out$x[rm_train_idxs, , drop = FALSE], out$x_test)
    out$y_test <- c(out$y[rm_train_idxs], out$y_test)
    out$x <- train_connected_out$X
    out$y <- train_connected_out$y
    names(out$y_test) <- NULL
    train_idx <- keep_train_idxs
    test_idx <- c(rm_train_idxs, test_idx)
  }

  # get A_full
  out$A_full <- A[c(train_idx, test_idx), c(train_idx, test_idx)]

  # get alphas and block_ids if provided
  if (!is.null(alphas) || !is.null(block_ids)) {
    out$verbose_data_out <- list()
  }
  if (!is.null(alphas)) {
    if (is.null(names(alphas))) {
      names(alphas) <- 1:length(alphas)
    }
    out$verbose_data_out$alphas_full <- alphas[c(train_idx, test_idx)]
  }
  if (!is.null(block_ids)) {
    out$verbose_data_out$block_ids_full <- block_ids[c(train_idx, test_idx)]
  }
  return(out)
}
