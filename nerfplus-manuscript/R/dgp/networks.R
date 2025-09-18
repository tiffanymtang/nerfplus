#' Create Stochastic Block Model Network
#'
#' @param n Number of nodes in the network.
#' @param K Number of blocks (communities).
#' @param pb Probability of edges between different blocks.
#' @param pw Probability of edges within the same block.
#' @param pi Vector of block probabilities (optional).
#' @param theta Vector of degree correction parameters (optional).
#' @param dc Logical indicating if degree correction should be applied.
#' @param poisson_edges Logical indicating if edges should be Poisson
#'   distributed. Default is FALSE so that edges are binary.
#' @param allow_self_loops Logical indicating if self-loops are allowed. Default
#'   is FALSE.
#'
#' @returns A list containing the adjacency matrix `A` and a vector of block IDs
sbm_network <- function(n, K, pb, pw, pi = NULL, theta = NULL, dc = FALSE,
                        poisson_edges = FALSE, allow_self_loops = FALSE) {
  if (is.null(pi)) {
    pi <- rep(1 / K, K)
  }
  # generate mixing matrix
  B <- matrix(pb, ncol = K, nrow = K)
  diag(B) <- pw
  if (!dc) {
    # create stochastic block model object
    sbm_obj <- fastRG::sbm(
      n = n, B = B, pi = pi,
      poisson_edges = poisson_edges, allow_self_loops = allow_self_loops
    )
  } else {
    # create degree-corrected stochastic block model object
    if (is.null(theta)) {
      theta <- sample(0.8 + 0.785 * ((1:n) / n)^2, n, replace = FALSE)
    }
    sbm_obj <- fastRG::dcsbm(
      n = n, theta = theta, B = B, pi = pi,
      poisson_edges = poisson_edges, allow_self_loops = allow_self_loops
    )
  }
  block_ids <- apply(sbm_obj$X, 1, FUN = function(x) which(x != 0))
  A_sparse <- fastRG::sample_sparse(sbm_obj)
  A <- as.matrix(A_sparse)
  return(list(A = A, block_ids = block_ids))
}


#' Create network from real-world data
#'
#' @param A Adjacency matrix of the network.
#' @param block_ids Vector of block IDs corresponding to the nodes in the
#'   network.
#' @param n Number of nodes to sample from the network. If NULL, all nodes are
#'   used. If specified, a random sample of nodes is taken.
#'
#' @returns A list containing the sampled adjacency matrix `A` and the vector of
#'   block IDs `block_ids`.
rwd_network <- function(A, block_ids, n = NULL) {
  if (is.null(n) || (n == nrow(A))) {
    out <- list(A = A, block_ids = block_ids)
  } else {
    keep_idxs <- sample(1:nrow(A), n, replace = FALSE)
    out <- list(A = A[keep_idxs, keep_idxs], block_ids = block_ids[keep_idxs])
  }
  return(out)
}
