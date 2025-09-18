## code to prepare `example_data` dataset goes here
set.seed(331)
n <- 120
p <- 10
train_prop <- 2 / 3
train_ids <- sort(sample(1:n, size = train_prop * n, replace = FALSE))
test_ids <- setdiff(1:n, train_ids)
X_full <- matrix(rnorm(n * p), n, p)
beta <- matrix(c(1, rep(0, p - 1)), ncol = 1)

# Function to generate SBM adjacency matrix
sbm <- function(n, block_sizes, prob_matrix) {
  # n: total number of nodes
  # block_sizes: vector of community sizes (sums to n)
  # prob_matrix: matrix of within- and between-block probabilities

  stopifnot(sum(block_sizes) == n)
  k <- length(block_sizes)

  # Assign block labels
  blocks <- rep(1:k, times = block_sizes)

  # Initialize adjacency matrix
  A <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in i:n) {
      p <- prob_matrix[blocks[i], blocks[j]]
      A[i, j] <- rbinom(1, 1, p)
      A[j, i] <- A[i, j]  # symmetric
    }
  }

  diag(A) <- 0  # no self-loops
  return(list(A = A, blockids = blocks))
}

sbm_prob_matrix <- matrix(
  c(0.8, 0.2, 0.2,
    0.2, 0.8, 0.2,
    0.2, 0.2, 0.8),
  nrow = 3
)
sbm_out <- sbm(n, block_sizes = rep(n / 3, 3), prob_matrix = sbm_prob_matrix)

A_full <- sbm_out$A
blockids <- sbm_out$blockids
alphas <- dplyr::case_when(
  blockids == 1 ~ -1,
  blockids == 2 ~ 0,
  blockids == 3 ~ 1
)
y_full <- alphas + X_full %*% beta + rnorm(n, sd = 0.5)

example_data <- list(
  x = X_full[train_ids, , drop = FALSE],
  xtest = X_full[-train_ids, , drop = FALSE],
  y = y_full[train_ids],
  ytest = y_full[-train_ids],
  A = A_full[train_ids, train_ids],
  A_full = A_full[c(train_ids, test_ids), c(train_ids, test_ids)]
)

usethis::use_data(example_data, overwrite = TRUE)
