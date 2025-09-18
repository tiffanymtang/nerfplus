test_that("network-embeddings works", {
  skip_on_ci()
  data(example_data)
  A <- example_data$A
  A_full <- example_data$A_full

  # fit embeddings
  laplacian_embedding <- fit_network_embedding(A = A, method = "laplacian")
  expect_snapshot(laplacian_embedding$X)
  adjacency_embedding <- fit_network_embedding(A = A, method = "adjacency")
  expect_snapshot(adjacency_embedding$X)
  score_embedding <- fit_network_embedding(A = A, method = "score")
  expect_snapshot(score_embedding$X)

  # predict on out-of-sample
  oos_laplacian_embedding <- oos_network_embedding(
    embedding_fit = laplacian_embedding,
    A = A,
    A_full = A_full
  )
  expect_snapshot(oos_laplacian_embedding$X)
  oos_adjacency_embedding <- oos_network_embedding(
    embedding_fit = adjacency_embedding,
    A = A,
    A_full = A_full
  )
  expect_snapshot(oos_adjacency_embedding$X)
  oos_score_embedding <- oos_network_embedding(
    embedding_fit = score_embedding,
    A = A,
    A_full = A_full
  )
  expect_snapshot(oos_score_embedding$X)
})
