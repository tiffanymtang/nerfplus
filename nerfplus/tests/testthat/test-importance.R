test_that("get_feature_importances works", {
  set.seed(123)

  n <- 50
  lambda_netcoh <- 1
  lambda_embed <- 0.1
  lambda_raw <- 2
  lambda_stump <- 3
  expected_vars <- c(paste0("V", 1:10), ".embed", ".alpha", ".network")
  data(example_data)

  # linear regression
  nerfplus_linear_out <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  permutation_fi <- get_feature_importances(
    nerfplus_linear_out, x = example_data$xtest, y = example_data$ytest, A_full = example_data$A_full,
    method = "permute", B = 4
  )
  expect_equal(permutation_fi$var, expected_vars)
  expect_equal(colnames(permutation_fi), c("var", "importance"))
  expect_snapshot(permutation_fi)

  mdiplus_fi <- get_feature_importances(
    nerfplus_linear_out, x = example_data$xtest, y = example_data$ytest, A_full = example_data$A_full,
    method = "mdi+"
  )
  expect_equal(mdiplus_fi$var, expected_vars)
  expect_equal(colnames(mdiplus_fi), c("var", "importance"))
  expect_snapshot(mdiplus_fi)

  local_fi <- get_feature_importances(
    nerfplus_linear_out, x = example_data$xtest, y = example_data$ytest, A_full = example_data$A_full,
    method = "local", B = 4
  )
  expect_equal(dim(local_fi), c(nrow(example_data$xtest), length(expected_vars)))
  expect_snapshot(local_fi)
})
