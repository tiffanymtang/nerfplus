test_that("rnc works", {
  skip_on_ci()
  lambda_netcoh <- 1
  lambda_x <- 0.1
  data(example_data)
  example_data$y_class <- ifelse(example_data$y > median(example_data$y), 1, 0)
  expected_names <- c(
    "intercept", "alpha", "beta",
    "lambda_netcoh", "lambda_x", "lambda_l",
    "nalpha_train", "family"
  )

  rnc_linear_out <- rnc(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh, lambda_x = lambda_x,
    family = "linear", low_dim = TRUE
  )
  expect_equal(names(rnc_linear_out), expected_names)
  expect_snapshot(rnc_linear_out$alpha)
  expect_snapshot(rnc_linear_out$beta)

  rnc_linear2_out <- rnc(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh, lambda_x = lambda_x,
    family = "linear", low_dim = FALSE
  )
  expect_equal(names(rnc_linear2_out), expected_names)
  expect_equal(rnc_linear_out$alpha, rnc_linear2_out$alpha)
  expect_equal(rnc_linear_out$beta, rnc_linear2_out$beta)

  rnc_logistic_out <- rnc(
    x = example_data$x, y = example_data$y_class, A = example_data$A,
    lambda_netcoh = lambda_netcoh, lambda_x = lambda_x,
    family = "logistic"
  )
  expect_equal(names(rnc_logistic_out), expected_names)
  expect_snapshot(rnc_logistic_out$alpha)
  expect_snapshot(rnc_logistic_out$beta)

  # check if predictions work
  preds_out <- predict(rnc_linear_out, x = example_data$xtest, A_full = example_data$A_full)
  expect_snapshot(preds_out$y)
  expect_snapshot(preds_out$alpha)

  preds_out <- predict(rnc_linear_out, x = example_data$x, A_full = example_data$A)
  expect_equal(preds_out$alpha, rnc_linear_out$alpha)

  preds_out <- predict(rnc_logistic_out, x = example_data$xtest, A_full = example_data$A_full)
  expect_snapshot(preds_out$y)
  expect_snapshot(preds_out$alpha)

  preds_out <- predict(rnc_logistic_out, x = example_data$x, A_full = example_data$A)
  expect_equal(preds_out$alpha, rnc_logistic_out$alpha)
})
