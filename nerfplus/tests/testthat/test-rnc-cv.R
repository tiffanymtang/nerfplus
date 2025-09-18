test_that("rnc_cv works", {
  skip_on_ci()
  cv_foldids <- list(1:5, 6:10)
  lambdas_netcoh <- c(0.1, 1)
  lambdas_x <- c(0.5, 1.5)
  data(example_data)
  example_data$y_class <- ifelse(example_data$y > median(example_data$y), 1, 0)
  expected_names <- c(
    "intercept", "alpha", "beta",
    "lambda_netcoh", "lambda_x", "lambda_l",
    "nalpha_train", "family",
    "lambda_grid", "best_params", "cv_errs", "cv_means", "cv_sds"
  )

  rnc_cv_linear_out <- rnc_cv(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambdas_netcoh = lambdas_netcoh, lambdas_x = lambdas_x,
    family = "linear", cv_foldids = cv_foldids
  )
  expect_equal(names(rnc_cv_linear_out), expected_names)
  expect_snapshot(rnc_cv_linear_out$alpha)
  expect_snapshot(rnc_cv_linear_out$beta)
  expect_equal(rnc_cv_linear_out$lambda_netcoh, 1)
  expect_equal(rnc_cv_linear_out$lambda_x, rep(0.5, 10))
  expect_equal(rnc_cv_linear_out$lambda_l, 0.05)
  expect_equal(
    rnc_cv_linear_out$lambda_grid,
    tibble::tibble(
      lambda_netcoh = c(0.1, 0.1, 1, 1),
      lambda_l = c(0.05, 0.05, 0.05, 0.05),
      lambda_x = list(
        rep(0.5, 10),
        rep(1.5, 10),
        rep(0.5, 10),
        rep(1.5, 10)
      )
    )
  )
  expect_snapshot(rnc_cv_linear_out$cv_errs)
  expect_equal(
    rnc_cv_linear_out$cv_means,
    apply(rnc_cv_linear_out$cv_errs, 1, mean, na.rm = TRUE)
  )
  expect_equal(
    rnc_cv_linear_out$cv_sds,
    apply(rnc_cv_linear_out$cv_errs, 1, sd, na.rm = TRUE)
  )

  rnc_cv_logistic_out <- rnc_cv(
    x = example_data$x, y = example_data$y_class, A = example_data$A,
    lambdas_netcoh = lambdas_netcoh, lambdas_x = lambdas_x,
    family = "logistic", cv_foldids = cv_foldids
  )
  expect_equal(names(rnc_cv_logistic_out), expected_names)
  expect_snapshot(rnc_cv_logistic_out$alpha)
  expect_snapshot(rnc_cv_logistic_out$beta)
  expect_equal(rnc_cv_logistic_out$lambda_l, 0.05)
  expect_snapshot(rnc_cv_logistic_out$cv_errs)
  expect_equal(
    rnc_cv_logistic_out$cv_means,
    apply(rnc_cv_logistic_out$cv_errs, 1, mean, na.rm = TRUE)
  )
  expect_equal(
    rnc_cv_logistic_out$cv_sds,
    apply(rnc_cv_logistic_out$cv_errs, 1, sd, na.rm = TRUE)
  )

  # check if predictions work
  preds_out <- predict(rnc_cv_linear_out, x = example_data$xtest, A_full = example_data$A_full)
  expect_snapshot(preds_out$y)
  expect_snapshot(preds_out$alpha)

  preds_out <- predict(rnc_cv_logistic_out, x = example_data$xtest, A_full = example_data$A_full)
  expect_snapshot(preds_out$y)
  expect_snapshot(preds_out$alpha)
})
