test_that("nerfplus works", {
  skip_on_ci()
  set.seed(123)
  n <- 50
  lambda_netcoh <- 1
  lambda_embed <- 0.1
  lambda_raw <- 2
  lambda_stump <- 3
  data(example_data)
  example_data$y_class <- ifelse(example_data$y > median(example_data$y), 1, 0)
  example_data$ytest_class <- ifelse(example_data$ytest > median(example_data$y), 1, 0)
  expected_names <- c(
    "rf_fit", "nerfplus_fits", "tree_infos",
    "pre_rf_preprocessing_info", "regularization_params",
    "model_info", "unordered_factors"
  )

  # linear regression
  nerfplus_linear_out <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )
  expect_equal(names(nerfplus_linear_out), expected_names)
  expect_equal(length(nerfplus_linear_out$nerfplus_fits), 500)

  nerfplus_linear_preds_out <- predict(
    nerfplus_linear_out, x = example_data$xtest, A_full = example_data$A_full,
    type = "response"
  )
  expect_snapshot(nerfplus_linear_preds_out)
  nerfplus_linear_alphas_out <- predict(
    nerfplus_linear_out, x = example_data$xtest, A_full = example_data$A_full,
    type = "alpha"
  )
  expect_snapshot(nerfplus_linear_alphas_out)

  # check predictions work with 1 test sample
  nerfplus_linear_pred1 <- predict(
    nerfplus_linear_out, x = example_data$xtest[1, , drop = FALSE],
    A_full = example_data$A_full[1:(nrow(example_data$A) + 1), 1:(nrow(example_data$A) + 1)],
    type = "response"
  )

  # logistic regression
  nerfplus_log_out <- nerfplus(
    x = example_data$x, y = example_data$y_class, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "logistic", embedding = "laplacian", sample_split = "none"
  )
  expect_equal(names(nerfplus_log_out), expected_names)
  expect_equal(length(nerfplus_log_out$nerfplus_fits), 500)

  nerfplus_log_preds_out <- predict(
    nerfplus_log_out, x = example_data$xtest, A_full = example_data$A_full,
    type = "response"
  )
  expect_snapshot(nerfplus_log_preds_out)
  nerfplus_log_alphas_out <- predict(
    nerfplus_log_out, x = example_data$xtest, A_full = example_data$A_full,
    type = "alpha"
  )
  expect_snapshot(nerfplus_log_alphas_out)

  # with oob/inbag
  for (sample_split in c("oob", "inbag")) {
    # linear regression
    nerfplus_linear_out <- nerfplus(
      x = example_data$x, y = example_data$y, A = example_data$A,
      lambda_netcoh = lambda_netcoh,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      family = "linear", embedding = "laplacian", sample_split = sample_split
    )
    expect_equal(names(nerfplus_linear_out), expected_names)
    expect_equal(length(nerfplus_linear_out$nerfplus_fits), 500)

    nerfplus_linear_preds_out <- predict(
      nerfplus_linear_out, x = example_data$xtest, A_full = example_data$A_full,
      type = "response"
    )
    expect_snapshot(nerfplus_linear_preds_out)
    nerfplus_linear_alphas_out <- predict(
      nerfplus_linear_out, x = example_data$xtest, A_full = example_data$A_full,
      type = "alpha"
    )
    expect_snapshot(nerfplus_linear_alphas_out)

    # todo: check feature importances

    # logistic regression
    nerfplus_log_out <- nerfplus(
      x = example_data$x, y = example_data$y_class, A = example_data$A,
      lambda_netcoh = lambda_netcoh,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      family = "logistic", embedding = "laplacian", sample_split = sample_split
    )
    expect_equal(names(nerfplus_log_out), expected_names)
    expect_equal(length(nerfplus_log_out$nerfplus_fits), 500)

    nerfplus_log_preds_out <- predict(
      nerfplus_log_out, x = example_data$xtest, A_full = example_data$A_full,
      type = "response"
    )
    expect_snapshot(nerfplus_log_preds_out)
    nerfplus_log_alphas_out <- predict(
      nerfplus_log_out, x = example_data$xtest, A_full = example_data$A_full,
      type = "alpha"
    )
    expect_snapshot(nerfplus_log_alphas_out)
  }

  # with nodeids
  for (sample_split in c("none", "oob", "inbag")) {
    # linear regression
    nerfplus_linear_out <- nerfplus(
      x = rbind(example_data$x, example_data$x),
      y = c(example_data$y, example_data$y),
      A = example_data$A,
      nodeids = c(1:nrow(example_data$x), 1:nrow(example_data$x)),
      lambda_netcoh = lambda_netcoh,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      family = "linear", embedding = "laplacian", sample_split = sample_split
    )
    expect_equal(names(nerfplus_linear_out), expected_names)
    expect_equal(length(nerfplus_linear_out$nerfplus_fits), 500)

    nerfplus_linear_preds_out <- predict(
      nerfplus_linear_out,
      x = rbind(example_data$x, example_data$xtest, example_data$xtest),
      A_full = example_data$A_full,
      nodeids = c(1:nrow(example_data$A_full), (nrow(example_data$x) + 1):nrow(example_data$A_full)),
      type = "response"
    )
    expect_equal(
      nerfplus_linear_preds_out[(nrow(example_data$x) + 1):(nrow(example_data$x) + nrow(example_data$xtest))],
      nerfplus_linear_preds_out[(nrow(example_data$x) + nrow(example_data$xtest) + 1):length(nerfplus_linear_preds_out)]
    )
    expect_snapshot(nerfplus_linear_preds_out)
    nerfplus_linear_alphas_out <- predict(
      nerfplus_linear_out,
      x = rbind(example_data$x, example_data$xtest, example_data$xtest),
      A_full = example_data$A_full,
      nodeids = c(1:nrow(example_data$A_full), (nrow(example_data$x) + 1):nrow(example_data$A_full)),
      type = "alpha"#, return_all = TRUE
    )
    expect_equal(
      nerfplus_linear_alphas_out[(nrow(example_data$x) + 1):(nrow(example_data$x) + nrow(example_data$xtest))],
      nerfplus_linear_alphas_out[(nrow(example_data$x) + nrow(example_data$xtest) + 1):length(nerfplus_linear_preds_out)]
    )
    expect_snapshot(nerfplus_linear_alphas_out)
  }

  # expect error in logistic regression with nodeids (not yet implemented)
  expect_error(
    nerfplus(
      x = rbind(example_data$x, example_data$x),
      y = c(example_data$y, example_data$y),
      A = example_data$A,
      nodeids = c(1:nrow(example_data$x), 1:nrow(example_data$x)),
      lambda_netcoh = lambda_netcoh,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      family = "logistic", embedding = "laplacian", sample_split = sample_split
    )
  )

  # check with factors in x
  train_idx <- round(seq(1, nrow(iris), length.out = nrow(example_data$x)))
  test_idx <- setdiff(1:nrow(iris), train_idx)[1:nrow(example_data$xtest)]
  x_train <- iris[train_idx, ] |> dplyr::select(-Sepal.Length)
  y_train <- iris[train_idx, ] |> dplyr::pull(Sepal.Length)
  xtest <- iris[test_idx, ] |> dplyr::select(-Sepal.Length)
  ytest <- iris[test_idx, ] |> dplyr::pull(Sepal.Length)
  nerfplus_linear_out <- nerfplus(
    x = x_train, y = y_train, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )
  nerfplus_linear_preds_out <- predict(
    nerfplus_linear_out,
    x = xtest,
    A_full = example_data$A_full,
    type = "response"
  )
  for (imp_method in c("permute", "mdi+", "local")) {
    imp_results <- get_feature_importances(
      nerfplus_linear_out, x = xtest, y = ytest, A_full = example_data$A_full,
      method = imp_method
    )
    print(imp_results)
  }
  expect_equal(
    mean(purrr::map_dbl(nerfplus_linear_out$nerfplus_fits, "intercept")) +
      rowSums(imp_results[, setdiff(colnames(imp_results), ".network")]),
    nerfplus_linear_preds_out,
    tolerance = 1e-4
  )

  # check with unordered factors in x
  nerfplus_linear_out <- nerfplus(
    x = x_train, y = y_train, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none",
    respect.unordered.factors = TRUE
  )
  nerfplus_linear_preds_out <- predict(
    nerfplus_linear_out,
    x = xtest,
    A_full = example_data$A_full,
    type = "response"
  )
  for (imp_method in c("permute", "mdi+", "local")) {
    imp_results <- get_feature_importances(
      nerfplus_linear_out, x = xtest, y = ytest, A_full = example_data$A_full,
      method = imp_method
    )
    print(imp_results)
  }
  expect_equal(
    mean(purrr::map_dbl(nerfplus_linear_out$nerfplus_fits, "intercept")) +
      rowSums(imp_results[, setdiff(colnames(imp_results), ".network")]),
    nerfplus_linear_preds_out,
    tolerance = 1e-4
  )
  # tmp <- dplyr::bind_rows(nerfplus_linear_out$tree_infos)
  # table(tmp$splitvarName)
})

test_that("nerfplus_cv works", {
  skip_on_ci()
  set.seed(123)
  n <- 50
  lambdas_netcoh <- c(1, 2)
  lambdas_embed <- c(0.1, 1)
  lambdas_raw <- c(1, 2)
  lambdas_stump <- c(1, 3)
  data(example_data)
  example_data$y_class <- ifelse(example_data$y > median(example_data$y), 1, 0)
  example_data$ytest_class <- ifelse(example_data$ytest > median(example_data$y), 1, 0)
  expected_names <- c(
    "rf_fit", "nerfplus_fits", "cv_losses", "best_cv_params", "tree_infos",
    "pre_rf_preprocessing_info", "regularization_params",
    "model_info", "unordered_factors"
  )

  # linear regression with cross-validation
  nerfplus_cv_linear_out <- nerfplus_cv(
    x = example_data$x, y = example_data$y, A = example_data$A,
    cv = 5, ntrees_cv = 2,
    lambdas_netcoh = lambdas_netcoh,
    lambdas_embed = lambdas_embed,
    lambdas_raw = lambdas_raw,
    lambdas_stump = lambdas_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  expect_equal(names(nerfplus_cv_linear_out), expected_names)
  expect_snapshot(nerfplus_cv_linear_out$best_cv_params)
  expect_equal(length(nerfplus_cv_linear_out$cv_losses), 2)
  expect_equal(nrow(nerfplus_cv_linear_out$cv_losses[[1]]), 16)
  expect_equal(ncol(nerfplus_cv_linear_out$cv_losses[[1]]), 10)

  # logistic regression with cross-validation
  nerfplus_cv_log_out <- nerfplus_cv(
    x = example_data$x, y = example_data$y_class, A = example_data$A,
    cv = 5, ntrees_cv = 2,
    lambdas_netcoh = lambdas_netcoh,
    lambdas_embed = lambdas_embed,
    lambdas_raw = lambdas_raw,
    lambdas_stump = lambdas_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  expect_equal(names(nerfplus_cv_log_out), expected_names)
  expect_snapshot(nerfplus_cv_log_out$best_cv_params)
  expect_equal(length(nerfplus_cv_log_out$cv_losses), 2)
  expect_equal(nrow(nerfplus_cv_log_out$cv_losses[[1]]), 16)
  expect_equal(ncol(nerfplus_cv_log_out$cv_losses[[1]]), 10)
})
