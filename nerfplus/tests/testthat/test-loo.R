test_that("get_loo works for linear regression", {
  skip_on_ci()
  set.seed(123)
  n <- 50
  lambda_netcoh <- 1
  lambda_embed <- 0.1
  lambda_raw <- 2
  lambda_stump <- 3
  data(example_data)

  # Fit a NeRF+ model
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  # Test LOO on training data
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A
  )

  # Test LOO with test data
  loo_test_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A,
    xtest = example_data$xtest,
    ytest = example_data$ytest,
    A_full = example_data$A_full
  )

  # Fit a NeRF+ model with sample split != "none"
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "inbag"
  )

  # Test LOO on training data with sample split != "none"
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A,
    xtest = example_data$xtest,
    ytest = example_data$ytest,
    A_full = example_data$A_full
  )

  # Fit a NeRF+ model without network cohesion sample split != "none"
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    include_netcoh = FALSE,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "inbag"
  )

  # Test LOO on training data with sample split != "none" and no netcoh
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A,
    xtest = example_data$xtest,
    ytest = example_data$ytest,
    A_full = example_data$A_full
  )
})

test_that("get_loo works for logistic regression", {
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

  # Fit a NeRF+ model
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y_class, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "logistic", embedding = "laplacian", sample_split = "none"
  )

  # Test LOO on training data
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y_class,
    A = example_data$A
  )

  # Test LOO with test data
  loo_test_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y_class,
    A = example_data$A,
    xtest = example_data$xtest,
    ytest = example_data$ytest_class,
    A_full = example_data$A_full
  )
})

test_that("get_loo handles custom metric", {
  skip_on_ci()
  set.seed(123)
  n <- 50
  lambda_netcoh <- 1
  lambda_embed <- 0.1
  lambda_raw <- 2
  lambda_stump <- 3
  data(example_data)

  # Fit a NeRF+ model
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  # Custom metric function
  custom_metric <- function(truth, estimate) {
    mean(abs(truth - estimate))
  }

  # Test LOO with custom metric
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A,
    metric = custom_metric
  )
})

test_that("get_loo handles return_all parameter", {
  skip_on_ci()
  set.seed(123)
  n <- 50
  lambda_netcoh <- 1
  lambda_embed <- 0.1
  lambda_raw <- 2
  lambda_stump <- 3
  data(example_data)

  # Fit a NeRF+ model
  nerfplus_fit <- nerfplus(
    x = example_data$x, y = example_data$y, A = example_data$A,
    lambda_netcoh = lambda_netcoh,
    lambda_embed = lambda_embed,
    lambda_raw = lambda_raw,
    lambda_stump = lambda_stump,
    family = "linear", embedding = "laplacian", sample_split = "none"
  )

  # Test LOO with return_all = TRUE
  loo_out <- get_loo(
    object = nerfplus_fit,
    x = example_data$x,
    y = example_data$y,
    A = example_data$A,
    return_all = TRUE
  )
})
