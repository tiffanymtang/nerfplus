lm_method_fun <- function(x, y, x_test, y_test, A = NULL, A_full = NULL,
                          verbose_data_out = NULL,
                          classification = FALSE,
                          standardize = TRUE,
                          embedding = NULL,
                          embedding_options = list(
                            ndim = 2,
                            regularization = 0.5,
                            varimax = FALSE,
                            center = TRUE,
                            scale = TRUE
                          ),
                          importance_modes = NULL,
                          importance_options = list(
                            B = 10,
                            metric = NULL
                          ),
                          return_features = NULL,
                          return_fit = FALSE,
                          return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out, standardize = standardize,
    embedding = embedding, embedding_options = embedding_options
  )
  x_orig_colnames <- preprocess_out$x_orig_colnames
  x <- preprocess_out$x
  x_test <- preprocess_out$x_test

  # fit model
  train_df <- dplyr::bind_cols(x, .y = y)
  if (!classification) {
    fit <- lm(.y ~ ., data = train_df)
    preds <- predict(fit, x_test)
  } else {
    fit <- glm(.y ~ ., data = train_df, family = "binomial")
    preds <- predict(fit, x_test, type = "response")
  }

  # evaluate importances
  if (!is.null(importance_modes)) {
    x_means <- apply(x, 2, mean)
    grouped_features <- get_grouped_features(x_orig_colnames, colnames(x))
    fi_out <- evaluate_fi(
      fit, x = x_test, y = y_test,
      x_means = x_means,
      classification = classification,
      methods = importance_modes,
      grouped_features = grouped_features,
      B = importance_options$B,
      metric = importance_options$metric
    )
  } else {
    fi_out <- NULL
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (!is.null(embedding)) {
    out[["embedding"]] <- x |> dplyr::select(starts_with(".embed"))
    out[["oos_embedding"]] <- x_test |> dplyr::select(starts_with(".embed"))
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = fi_out$global, local_importance = fi_out$local,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


rf_method_fun <- function(x, y, x_test, y_test, A = NULL, A_full = NULL,
                          verbose_data_out = NULL,
                          ntrees = 500, mtry = NULL, num.threads = 1,
                          classification = FALSE,
                          standardize = TRUE,
                          embedding = NULL,
                          embedding_options = list(
                            ndim = 2,
                            regularization = 0.5,
                            varimax = FALSE,
                            center = TRUE,
                            scale = TRUE
                          ),
                          importance_modes = NULL,
                          importance_options = list(
                            B = 10,
                            metric = NULL
                          ),
                          return_features = NULL,
                          return_fit = FALSE,
                          return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out,
    standardize = standardize, dummy_code = FALSE,
    embedding = embedding, embedding_options = embedding_options
  )
  # x_orig_colnames <- preprocess_out$x_orig_colnames
  x <- preprocess_out$x
  x_test <- preprocess_out$x_test

  if (is.null(mtry)) {
    if (classification) {
      mtry <- sqrt(ncol(x))
    } else {
      mtry <- ncol(x) / 3
    }
  }

  # fit models and evaluate importances
  train_df <- dplyr::bind_cols(x, .y = y)
  if (is.null(importance_modes)) {
    fit <- ranger::ranger(
      formula = .y ~ .,
      data = train_df,
      num.trees = ntrees,
      mtry = mtry,
      num.threads = num.threads,
      classification = classification,
      ...
    )
    fi_out <- NULL
  } else {
    importance_modes <- intersect(sort(importance_modes), c("mdi+", "permute"))
    fit_ls <- purrr::map(
      importance_modes,
      function(importance_mode) {
        if (importance_mode == "permute") {
          importance <- "permutation"
        } else if (importance_mode == "mdi+") {
          importance <- "impurity"
        }
        ranger::ranger(
          formula = .y ~ .,
          data = train_df,
          num.trees = ntrees,
          mtry = mtry,
          num.threads = num.threads,
          classification = classification,
          importance = importance,
          ...
        )
      }
    ) |>
      setNames(sort(importance_modes))
    fit <- fit_ls[[1]]
    fi_out <- purrr::map(
      names(fit_ls),
      ~ as.data.frame(fit_ls[[.x]]$variable.importance) |>
        tibble::rownames_to_column("var") |>
        setNames(c("var", .x))
    ) |>
      purrr::reduce(dplyr::left_join, by = "var")
  }
  # make predictions
  if (classification) {
    preds <- predict(
      fit, x_test, predict.all = TRUE, num.threads = 1
    )$predictions |>
      rowMeans()
  } else {
    preds <- predict(fit, x_test, num.threads = 1)$predictions
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (!is.null(embedding)) {
    out[["embedding"]] <- x |> dplyr::select(starts_with(".embed"))
    out[["oos_embedding"]] <- x_test |> dplyr::select(starts_with(".embed"))
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = fi_out, local_importance = NULL,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


bart_method_fun <- function(x, y, x_test, y_test, A = NULL, A_full = NULL,
                            verbose_data_out = NULL,
                            classification = FALSE,
                            standardize = TRUE,
                            embedding = NULL,
                            embedding_options = list(
                              ndim = 2,
                              regularization = 0.5,
                              varimax = FALSE,
                              center = TRUE,
                              scale = TRUE
                            ),
                            return_features = NULL,
                            return_fit = FALSE,
                            return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out,
    standardize = standardize, dummy_code = FALSE,
    embedding = embedding, embedding_options = embedding_options
  )
  x <- preprocess_out$x
  x_test <- preprocess_out$x_test

  # fit model
  if (!classification) {
    fit <- BART::wbart(
      x.train = x, y.train = y, x.test = x_test, ...
    )
    preds <- fit$yhat.test.mean
  } else {
    fit <- BART::pbart(
      x.train = x, y.train = y, x.test = x_test, ...
    )
    preds <- colMeans(pnorm(fit$yhat.test))
  }
  end_time <- Sys.time()

  out <- list()
  if (!is.null(embedding)) {
    out[["embedding"]] <- x |> dplyr::select(starts_with(".embed"))
    out[["oos_embedding"]] <- x_test |> dplyr::select(starts_with(".embed"))
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = NULL, local_importance = NULL,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


nerfplus_method_fun <- function(x, y, x_test, y_test, A, A_full,
                                verbose_data_out = NULL,
                                classification = FALSE,
                                include_netcoh = TRUE,
                                lambda_netcoh,
                                lambda_embed = lambda_stump,
                                lambda_raw = lambda_stump,
                                lambda_stump,
                                lambda_l = 0.05,
                                standardize = TRUE,
                                embedding = NULL,
                                embedding_options = list(
                                  ndim = 2,
                                  regularization = 0.5,
                                  varimax = FALSE,
                                  center = TRUE,
                                  scale = TRUE
                                ),
                                importance_modes = NULL,
                                importance_options = list(
                                  B = 10,
                                  metric = NULL
                                ),
                                loo = FALSE,
                                return_features = NULL,
                                return_alpha = FALSE,
                                return_fit = FALSE,
                                return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test
  if (!is.null(embedding) && !is.character(embedding)) {
    oos_embedding <- embedding[(nrow(x) + 1):(nrow(embedding)), , drop = FALSE]
    embedding <- embedding[1:nrow(x), , drop = FALSE]
    x_embed <- embedding
  } else {
    oos_embedding <- NULL
    x_embed <- NULL
  }

  family <- dplyr::case_when(
    classification ~ "logistic",
    TRUE ~ "linear"
  )

  # fit model
  fit <- nerfplus::nerfplus(
    x = x, y = y, A = A, family = family, include_netcoh = include_netcoh,
    embedding = embedding, embedding_options = embedding_options,
    standardize_x = standardize,
    lambda_netcoh = lambda_netcoh, lambda_embed = lambda_embed,
    lambda_raw = lambda_raw, lambda_stump = lambda_stump, lambda_l = lambda_l,
    nodeids = verbose_data_out$nodeids,
    ...
  )

  # make predictions
  preds <- predict(
    fit, x = x_test, x_embed = oos_embedding, A_full = A_full,
    nodeids = verbose_data_out$nodeids_test
  )

  # evaluate importances
  if (!is.null(importance_modes)) {
    fis <- purrr::map(
      importance_modes,
      function(importance_mode) {
        fi_out <- nerfplus::get_feature_importances(
          fit, x = x_test, x_embed = oos_embedding, y = y_test, A_full = A_full,
          nodeids = verbose_data_out$nodeids_test,
          method = importance_mode,
          B = importance_options$B,
          metric = importance_options$metric
        )
        if (importance_mode == "local") {
          global_fi <- NULL
          local_fi <- fi_out
        } else {
          global_fi <- fi_out |> setNames(c("var", importance_mode))
          local_fi <- NULL
        }
        return(list(global = global_fi, local = local_fi))
      }
    ) |>
      setNames(importance_modes)
    globalfi_out <- purrr::map(fis, "global") |>
      purrr::compact()
    if (length(globalfi_out) > 0) {
      globalfi_out <- purrr::reduce(globalfi_out, dplyr::left_join, by = "var")
    }
    localfi_out <- purrr::map(fis, "local") |>
      purrr::compact()
  } else {
    globalfi_out <- NULL
    localfi_out <- NULL
  }

  # evaluate loo
  aloo_out <- NULL
  if (loo) {
    aloo_out <- nerfplus::get_loo(
      fit, x = x, x_embed = x_embed, y = y, A = A, 
      xtest = x_test, xtest_embed = oos_embedding, ytest = y_test, A_full = A_full
    )
    keep_aloo_names <- c(
      "mean_change_alphas", 
      "mean_change_betas", 
      "mean_change_error_test", 
      "mean_change_preds_test"
    )
    aloo_out <- aloo_out[keep_aloo_names]
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (return_alpha) {
    alphas <- predict(
      fit, x = x_test, x_embed = oos_embedding, A_full = A_full,
      type = "alpha", return_all = TRUE,
      nodeids = verbose_data_out$nodeids_test
    )
    out[["alphas"]] <- alphas
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = globalfi_out, local_importance = localfi_out,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  out[["loo"]] <- aloo_out
  return(out)
}


nerfplus_cv_method_fun <- function(x, y, x_test, y_test, A, A_full,
                                   verbose_data_out = NULL,
                                   classification = FALSE,
                                   include_netcoh = TRUE,
                                   lambdas_netcoh = 1,
                                   lambdas_embed = NULL,
                                   lambdas_raw = NULL,
                                   lambdas_stump = 1,
                                   lambdas_l = 0.05,
                                   standardize = TRUE,
                                   embedding = NULL,
                                   embedding_options = list(
                                     ndim = 2,
                                     regularization = 0.5,
                                     varimax = FALSE,
                                     center = TRUE,
                                     scale = TRUE
                                   ),
                                   importance_modes = NULL,
                                   importance_options = list(
                                     B = 10,
                                     metric = NULL
                                   ),
                                   loo = FALSE,
                                   return_features = NULL,
                                   return_alpha = FALSE,
                                   return_fit = FALSE,
                                   return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test
  if (!is.null(embedding) && !is.character(embedding)) {
    oos_embedding <- embedding[(nrow(x) + 1):(nrow(embedding)), , drop = FALSE]
    embedding <- embedding[1:nrow(x), , drop = FALSE]
    x_embed <- embedding
  } else {
    oos_embedding <- NULL
    x_embed <- NULL
  }

  family <- dplyr::case_when(
    classification ~ "logistic",
    TRUE ~ "linear"
  )

  # fit model
  fit <- nerfplus::nerfplus_cv(
    x = x, y = y, A = A, family = family, include_netcoh = include_netcoh,
    embedding = embedding, embedding_options = embedding_options,
    standardize_x = standardize,
    lambdas_netcoh = lambdas_netcoh, lambdas_embed = lambdas_embed,
    lambdas_raw = lambdas_raw, lambdas_stump = lambdas_stump, lambdas_l = lambdas_l,
    nodeids = verbose_data_out$nodeids,
    ...
  )

  # make predictions
  preds <- predict(
    fit, x = x_test, x_embed = oos_embedding, A_full = A_full,
    nodeids = verbose_data_out$nodeids_test
  )

  # evaluate importances
  if (!is.null(importance_modes)) {
    fis <- purrr::map(
      importance_modes,
      function(importance_mode) {
        fi_out <- nerfplus::get_feature_importances(
          fit, x = x_test, x_embed = oos_embedding, y = y_test, A_full = A_full,
          nodeids = verbose_data_out$nodeids_test,
          method = importance_mode,
          B = importance_options$B,
          metric = importance_options$metric
        )
        if (importance_mode == "local") {
          global_fi <- NULL
          local_fi <- fi_out
        } else {
          global_fi <- fi_out |> setNames(c("var", importance_mode))
          local_fi <- NULL
        }
        return(list(global = global_fi, local = local_fi))
      }
    ) |>
      setNames(importance_modes)
    globalfi_out <- purrr::map(fis, "global") |>
      purrr::compact()
    if (length(globalfi_out) > 0) {
      globalfi_out <- purrr::reduce(globalfi_out, dplyr::left_join, by = "var")
    }
    localfi_out <- purrr::map(fis, "local") |>
      purrr::compact()
  } else {
    globalfi_out <- NULL
    localfi_out <- NULL
  }

  # evaluate loo
  aloo_out <- NULL
  if (loo) {
    aloo_out <- nerfplus::get_loo(
      fit, x = x, x_embed = x_embed, y = y, A = A, 
      xtest = x_test, xtest_embed = oos_embedding, ytest = y_test, A_full = A_full
    )
    keep_aloo_names <- c(
      "mean_change_alphas", 
      "mean_change_betas", 
      "mean_change_error_test", 
      "mean_change_preds_test"
    )
    aloo_out <- aloo_out[keep_aloo_names]
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (return_alpha) {
    alphas <- predict(
      fit, x = x_test, x_embed = oos_embedding, A_full = A_full,
      type = "alphas", return_all = TRUE,
      nodeids = verbose_data_out$nodeids_test,
    )
    out[["alphas"]] <- alphas
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = globalfi_out, local_importance = localfi_out,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  out[["loo"]] <- aloo_out
  return(out)
}


rnc_method_fun <- function(x, y, x_test, y_test, A, A_full,
                           verbose_data_out = NULL,
                           classification = FALSE,
                           lambda_netcoh = 1,
                           lambda_raw = 1,
                           lambda_l = 0.05,
                           standardize = TRUE,
                           embedding = NULL,
                           embedding_options = list(
                             ndim = 2,
                             regularization = 0.5,
                             varimax = FALSE,
                             center = TRUE,
                             scale = TRUE
                           ),
                           importance_modes = NULL,
                           importance_options = list(
                             B = 10,
                             metric = NULL
                           ),
                           return_features = NULL,
                           return_alpha = FALSE,
                           return_fit = FALSE,
                           return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out, standardize = standardize,
    embedding = embedding, embedding_options = embedding_options
  )
  x_orig_colnames <- preprocess_out$x_orig_colnames
  x <- as.matrix(preprocess_out$x)
  x_test <- as.matrix(preprocess_out$x_test)

  family <- dplyr::case_when(
    classification ~ "logistic",
    TRUE ~ "linear"
  )

  # fit model
  fit <- nerfplus::rnc(
    x = x, y = y, A = A, nodeids = verbose_data_out$nodeids,
    lambda_netcoh = lambda_netcoh, lambda_x = lambda_raw, lambda_l = lambda_l,
    family = family, ...
  )

  # make predictions
  preds <- c(
    predict(
      fit, x = x_test, A_full = A_full, nodeids = verbose_data_out$nodeids_test
    )$y
  )

  # evaluate importances
  if (!is.null(importance_modes)) {
    x_means <- apply(x, 2, mean)
    grouped_features <- get_grouped_features(x_orig_colnames, colnames(x))
    fi_out <- evaluate_fi(
      fit, x = x_test, y = y_test, A_full = A_full,
      nodeids = verbose_data_out$nodeids_test,
      x_means = x_means,
      classification = classification,
      methods = importance_modes,
      grouped_features = grouped_features,
      B = importance_options$B,
      metric = importance_options$metric
    )
  } else {
    fi_out <- NULL
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (!is.null(embedding)) {
    out[["embedding"]] <- x |> dplyr::select(starts_with(".embed"))
    out[["oos_embedding"]] <- x_test |> dplyr::select(starts_with(".embed"))
  }
  if (return_alpha) {
    alphas <- predict(
      fit, x_test, A_full, nodeids = verbose_data_out$nodeids_test
    )$alpha
    out[["alphas"]] <- alphas
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = fi_out$global, local_importance = fi_out$local,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


rnc_cv_method_fun <- function(x, y, x_test, y_test, A, A_full,
                              verbose_data_out = NULL,
                              classification = FALSE,
                              lambdas_netcoh = 1,
                              lambdas_raw = 1,
                              lambdas_l = 0.05,
                              standardize = TRUE,
                              embedding = NULL,
                              embedding_options = list(
                                ndim = 2,
                                regularization = 0.5,
                                varimax = FALSE,
                                center = TRUE,
                                scale = TRUE
                              ),
                              importance_modes = NULL,
                              importance_options = list(
                                B = 10,
                                metric = NULL
                              ),
                              return_features = NULL,
                              return_alpha = FALSE,
                              return_fit = FALSE,
                              return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out, standardize = standardize,
    embedding = embedding, embedding_options = embedding_options
  )
  x_orig_colnames <- preprocess_out$x_orig_colnames
  x <- as.matrix(preprocess_out$x)
  x_test <- as.matrix(preprocess_out$x_test)

  family <- dplyr::case_when(
    classification ~ "logistic",
    TRUE ~ "linear"
  )

  # fit model
  fit <- nerfplus::rnc_cv(
    x = x, y = y, A = A, nodeids = verbose_data_out$nodeids,
    lambdas_netcoh = lambdas_netcoh, lambdas_x = lambdas_raw,
    lambdas_l = lambdas_l, family = family, ...
  )

  # make predictions
  preds <- c(
    predict(
      fit, x = x_test, A_full = A_full, nodeids = verbose_data_out$nodeids_test
    )$y
  )

  # evaluate importances
  if (!is.null(importance_modes)) {
    x_means <- apply(x, 2, mean)
    grouped_features <- get_grouped_features(x_orig_colnames, colnames(x))
    fi_out <- evaluate_fi(
      fit, x = x_test, y = y_test, A_full = A_full,
      nodeids = verbose_data_out$nodeids_test,
      x_means = x_means,
      classification = classification,
      methods = importance_modes,
      grouped_features = grouped_features,
      B = importance_options$B,
      metric = importance_options$metric
    )
  } else {
    fi_out <- NULL
  }
  end_time <- Sys.time()

  # return values
  out <- list()
  if (!is.null(embedding)) {
    out[["embedding"]] <- x |> dplyr::select(starts_with(".embed"))
    out[["oos_embedding"]] <- x_test |> dplyr::select(starts_with(".embed"))
  }
  if (return_alpha) {
    alphas <- c(
      predict(
        fit, x = x_test, A_full = A_full,
        nodeids = verbose_data_out$nodeids_test
      )$alpha
    )
    out[["alphas"]] <- alphas
  }
  out <- return_method_output(
    out = out, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = fi_out$global, local_importance = fi_out$local,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


network_bart_fun <- function(x, y, x_test, y_test, A, A_full,
                             verbose_data_out = NULL,
                             classification = FALSE,
                             standardize = TRUE,
                             embedding = NULL,
                             embedding_options = list(
                               ndim = 2,
                               regularization = 0.5,
                               varimax = FALSE,
                               center = TRUE,
                               scale = TRUE
                             ),
                             unif_cuts = FALSE,
                             verbose = FALSE,
                             return_features = NULL,
                             return_fit = FALSE,
                             return_data = FALSE, ...) {
  start_time <- Sys.time()
  x_orig <- x
  x_test_orig <- x_test

  # preprocess data
  preprocess_out <- preprocess_fun(
    x = x, x_test = x_test, A = A, A_full = A_full,
    verbose_data_out = verbose_data_out, standardize = standardize,
    embedding = embedding, embedding_options = embedding_options
  )
  x_orig_colnames <- preprocess_out$x_orig_colnames
  x <- preprocess_out$x
  x_test <- preprocess_out$x_test

  if (is.null(verbose_data_out$nodeids)) {
    vertex_id_train <- 1:nrow(x)
  } else {
    vertex_id_train <- verbose_data_out$nodeids
  }
  if (is.null(verbose_data_out$nodeids_test)) {
    vertex_id_test <- (nrow(x) + 1):(nrow(A_full))
  } else {
    vertex_id_test <- verbose_data_out$nodeids_test
  }

  if (isFALSE(unif_cuts)) {
    unif_cuts <- rep(FALSE, ncol(x))
    cutpoints_list <- purrr::map(x, ~ sort(unique(.x)))
  } else {
    unif_cuts <- rep(TRUE, ncol(x))
    cutpoints_list <- NULL
  }

  # fit model
  fit <- flexBART::network_BART(
    Y_train = y,
    vertex_id_train = vertex_id_train,
    X_cont_train = as.matrix(x),
    vertex_id_test = vertex_id_test,
    X_cont_test = as.matrix(x_test),
    A = A_full,
    unif_cuts = unif_cuts,
    cutpoints_list = cutpoints_list,
    verbose = verbose,
    ...
  )

  # make predictions
  preds <- c(fit$yhat.test.mean)
  end_time <- Sys.time()

  # return values
  out <- return_method_output(
    out = NULL, x = x_orig, y = y, x_test = x_test_orig, y_test = y_test,
    A = A, A_full = A_full, fit = fit, verbose_data_out = verbose_data_out,
    predictions = preds, classification = classification,
    importance = NULL, local_importance = NULL,
    time_elapsed = difftime(end_time, start_time, units = "secs"),
    return_features = return_features, return_fit = return_fit,
    return_data = return_data
  )
  return(out)
}


#' Helper function to save the data in simChef pipeline
save_data_fun <- function(...) {
  return(list(.data = list(...)))
}
