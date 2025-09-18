nerfplus_loo_method_fun <- function(x, y, x_test, y_test, A, A_full,
                                    max_n = Inf,
                                    verbose_data_out = NULL,
                                    classification = FALSE,
                                    include_netcoh = TRUE,
                                    cv = FALSE,
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
                                    ...) {
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

  if (cv) {
    # use cv to choose appropriate regularization parameters, for which to run loo
    cv_fit <- nerfplus::nerfplus_cv(
      x = x, y = y, A = A, family = family, include_netcoh = include_netcoh,
      embedding = embedding, embedding_options = embedding_options,
      standardize_x = standardize,
      lambdas_netcoh = lambda_netcoh, lambdas_embed = lambda_embed,
      lambdas_raw = lambda_raw, lambdas_stump = lambda_stump, lambdas_l = lambda_l,
      nodeids = verbose_data_out$nodeids,
      ...
    )
    best_cv_params <- cv_fit$best_cv_params[1:cv_fit$model_info$ntrees_cv, ] |>
      dplyr::summarise(
        dplyr::across(
          tidyselect::starts_with("lambda"),
          ~ table(.x) |>
            which.max() |>
            names() |>
            as.numeric()
        )
      )
    lambda_netcoh <- best_cv_params$lambda_netcoh[[1]]
    lambda_embed <- best_cv_params$lambda_embed[[1]]
    lambda_raw <- best_cv_params$lambda_raw[[1]]
    lambda_stump <- best_cv_params$lambda_stump[[1]]
    lambda_l <- best_cv_params$lambda_l[[1]]
  }

  # fit NeRF+ model
  fit <- nerfplus::nerfplus(
    x = x, y = y, A = A, family = family, include_netcoh = include_netcoh,
    embedding = embedding, embedding_options = embedding_options,
    standardize_x = standardize,
    lambda_netcoh = lambda_netcoh, lambda_embed = lambda_embed,
    lambda_raw = lambda_raw, lambda_stump = lambda_stump, lambda_l = lambda_l,
    nodeids = verbose_data_out$nodeids,
    ...
  )

  # make original predictions
  preds_ls <- predict(
    fit, x = x_test, x_embed = oos_embedding, A_full = A_full,
    nodeids = verbose_data_out$nodeids_test, return_all = TRUE
  )

  # approximate loo
  cat("Approximate LOO...\n")
  start_time <- Sys.time()
  aloo_out <- nerfplus::get_loo(
    fit, x = x, x_embed = x_embed, y = y, A = A,
    xtest = x_test, xtest_embed = oos_embedding, ytest = y_test, A_full = A_full
  )
  end_time <- Sys.time()
  aloo_time <- end_time - start_time
  print(aloo_time)

  # true loo with fixed trees, embeddings, networks, hyperparameters, etc.
  cat("True LOO with fixed trees...\n")
  start_time <- Sys.time()
  nerfplus_loo_fixed_out <- nerfplus_loo_partial(
    fit, x = x, x_test = x_test, y = y, y_test = y_test, A = A, A_full = A_full,
    max_n = max_n, family = family,
    include_netcoh = include_netcoh,
    embedding = embedding, embedding_options = embedding_options,
    standardize_x = standardize,
    lambda_netcoh = lambda_netcoh, lambda_embed = lambda_embed,
    lambda_raw = lambda_raw, lambda_stump = lambda_stump, lambda_l = lambda_l,
    nodeids = verbose_data_out$nodeids,
    nodeids_test = verbose_data_out$nodeids_test, ...
  )
  end_time <- Sys.time()
  fixed_time <- end_time - start_time
  print(fixed_time)

  # true loo
  cat("True LOO...\n")
  start_time <- Sys.time()
  nerfplus_loo_out <- nerfplus_loo_full(
    x = x, x_test = x_test, y = y, y_test = y_test, A = A, A_full = A_full,
    max_n = max_n, family = family,
    include_netcoh = include_netcoh,
    embedding = embedding, oos_embedding = oos_embedding,
    embedding_options = embedding_options,
    standardize_x = standardize,
    lambda_netcoh = lambda_netcoh, lambda_embed = lambda_embed,
    lambda_raw = lambda_raw, lambda_stump = lambda_stump, lambda_l = lambda_l,
    nodeids = verbose_data_out$nodeids,
    nodeids_test = verbose_data_out$nodeids_test, ...
  )
  end_time <- Sys.time()
  true_time <- end_time - start_time
  print(true_time)

  end_time <- Sys.time()
  return(list(
    y_train = y,
    y_test = y_test,
    alpha_ls = purrr::map(fit, "alpha"),
    beta_ls = purrr::map(fit, "beta"),
    preds_ls = preds_ls,
    aloo_out = aloo_out,
    nerfplus_loo_fixed_out = nerfplus_loo_fixed_out,
    nerfplus_loo_out = nerfplus_loo_out,
    aloo_time = aloo_time,
    fixed_time = fixed_time,
    true_time = true_time
  ))
}


#' NeRF+ LOO with fixed trees, embeddings, networks, etc.
nerfplus_loo_partial <- function(fit, x, x_test, y, y_test, A = NULL, A_full = NULL,
                                 max_n = Inf,
                                 nodeids = NULL, nodeids_test = NULL,
                                 family = c("linear", "logistic"),
                                 include_raw = TRUE,
                                 include_netcoh = TRUE,
                                 embedding = NULL,
                                 embedding_options = list(
                                   ndim = 2,
                                   regularization = 0.5,
                                   varimax = FALSE,
                                   center = TRUE,
                                   scale = TRUE
                                 ),
                                 x_embed = NULL,
                                 x_test_embed = NULL,
                                 standardize_x = TRUE,
                                 normalize_stump = FALSE,
                                 sample_split = c("none", "oob", "inbag"),
                                 ntrees = 500,
                                 mtry = NULL,
                                 lambda_netcoh,
                                 lambda_embed = lambda_raw,
                                 lambda_raw = lambda_stump,
                                 lambda_stump,
                                 lambda_l = 0.05,
                                 parallel = FALSE,
                                 num.threads = 1,
                                 ...) {

  family <- match.arg(family)
  sample_split <- match.arg(sample_split)
  p <- ncol(x)
  if (is.null(mtry)) {
    mtry <- dplyr::case_when(
      family == "linear" ~ round(p / 3),
      family == "logistic" ~ round(sqrt(p))
    )
  }

  n <- nrow(x)
  pre_rf_preprocessing_info <- fit$pre_rf_preprocessing_info
  rf_fit <- fit$rf_fit
  ntrees <- rf_fit$num.trees
  include_raw <- fit$model_info$include_raw
  unordered_factors <- fit$unordered_factors
  inbag_counts <- rf_fit$inbag.counts

  if (parallel) {
    map_fun <- furrr::future_map
    imap_fun <- furrr::future_imap
    pmap_fun <- furrr::future_pmap
  } else {
    map_fun <- purrr::map
    imap_fun <- purrr::imap
    pmap_fun <- purrr::pmap
  }
  if (include_netcoh) {
    if (length(lambda_netcoh) == 1) {
      lambda_netcoh <- rep(lambda_netcoh, ntrees)
    } else if (length(lambda_netcoh) != ntrees) {
      stop("Length of lambda_netcoh must be 1 or equal to ntrees.")
    }
  } else {
    lambda_netcoh <- rep(0, ntrees)
  }
  if (!is.null(embedding)) {
    if (length(lambda_embed) == 1) {
      lambda_embed <- rep(lambda_embed, ntrees)
    } else if (length(lambda_embed) != ntrees) {
      stop("Length of lambda_embed must be 1 or equal to ntrees.")
    }
  } else {
    lambda_embed <- rep(0, ntrees)
  }
  if (length(lambda_raw) == 1) {
    lambda_raw <- rep(lambda_raw, ntrees)
  } else if (length(lambda_raw) != ntrees) {
    stop("Length of lambda_raw must be 1 or equal to ntrees.")
  }
  if (length(lambda_stump) == 1) {
    lambda_stump <- rep(lambda_stump, ntrees)
  } else if (length(lambda_stump) != ntrees) {
    stop("Length of lambda_stump must be 1 or equal to ntrees.")
  }
  if (length(lambda_l) == 1) {
    lambda_l <- rep(lambda_l, ntrees)
  } else if (length(lambda_l) != ntrees) {
    stop("Length of lambda_l must be 1 or equal to ntrees.")
  }
  if (".y" %in% colnames(x)) {
    stop("Column name '.y' is reserved for the outcome variable. Please ensure that '.y' is not a column name in x.'")
  }
  if (any(stringr::str_starts(colnames(x), ".embed"))) {
    stop("Column names starting with '.embed' are reserved for the embedding variables. Please ensure that no column names in x start with '.embed'.")
  }
  if (any(stringr::str_starts(colnames(x), ".network"))) {
    stop("Please ensure that no column names in x start with '.network'.")
  }
  if (any(stringr::str_starts(colnames(x), ".alpha"))) {
    stop("Please ensure that no column names in x start with '.alpha'.")
  }

  x <- nerfplus:::apply_pre_rf_preprocessing(
    pre_rf_preprocessing_info, x = x, x_embed = x_embed,
    A_full = A, nodeids = nodeids
  )
  x_numeric <- nerfplus:::apply_post_rf_preprocessing(rf_fit, x)

  x_test <- nerfplus:::apply_pre_rf_preprocessing(
    pre_rf_preprocessing_info, x = x_test, x_embed = x_test_embed,
    A_full = A_full, nodeids = nodeids_test
  )
  x_test_numeric <- nerfplus:::apply_post_rf_preprocessing(rf_fit, x_test)

  tree_infos <- purrr::map(1:ntrees, ~ ranger::treeInfo(rf_fit, .x))
  node_preds <- predict(
    rf_fit, x, type = "terminalNodes", num.threads = num.threads
  )$predictions |>
    as.data.frame()
  node_preds_test <- predict(
    rf_fit, x_test, type = "terminalNodes", num.threads = 1
  )$predictions |>
    as.data.frame()
  forest_paths <- nerfplus:::get_forest_paths(tree_infos)

  loo_out <- list()
  for (i in 1:min(n, max_n)) {
    if (i %% 10 == 0) {
      cat(i, "\n")
    }
    nerfplus_fits <- pmap_fun(
      list(
        tree_info = tree_infos,
        tree_node_preds = node_preds,
        tree_paths = forest_paths,
        tree_inbag_counts = inbag_counts,
        lam_netcoh = lambda_netcoh,
        lam_embed = lambda_embed,
        lam_raw = lambda_raw,
        lam_stump = lambda_stump,
        lam_l = lambda_l
      ),
      function(tree_info, tree_node_preds, tree_paths, tree_inbag_counts,
               lam_netcoh, lam_embed, lam_raw, lam_stump, lam_l) {
        psi_out <- nerfplus:::fit_psi(
          x = x_numeric,
          tree_info = tree_info,
          tree_paths = tree_paths,
          node_preds = tree_node_preds,
          unordered_factors = unordered_factors,
          normalize = normalize_stump,
          inbag_counts = tree_inbag_counts
        )
        psi <- psi_out$psi

        x_augmented_out <- nerfplus:::fit_augmentation(
          x = x,
          psi = psi,
          tree_info = tree_info,
          include_raw = include_raw
        )
        x_augmented <- x_augmented_out$x

        if (sample_split == "inbag") {
          tree_inbag_counts_i <- tree_inbag_counts
          tree_inbag_counts_i[i] <- -1
          inbag_idxs <- tree_inbag_counts > 0
          x_train <- x_augmented[inbag_idxs, , drop = FALSE]
          y_train <- y[inbag_idxs]
          A_train <- A[inbag_idxs, inbag_idxs]
          nodeids_train <- nodeids[inbag_idxs]
        } else if (sample_split == "oob") {
          tree_inbag_counts_i <- tree_inbag_counts
          tree_inbag_counts_i[i] <- -1
          x_train <- x_augmented[tree_inbag_counts_i == 0, , drop = FALSE]
          y_train <- y[tree_inbag_counts_i == 0]
          A_train <- A[tree_inbag_counts_i == 0, tree_inbag_counts_i == 0]
          nodeids_train <- nodeids[tree_inbag_counts_i == 0]
        } else {
          x_train <- x_augmented[-i, ]
          y_train <- y[-i]
          A_train <- A[-i, -i]
          nodeids_train <- nodeids[-i]
        }
        A_full_loo <- matrix(NA, nrow = nrow(A_train) + 1, ncol = ncol(A_train) + 1)
        A_full_loo[1:nrow(A_train), 1:ncol(A_train)] <- A_train
        A_full_loo[nrow(A_full_loo), 1:ncol(A_train)] <- A[i, colnames(A_train)]
        A_full_loo[1:nrow(A_train), ncol(A_full_loo)] <- A[rownames(A_train), i]
        A_full_loo[nrow(A_full_loo), ncol(A_full_loo)] <- A[i, i]
        nodeids_train_loo <- c(nodeids_train, nodeids[i])

        lambda_x <- nerfplus:::get_lambda_x(
          lambda_embed = lam_embed,
          lambda_raw = lam_raw,
          lambda_stump = lam_stump,
          x = x_train,
          psi = psi,
          include_raw = include_raw
        )

        if (include_netcoh) {
          fit <- nerfplus::rnc(
            x = x_train, y = y_train, A = A_train,
            nodedegrees = rowSums(A)[-i],
            nodeids = nodeids_train,
            family = family,
            lambda_netcoh = lam_netcoh * (n - 1)^2 / n^2,
            lambda_x = lambda_x,
            lambda_l = lam_l
          )
        } else {
          if (all(lambda_x == 0)) {
            aug_train_df <- dplyr::bind_cols(x_train, .y = y_train)
            if (family == "logistic") {
              fit <- glm(.y ~ ., data = aug_train_df, family = "binomial")
            } else if (family == "linear") {
              fit <- lm(.y ~ ., data = aug_train_df)
            }
          } else {
            glmnet_family <- dplyr::case_when(
              family == "linear" ~ "gaussian",
              family == "logistic" ~ "binomial"
            )
            fit <- nerfplus:::glmnet_wrapper(
              x = x_train, y = y_train, alpha = 0, family = glmnet_family,
              lambda = sum(lambda_x) / ncol(x_train),
              penalty.factor = lambda_x
            )
          }
        }

        fit$family <- family
        fit$preprocessing_info <- list(
          psi_unique_values = psi_out$psi_unique_values,
          dummy_fit = x_augmented_out$dummy_fit,
          x_train_means = colMeans(x_train)
        )
        fit$loo_tree_pred <- nerfplus:::predict_tree(
          tree_object = fit, x = x_augmented[i, , drop = FALSE], A_full = A_full_loo,
          nodeids = nodeids_train_loo
        )
        return(fit)
      }
    )

    preds_ls <- purrr::pmap(
      list(
        tree_fit = nerfplus_fits,
        tree_info = tree_infos,
        tree_node_preds = node_preds_test,
        tree_paths = forest_paths
      ),
      function(tree_fit, tree_info, tree_node_preds, tree_paths) {
        psi <- nerfplus:::apply_psi(
          x = x_test_numeric,
          tree_info = tree_info,
          tree_paths = tree_paths,
          node_preds = tree_node_preds,
          unordered_factors = unordered_factors,
          psi_unique_values = tree_fit$preprocessing_info$psi_unique_values
        )
        x_augmented <- nerfplus:::apply_augmentation(
          x = x_test,
          psi = psi,
          tree_info = tree_info,
          include_raw = include_raw,
          dummy_fit = tree_fit$preprocessing_info$dummy_fit
        )
        nerfplus:::predict_tree(
          tree_object = tree_fit, x = x_augmented, A_full = A_full[-i, -i],
          nodeids = nodeids, type = "response"
        )
      }
    )

    loo_out[[i]] <- list(
      loo_pred = purrr::reduce(purrr::map(nerfplus_fits, "loo_tree_pred"), `+`) /
        length(nerfplus_fits),
      preds_ls = preds_ls,
      alphas_ls = purrr::map(nerfplus_fits, "alpha"),
      betas_ls = purrr::map(nerfplus_fits, "beta")
    )
  }

  return(loo_out)
}


#' NeRF+ LOO with non-fixed trees, embeddings, networks, etc
nerfplus_loo_full <- function(x, x_test, y, y_test, A = NULL, A_full = NULL,
                              max_n = Inf,
                              nodeids = NULL, nodeids_test = NULL,
                              family = c("linear", "logistic"),
                              include_raw = TRUE,
                              include_netcoh = TRUE,
                              embedding = NULL,
                              oos_embedding = NULL,
                              embedding_options = list(
                                ndim = 2,
                                regularization = 0.5,
                                varimax = FALSE,
                                center = TRUE,
                                scale = TRUE
                              ),
                              standardize_x = TRUE,
                              normalize_stump = FALSE,
                              sample_split = c("none", "oob", "inbag"),
                              ntrees = 500,
                              mtry = NULL,
                              lambda_netcoh,
                              lambda_embed = lambda_raw,
                              lambda_raw = lambda_stump,
                              lambda_stump,
                              lambda_l = 0.05,
                              parallel = FALSE,
                              num.threads = 1, ...) {
  loo_out <- list()
  for (i in 1:min(nrow(x), max_n)) {
    if (i %% 10 == 0) {
      cat(i, "\n")
    }
    x_i <- x[-i, ]
    y_i <- y[-i]
    A_i <- A[-i, -i]
    A_full_i <- A_full[-i, -i]
    nodeids_i <- nodeids[-i]
    fit <- nerfplus::nerfplus(
      x = x_i, y = y_i, A = A_i, nodeids = nodeids_i,
      family = family, include_raw = include_raw, include_netcoh = include_netcoh,
      embedding = embedding, embedding_options = embedding_options,
      standardize_x = standardize_x,
      normalize_stump = normalize_stump,
      sample_split = sample_split, ntrees = ntrees, mtry = mtry,
      lambda_netcoh = lambda_netcoh * (nrow(x) - 1)^2 / nrow(x)^2,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      lambda_l = lambda_l,
      parallel = parallel, num.threads = num.threads,
      ...
    )
    A_full_loo <- matrix(NA, nrow = nrow(A_i) + 1, ncol = ncol(A_i) + 1)
    A_full_loo[1:nrow(A_i), 1:ncol(A_i)] <- A_i
    A_full_loo[nrow(A_full_loo), 1:ncol(A_i)] <- A[i, -i]
    A_full_loo[1:nrow(A_i), ncol(A_full_loo)] <- A[-i , i]
    A_full_loo[nrow(A_full_loo), ncol(A_full_loo)] <- A[i, i]
    loo_pred <- predict(
      fit, x = x[i, , drop = FALSE], x_embed = oos_embedding[i, , drop = FALSE],
      A_full = A_full_loo, nodeids = c(nodeids_i, nodeids[i])
    )
    preds_ls <- predict(
      fit, x = x_test, x_embed = oos_embedding, A_full = A_full_i,
      nodeids = nodeids_test, return_all = TRUE
    )

    loo_out[[i]] <- list(
      loo_pred = loo_pred,
      preds_ls = preds_ls,
      alphas_ls = purrr::map(fit$nerfplus_fits, "alpha"),
      betas_ls = purrr::map(fit$nerfplus_fits, "beta")
    )
  }
  return(loo_out)
}
