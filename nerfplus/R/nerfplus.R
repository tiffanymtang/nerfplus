#' Arguments that are shared by multiple functions
#' @name shared_args
#'
#' @param x A numeric matrix or data frame of predictors (features); size n x p.
#'   Should be centered so that each column has mean 0.
#' @param y A numeric vector of responses of length n. Should be centered so
#'   that the mean is 0.
#' @param A An adjacency matrix representing the network structure.
#' @param nodeids (Optional) vector of node IDs of length n.
#'   If provided, node IDs indicate the rows of A, corresponding to each
#'   sample. If not provided, the rows of A are assumed to be in the same order
#'   as the rows of x and y.
#' @param family A character string indicating the type of model to fit.
#'   Currently, only "linear" and "logistic" are supported.
#' @param lambda_netcoh Regularization parameter for the network cohesion term.
#' @param lambda_l (Optional) Regularization parameter for the graph Laplacian.
#'   Default is 0.05.
#' @param lambdas_netcoh Vector or list of regularization parameters for the
#'   network cohesion term.
#' @param lambdas_l (Optional) Vector or list of regularization parameters for
#'   the graph Laplacian.
#' @param cv Number of cross-validation folds. Default is 5.
#' @param cv_foldids (Optional) List of length `cv`, where each component in the
#'   list is a vector of sample indices in that fold. If `NULL` (default),
#'   cross-validation folds will be created randomly.
#' @param refit Logical indicating whether or not to refit tuned model on full
#'   training set after cross-validation. Default is `TRUE`.
#' @param low_dim (Optional) If `TRUE`, the algorithm will use a naive solver
#'   for low-dimensional problems. Default is `NULL`, which will use the navie
#'   low-dimensional solver if the number of covariates is <= 1/5 * the number
#'   of samples. Only used if `family = "linear"`.
#' @param init (Optional) initial values for the optimization algorithm to fit
#'   logistic regression. Ignored for linear regression.
#' @param newton_maxit Maximum number of Newton iterations when fitting
#'   logistic regression. Default is 50. Ignored for linear regression.
#' @param newton_tol Tolerance for convergence of Newton iterations when fitting
#'   logistic regression. Default is 1e-4. Ignored for linear regression.
#' @param verbose Logical indicating whether to print progress messages.
#' @param parallel Logical indicating whether to use parallel processing.
#' @param num.threads Number of threads to use for parallel processing. Default
#'   is 1. Ignored if `parallel = FALSE`.
#'
#' @keywords internal
NULL


#' Fit Network-assisted Random Forest+ (NeRF+)
#'
#' @inheritParams shared_args
#' @param include_raw Logical indicating whether to include the raw covariates
#'   in the NeRF+ model. Default is `TRUE`.
#' @param include_netcoh Logical indicating whether to include the individual
#'   node effects and network cohesion regularization in the NeRF+ model.
#'   Default is `TRUE`.
#' @param embedding Embedding type(s), at least one of "adjacency", "laplacian",
#'   score", or NULL (i.e., do not include any network embedding features).
#'   Alternatively, can directly input an n x d matrix of network embedding
#'   features corresponding to `x`.
#' @param embedding_options A list of options for the network embedding.
#'   Ignored if `embedding = NULL`. If provided, the list should contain the
#'   following components:
#'   - `ndim`: Number of dimensions in the embedding (default is 2).
#'   - `regularization`: Regularization parameter for the adjacency matrix (default is 0.5).
#'   - `varimax`: Whether to apply varimax rotation to the embedding (default is FALSE).
#'   - `center`: Whether to center the embedding so that each column has mean 0 (default is TRUE).
#'   - `scale`: Whether to scale the embedding so that first embedding component column has SD 1 (default is TRUE).
#'     All other embedding components are scaled, proportional to their eigenvalues.
#' @param standardize_x Logical indicating whether to standardize the covariates
#'   so that each column has mean 0 and SD 1. Default is `TRUE`.
#' @param normalize_stump Logical indicating whether to normalize the decision
#'   stump features by number of samples in children nodes. Default is `FALSE`.
#' @param sample_split Character string indicating how to split the samples for
#'   training the model; one of "none" (default), "oob", or "inbag". If "none",
#'   all samples are used for estimating coefficients in NeRF+. If "oob", only
#'   out-of-bag samples are used for estimating coefficients in NeRF+. If
#'   "inbag", only in-bag samples are used for estimating coefficients in NeRF+.
#' @param ntrees Number of trees in ensemble.
#' @param mtry Number of features to consider at each split. Default is the
#'   number of features / 3 for regression and the square root of the number of
#'   features for classification.
#' @param lambda_netcoh Regularization parameter for the network cohesion term.
#'   Can be either a scalar or a vector of length `ntrees`, specifying the
#'   regularization parameter for each tree. Ignored if
#'   `include_netcoh = FALSE`.
#' @param lambda_embed Regularization parameter for the network embedding
#'   features. Default is same as `lambda_raw`. Can be either a scalar or a
#'   vector of length `ntrees`, specifying the regularization parameter for each
#'   tree. Ignored if `embedding = NULL`.
#' @param lambda_raw Regularization parameter for the raw covariates. Default is
#'   same as `lambda_stump`. Can be either a scalar or a vector of length
#'   `ntrees`, specifying the regularization parameter for each tree. Ignored if
#'   `include_raw = FALSE`.
#' @param lambda_stump Regularization parameter for the decision stump features.
#'   Can be either a scalar or a vector of length `ntrees`, specifying the
#'   regularization parameter for each tree.
#' @param lambda_l (Optional) Regularization parameter for the graph Laplacian.
#'   Default is 0.05. Can be either a scalar or a vector of length `ntrees`,
#'   specifying the regularization parameter for each tree.
#' @param ... Additional arguments passed to the [ranger::ranger()] function for
#'   fitting the random forest model.
#'
#' @returns A list containing the following:
#' - `rf_fit`: The fitted random forest model object from [ranger::ranger()].
#' - `nerfplus_fits`: A list of fitted NeRF+ models for each tree in the random
#'   forest. Each element of the list is a fitted model object that can be used
#'   to make predictions.
#' - `tree_infos`: A list of tree information objects for each tree in the
#'   random forest.
#' - `pre_rf_preprocessing_info`: A list containing preprocessing information
#'   for the NeRF+ model; output of [fit_pre_rf_preprocessing()].
#' - `regularization_params`: A list containing the regularization parameters
#'   used in the NeRF+ model
#' - `model_info`: A list containing information about the model, such as
#'   `family`, `include_raw`, `include_netcoh`, `normalize_stump`, and
#'   `sample_split`.
#' - `unordered_factors`: A character vector of variable names that are
#'   unordered factors.
#'
#' @examples
#' data(example_data)
#' nerfplus_out <- nerfplus(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1,
#'   lambda_embed = 0.1,
#'   lambda_raw = 2,
#'   lambda_stump = 3,
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#'
#' @export
nerfplus <- function(x, y, A = NULL, nodeids = NULL,
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
  if (include_raw) {
    if (length(lambda_raw) == 1) {
      lambda_raw <- rep(lambda_raw, ntrees)
    } else if (length(lambda_raw) != ntrees) {
      stop("Length of lambda_raw must be 1 or equal to ntrees.")
    }
  } else {
    lambda_embed <- rep(0, ntrees)
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

  # maybe stsandardize x and add network embedding features prior to fitting RF
  pre_rf_preprocessing_info <- fit_pre_rf_preprocessing(
    x = x,
    A = A,
    standardize = standardize_x,
    embedding = embedding,
    embedding_options = embedding_options,
    nodeids = nodeids
  )
  x <- pre_rf_preprocessing_info$x
  pre_rf_preprocessing_info$x <- NULL

  # fit RF
  train_df <- dplyr::bind_cols(x, .y = y)
  rf_fit <- ranger::ranger(
    formula = .y ~ .,
    data = train_df,
    num.trees = ntrees,
    mtry = mtry,
    num.threads = num.threads,
    keep.inbag = TRUE,
    classification = family == "logistic",
    ...
  )

  # get helpful tree info
  tree_infos <- purrr::map(1:ntrees, ~ ranger::treeInfo(rf_fit, .x))
  node_preds <- stats::predict(
    rf_fit, x, type = "terminalNodes", num.threads = num.threads
  )$predictions |>
    as.data.frame()
  forest_paths <- get_forest_paths(tree_infos)
  unordered_factors <- colnames(x)[!rf_fit$forest$is.ordered]
  inbag_counts <- rf_fit$inbag.counts

  # convert any categorical variables to numeric
  x_numeric <- apply_post_rf_preprocessing(rf_fit, x)

  # fit NeRF+ for each tree
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
      # get psi matrix
      psi_out <- fit_psi(
        x = x_numeric,
        tree_info = tree_info,
        tree_paths = tree_paths,
        node_preds = tree_node_preds,
        unordered_factors = unordered_factors,
        normalize = normalize_stump,
        inbag_counts = tree_inbag_counts
      )
      psi <- psi_out$psi

      # augment psi matrix with original x features
      x_augmented_out <- fit_augmentation(
        x = x,
        psi = psi,
        tree_info = tree_info,
        include_raw = include_raw
      )
      x_augmented <- x_augmented_out$x

      # subset to inbag or oob samples if specified
      if (sample_split == "inbag") {
        keep_idxs <- tree_inbag_counts > 0
      } else if (sample_split == "oob") {
        keep_idxs <- tree_inbag_counts == 0
      } else {
        keep_idxs <- rep(TRUE, length(tree_inbag_counts))
      }
      x_train <- x_augmented[keep_idxs, , drop = FALSE]
      y_train <- y[keep_idxs]
      if (is.null(nodeids)) {
        A_train <- A[keep_idxs, keep_idxs]
        nodeids_train <- NULL
      } else {
        A_train <- A
        nodeids_train <- nodeids[keep_idxs]
      }

      # if no splits in tree, add placeholder/intercept column to x_train
      if (ncol(x_train) == 0) {
        x_train <- cbind(x_train, matrix(1, nrow = nrow(x_train), ncol = 1))
      }

      # get vector of regularization parameters for each column in x
      lambda_x <- get_lambda_x(
        lambda_embed = lam_embed,
        lambda_raw = lam_raw,
        lambda_stump = lam_stump,
        x = x_train,
        psi = psi,
        include_raw = include_raw
      )

      if (include_netcoh) {
        # fit RNC
        fit <- rnc(
          x = x_train, y = y_train, A = A_train, nodeids = nodeids_train,
          family = family,
          lambda_netcoh = lam_netcoh, lambda_x = lambda_x, lambda_l = lam_l
        )
        if (is.null(nodeids_train)) {
          alpha <- rep(NA, nrow(x_augmented))
          alpha[keep_idxs] <- fit$alpha
          fit$alpha <- alpha
          fit$nalpha_train <- nrow(A)
        }
      } else {
        if (all(lambda_x == 0)) {
          # fit unpenalized GLM
          aug_train_df <- dplyr::bind_cols(x_train, .y = y_train)
          if (family == "logistic") {
            fit <- stats::glm(.y ~ ., data = aug_train_df, family = "binomial")
          } else if (family == "linear") {
            fit <- stats::lm(.y ~ ., data = aug_train_df)
          }
        } else {
          # fit regularized GLM
          glmnet_family <- dplyr::case_when(
            family == "linear" ~ "gaussian",
            family == "logistic" ~ "binomial"
          )
          fit <- glmnet_wrapper(
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
      return(fit)
    }
  )

  out <- list(
    rf_fit = rf_fit,
    nerfplus_fits = nerfplus_fits,
    tree_infos = tree_infos,
    pre_rf_preprocessing_info = pre_rf_preprocessing_info,
    regularization_params = list(
      lambda_netcoh = lambda_netcoh,
      lambda_embed = lambda_embed,
      lambda_raw = lambda_raw,
      lambda_stump = lambda_stump,
      lambda_l = lambda_l
    ),
    model_info = list(
      family = family,
      include_raw = include_raw,
      include_netcoh = include_netcoh,
      normalize_stump = normalize_stump,
      sample_split = sample_split
    ),
    unordered_factors = unordered_factors
  )
  class(out) <- "nerfplus"
  return(out)
}


#' Fit Network-assisted Random Forest+ (NeRF+) with Cross-Validation
#'
#' @inheritParams shared_args
#' @inheritParams nerfplus
#' @param ntrees_cv Number of trees that will be tuned using cross-validation.
#'   Default is `ntrees` (i.e., every tree will be tuned). Reduce this number
#'   to speed up the cross-validation process. For all trees that aren't tuned,
#'   the hyperparameter will be chosen randomly from the tuned trees.
#' @param lambdas_netcoh Vector of regularization parameters for the
#'   network cohesion term.
#' @param lambdas_embed Vector of regularization parameters for the
#'   network embedding features. If `NULL`, the regularization parameter
#'   corresponding to the network embedding features will be equal to the
#'   regularization parameter for the raw covariates.
#' @param lambdas_raw Vector of regularization parameters for the raw
#'   covariate features. If `NULL`, the regularization parameter
#'   for the raw covariates will be equal to the regularization parameter
#'   for the decision stump features.
#' @param lambdas_stump Vector of regularization parameters for the
#'   decision stump features.
#' @param lambdas_l Vector of regularization parameters for the graph Laplacian.
#'
#' @returns A list containing the following:
#' - `rf_fit`: The fitted random forest model object from [ranger::ranger()].
#' - `nerfplus_fits`: A list of fitted NeRF+ models for each tree in the random
#'   forest using the tuned hyperparameters. Each element of the list is a
#'   fitted model object that can be used to make predictions.
#' - `cv_losses`: A list of ntrees_cv data frames containing the
#'   cross-validation losses for each tree and each fold. Each item in the list
#'   corresponds to a tree in the random forest. Each row in the data frame
#'   corresponds to a different set of hyperparameters.
#' - `best_cv_params`: A data frame containing the used hyperparameters
#'   for each tree in the random forest.
#' - `tree_infos`: A list of tree information objects for each tree in the
#'   random forest.
#' - `pre_rf_preprocessing_info`: A list containing preprocessing information
#'   for the NeRF+ model; output of [fit_pre_rf_preprocessing()].
#' - `regularization_params`: A list containing the regularization parameters
#'   used in the NeRF+ model
#' - `model_info`: A list containing information about the model, such as
#'   `family`, `include_raw`, `include_netcoh`, `normalize_stump`, and
#'   `sample_split`.
#' - `unordered_factors`: A character vector of variable names that are
#'   unordered factors.
#'
#' @examples
#' \donttest{
#' data(example_data)
#' nerfplus_cv_out <- nerfplus_cv(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambdas_netcoh = c(0.1, 1),
#'   lambdas_embed = c(0, 0.1),
#'   lambdas_raw = c(1, 2),
#'   lambdas_stump = c(1, 2),
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#' }
#'
#' @export
nerfplus_cv <- function(x, y, A = NULL, nodeids = NULL,
                        cv = 5, cv_foldids = NULL,
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
                        standardize_x = TRUE,
                        normalize_stump = FALSE,
                        sample_split = c("none", "oob", "inbag"),
                        ntrees = 500,
                        ntrees_cv = ntrees,
                        mtry = NULL,
                        lambdas_netcoh,
                        lambdas_embed = NULL,
                        lambdas_raw = NULL,
                        lambdas_stump,
                        lambdas_l = 0.05,
                        parallel = FALSE,
                        num.threads = 1,
                        ...) {
  # to avoid no visible binding notes
  lambda_stump <- NULL
  lambda_raw <- NULL
  lambda_embed <- NULL
  lambda_netcoh <- NULL
  lambda_l <- NULL
  tree_id <- NULL

  family <- match.arg(family)
  sample_split <- match.arg(sample_split)
  p <- ncol(x)
  if (is.null(mtry)) {
    mtry <- dplyr::case_when(
      family == "linear" ~ round(p / 3),
      family == "logistic" ~ round(sqrt(p))
    )
  }

  if (parallel) {
    map_fun <- furrr::future_map
    imap_fun <- furrr::future_imap
    pmap_fun <- furrr::future_pmap
  } else {
    map_fun <- purrr::map
    imap_fun <- purrr::imap
    pmap_fun <- purrr::pmap
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

  if (!include_netcoh) {
    lambdas_netcoh <- 0
    lambdas_l <- 0
  }
  if (is.null(embedding) && !is.null(lambdas_embed)) {
    lambdas_embed <- 0
  }
  if (!include_raw && !is.null(lambdas_raw)) {
    lambdas_raw <- 0
  }
  # get gird of hyperparameters
  expand_params <- list(
    lambda_netcoh = lambdas_netcoh,
    lambda_embed = lambdas_embed,
    lambda_raw = lambdas_raw,
    lambda_stump = lambdas_stump,
    lambda_l = lambdas_l
  ) |>
    purrr::compact()
  param_grid <- do.call(
    tidyr::expand_grid, expand_params
  )
  if (is.null(lambdas_raw)) {
    param_grid <- param_grid |>
      dplyr::mutate(
        lambda_raw = lambda_stump
      )
  }
  if (is.null(lambdas_embed)) {
    param_grid <- param_grid |>
      dplyr::mutate(
        lambda_embed = lambda_raw
      )
  }

  # maybe stsandardize x and add network embedding features prior to fitting RF
  pre_rf_preprocessing_info <- fit_pre_rf_preprocessing(
    x = x,
    A = A,
    standardize = standardize_x,
    embedding = embedding,
    embedding_options = embedding_options,
    nodeids = nodeids
  )
  x <- pre_rf_preprocessing_info$x
  pre_rf_preprocessing_info$x <- NULL

  # fit RF
  train_df <- dplyr::bind_cols(x, .y = y)
  rf_fit <- ranger::ranger(
    formula = .y ~ .,
    data = train_df,
    num.trees = ntrees,
    mtry = mtry,
    num.threads = num.threads,
    keep.inbag = TRUE,
    classification = family == "logistic",
    ...
  )

  # get helpful tree info
  tree_infos <- purrr::map(1:ntrees, ~ ranger::treeInfo(rf_fit, .x))
  node_preds <- stats::predict(
    rf_fit, x, type = "terminalNodes", num.threads = num.threads
  )$predictions |>
    as.data.frame()
  forest_paths <- get_forest_paths(tree_infos)
  unordered_factors <- colnames(x)[!rf_fit$forest$is.ordered]
  inbag_counts <- rf_fit$inbag.counts

  # convert any categorical variables to numeric
  x_numeric <- apply_post_rf_preprocessing(rf_fit, x)

  cv_losses <- NULL
  best_cv_params <- NULL
  if (ntrees_cv > 0) {
    # do cv to tune hyperparameters for `ntrees_cv` trees
    cv_out <- pmap_fun(
      list(
        tree_info = tree_infos[1:ntrees_cv],
        tree_node_preds = node_preds[, 1:ntrees_cv],
        tree_paths = forest_paths[1:ntrees_cv],
        tree_inbag_counts = inbag_counts[1:ntrees_cv]
      ),
      function(tree_info, tree_node_preds, tree_paths, tree_inbag_counts) {
        # get psi matrix
        psi_out <- fit_psi(
          x = x_numeric,
          tree_info = tree_info,
          tree_paths = tree_paths,
          node_preds = tree_node_preds,
          unordered_factors = unordered_factors,
          normalize = normalize_stump,
          inbag_counts = tree_inbag_counts
        )
        psi <- psi_out$psi

        # augment psi matrix with original x features
        x_augmented_out <- fit_augmentation(
          x = x,
          psi = psi,
          tree_info = tree_info,
          include_raw = include_raw
        )
        x_augmented <- x_augmented_out$x

        # subset to inbag or oob samples if specified
        if (sample_split == "inbag") {
          keep_idxs <- tree_inbag_counts > 0
        } else if (sample_split == "oob") {
          keep_idxs <- tree_inbag_counts == 0
        } else {
          keep_idxs <- rep(TRUE, length(tree_inbag_counts))
        }
        x_train <- x_augmented[keep_idxs, , drop = FALSE]
        y_train <- y[keep_idxs]
        if (is.null(nodeids)) {
          A_train <- A[keep_idxs, keep_idxs]
          nodeids_train <- NULL
        } else {
          A_train <- A
          nodeids_train <- nodeids[keep_idxs]
        }

        # if no splits in tree, add placeholder/intercept column to x_train
        if (ncol(x_train) == 0) {
          x_train <- cbind(x_train, matrix(1, nrow = nrow(x_train), ncol = 1))
        }

        if (include_netcoh) {
          # do cv for RNC
          cur_param_grid <- param_grid |>
            dplyr::mutate(
              lambda_x = purrr::pmap(
                list(
                  lam_embed = lambda_embed,
                  lam_raw = lambda_raw,
                  lam_stump = lambda_stump
                ),
                function(lam_embed, lam_raw, lam_stump) {
                  lambda_x <- get_lambda_x(
                    lambda_embed = lam_embed,
                    lambda_raw = lam_raw,
                    lambda_stump = lam_stump,
                    x = x_train,
                    psi = psi,
                    include_raw = include_raw
                  )
                }
              )
            ) |>
            dplyr::select(lambda_netcoh, lambda_x, lambda_l)
          cv_fit <- rnc_cv(
            x = x_train, y = y_train, A = A_train, nodeids = nodeids_train,
            lambda_grid = cur_param_grid,
            family = family, cv = cv, cv_foldids = cv_foldids, refit = FALSE
          )

          # reformat cv losses and best parameters
          cv_losses <- cv_fit$lambda_grid |>
            invert_lambda_x(x = x_train) |>
            dplyr::bind_cols(
              as.data.frame(cv_fit$cv_errs) |>
                stats::setNames(paste0("Fold", 1:ncol(cv_fit$cv_errs)))
            )
          cv_best_params <- cv_fit$best_params |>
            invert_lambda_x(x = x_train)
        } else {
          # do cv for glmnet
          glmnet_family <- dplyr::case_when(
            family == "linear" ~ "gaussian",
            family == "logistic" ~ "binomial"
          )
          response_type <- dplyr::case_when(
            family == "linear" ~ "response",
            family == "logistic" ~ "class"
          )

          if (is.null(lambdas_embed) && is.null(lambdas_raw)) {
            fit <- glmnet_wrapper(
              x = x_train, y = y_train, alpha = 0, family = glmnet_family,
              lambda = param_grid$lambda_stump / ncol(x_train),
              nfolds = cv, cv = TRUE
            )
            cv_losses <- fit$cvm
            cv_best_params <- param_grid[which.min(cv_losses)[1], ]
          } else {
            cv_losses <- matrix(NA, nrow = nrow(param_grid), ncol = cv)
            cv_foldids <- sample(
              rep(1:cv, length.out = nrow(x_train)),
              size = nrow(x_train)
            )
            if (ncol(x_train) > 1) {
              for (k in 1:cv) {
                train_idx <- cv_foldids != k
                valid_idx <- cv_foldids == k
                for (param_idx in 1:nrow(param_grid)) {
                  lambda_x <- get_lambda_x(
                    lambda_embed = param_grid$lambda_embed[[param_idx]],
                    lambda_raw = param_grid$lambda_raw[[param_idx]],
                    lambda_stump = param_grid$lambda_stump[[param_idx]],
                    x = x_train,
                    psi = psi,
                    include_raw = include_raw
                  )
                  fold_fit <- glmnet_wrapper(
                    x = x_train[train_idx, , drop = FALSE],
                    y = y_train[train_idx],
                    alpha = 0, family = glmnet_family,
                    lambda = sum(lambda_x) / ncol(x_train)^2,
                    penalty.factor = lambda_x
                  )
                  fold_preds <- stats::predict(
                    fold_fit,
                    x_train[valid_idx, , drop = FALSE],
                    type = response_type
                  )[, 1]
                  if (family == "logistic") {
                    cv_losses[param_idx, k] <- yardstick::roc_auc_vec(
                      factor(y_train[valid_idx], levels = c(1, 0)),
                      fold_preds
                    )
                  } else {
                    cv_losses[param_idx, k] <- yardstick::rmse_vec(
                      y_train[valid_idx], fold_preds
                    )
                  }
                }
              }
              # reformat cv losses and best parameters
              if (family == "logistic") {
                cv_best_params <- param_grid[which.max(rowMeans(cv_losses)), ]
              } else {
                cv_best_params <- param_grid[which.min(rowMeans(cv_losses)), ]
              }
              cv_losses <- param_grid |>
                dplyr::bind_cols(
                  as.data.frame(cv_losses) |>
                    stats::setNames(paste0("Fold", 1:ncol(cv_losses)))
                )
            } else {
              cv_losses <- NULL
              cv_best_params <- tibble::tibble(
                lambda_netcoh = NA,
                lambda_raw = NA,
                lambda_stump = NA,
                lambda_embed = NA,
                lambda_l = NA
              )
            }
          }
        }
        return(list(best_params = cv_best_params, cv_losses = cv_losses))
      }
    )
    best_cv_params <- purrr::map(cv_out, "best_params") |>
      purrr::list_rbind() |>
      tibble::rownames_to_column("tree_id") |>
      dplyr::mutate(
        dplyr::across(
          c(lambda_embed, lambda_raw),
          ~ tidyr::replace_na(.x, 1e12)
        )
      )
    cv_losses <- purrr::map(cv_out, "cv_losses")

    # randomize all other trees
    tuned_tree_idxs <- which(!is.na(best_cv_params$lambda_stump))
    rand_tree_idxs <- sample(
      tuned_tree_idxs, size = ntrees, replace = TRUE
    )
    rand_tree_idxs[tuned_tree_idxs] <- tuned_tree_idxs
    best_cv_params <- best_cv_params[rand_tree_idxs, ] |>
      dplyr::mutate(tree_id = 1:ntrees) |>
      dplyr::select(
        tree_id, lambda_netcoh, lambda_embed, lambda_raw, lambda_stump, lambda_l
      )
  } else {
    # no cross-validation, use random parameters
    best_cv_params <- tibble::tibble(
      tree_id = 1:ntrees,
      lambda_netcoh = sample(
        unique(param_grid$lambda_netcoh), ntrees, replace = TRUE
      ),
      lambda_embed = sample(
        unique(param_grid$lambda_embed), ntrees, replace = TRUE
      ),
      lambda_raw = sample(
        unique(param_grid$lambda_raw), ntrees, replace = TRUE
      ),
      lambda_stump = sample(
        unique(param_grid$lambda_stump), ntrees, replace = TRUE
      ),
      lambda_l = sample(
        unique(param_grid$lambda_l), ntrees, replace = TRUE
      )
    )
  }

  # fit NeRF+ for each tree using best hyperparameters
  nerfplus_fits <- pmap_fun(
    list(
      tree_id = 1:ntrees,
      tree_info = tree_infos,
      tree_node_preds = node_preds,
      tree_paths = forest_paths,
      tree_inbag_counts = inbag_counts
    ),
    function(tree_id, tree_info, tree_node_preds, tree_paths, tree_inbag_counts) {
      # get psi matrix
      psi_out <- fit_psi(
        x = x_numeric,
        tree_info = tree_info,
        tree_paths = tree_paths,
        node_preds = tree_node_preds,
        unordered_factors = unordered_factors,
        normalize = normalize_stump,
        inbag_counts = tree_inbag_counts
      )
      psi <- psi_out$psi

      # augment psi matrix with original x features
      x_augmented_out <- fit_augmentation(
        x = x,
        psi = psi,
        tree_info = tree_info,
        include_raw = include_raw
      )
      x_augmented <- x_augmented_out$x

      # subset to inbag or oob samples if specified
      if (sample_split == "inbag") {
        keep_idxs <- tree_inbag_counts > 0
      } else if (sample_split == "oob") {
        keep_idxs <- tree_inbag_counts == 0
      } else {
        keep_idxs <- rep(TRUE, length(tree_inbag_counts))
      }
      x_train <- x_augmented[keep_idxs, , drop = FALSE]
      y_train <- y[keep_idxs]
      if (is.null(nodeids)) {
        A_train <- A[keep_idxs, keep_idxs]
        nodeids_train <- NULL
      } else {
        A_train <- A
        nodeids_train <- nodeids[keep_idxs]
      }

      # if no splits in tree, add placeholder/intercept column to x_train
      if (ncol(x_train) == 0) {
        x_train <- cbind(x_train, matrix(1, nrow = nrow(x_train), ncol = 1))
      }

      best_lam_netcoh <- best_cv_params$lambda_netcoh[[tree_id]]
      best_lam_embed <- best_cv_params$lambda_embed[[tree_id]]
      best_lam_raw <- best_cv_params$lambda_raw[[tree_id]]
      best_lam_stump <- best_cv_params$lambda_stump[[tree_id]]
      lambda_x <- get_lambda_x(
        lambda_embed = best_lam_embed,
        lambda_raw = best_lam_raw,
        lambda_stump = best_lam_stump,
        x = x_train,
        psi = psi,
        include_raw = include_raw
      )
      if (include_netcoh) {
        # fit RNC
        best_lam_l <- best_cv_params$lambda_l[[tree_id]]
        fit <- rnc(
          x = x_train, y = y_train, A = A_train, nodeids = nodeids_train,
          family = family,
          lambda_netcoh = best_lam_netcoh, lambda_x = lambda_x,
          lambda_l = best_lam_l
        )
        if (is.null(nodeids_train)) {
          alpha <- rep(NA, nrow(x_augmented))
          alpha[keep_idxs] <- fit$alpha
          fit$alpha <- alpha
          fit$nalpha_train <- nrow(A)
        }
      } else {
        # fit regularized GLM
        glmnet_family <- dplyr::case_when(
          family == "linear" ~ "gaussian",
          family == "logistic" ~ "binomial"
        )
        if (is.null(lambdas_embed) && is.null(lambdas_raw)) {
          fit <- glmnet_wrapper(
            x = x_train, y = y_train, alpha = 0, family = glmnet_family,
            lambda = best_lam_stump / ncol(x_train)
          )
        } else {
          fit <- glmnet_wrapper(
            x = x_train, y = y_train, alpha = 0, family = glmnet_family,
            lambda = sum(lambda_x) / ncol(x_train)^2,
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
      return(fit)
    }
  )

  out <- list(
    rf_fit = rf_fit,
    nerfplus_fits = nerfplus_fits,
    cv_losses = cv_losses,
    best_cv_params = best_cv_params,
    tree_infos = tree_infos,
    pre_rf_preprocessing_info = pre_rf_preprocessing_info,
    regularization_params = list(
      lambdas_netcoh = lambdas_netcoh,
      lambdas_embed = lambdas_embed,
      lambdas_raw = lambdas_raw,
      lambdas_stump = lambdas_stump,
      lambdas_l = lambdas_l
    ),
    model_info = list(
      family = family,
      include_raw = include_raw,
      include_netcoh = include_netcoh,
      normalize_stump = normalize_stump,
      sample_split = sample_split,
      ntrees_cv = ntrees_cv
    ),
    unordered_factors = unordered_factors
  )
  class(out) <- c("nerfplus", "nerfplus_cv")
  return(out)
}


#' @keywords internal
predict_tree <- function(tree_object, x, A_full, nodeids = NULL,
                         alpha = NULL, beta = NULL, preds = NULL,
                         type = c("response", "alpha")) {
  type <- match.arg(type)
  if (type == "response") {
    if (!is.null(preds)) {
      preds <- preds
    } else if (!is.null(alpha) && !is.null(beta)) {
      # } else if (!is.null(intercept_alpha) && !is.null(alpha) && !is.null(beta)) {
      # TODO: check that the column names in x and names of beta align
      preds <- tree_object$intercept + alpha + as.matrix(x) %*% beta
      if (tree_object$family == "logistic") {
        preds <- 1 / (1 + exp(-preds))
      }
    } else if ("rnc" %in% class(tree_object)) {
      preds <- stats::predict(tree_object, x, A_full, nodeids)$y
    } else if (any(c("glmnet", "cv.glmnet") %in% class(tree_object))) {
      preds <- stats::predict(tree_object, x, type = "response")
    } else {
      preds <- stats::predict(tree_object, as.data.frame(x), type = "response")
    }
  } else if (type == "alpha") {
    if (!is.null(alpha)) {
      preds <- alpha
    } else if ("rnc" %in% class(tree_object)) {
      preds <- stats::predict(tree_object, x, A_full, nodeids)$alpha
    } else {
      stop("No such type = 'alphas' when trained with use_netcoh = FALSE.")
    }
  }
  if (isTRUE(ncol(preds) == 1)) {
    preds <- c(preds)
  }
  return(preds)
}


#' Predict method for a NeRF+ model
#'
#' @param object A fitted NeRF+ model object.
#' @param x A data frame or matrix of new data for which predictions are to be
#'   made.
#' @param x_embed Optional embedding data frame or matrix, whose rows are
#'   aligned with those in `x`. If provided, it will be used to augment the
#'   input `x` data. Only needed if training embeddings were manually inputted.
#' @param A_full An adjacency matrix representing the network structure for
#'   the full set of nodes (training + testing nodes in that order). Note: if
#'   `nrow(x) == nrow(A_full)`, then `x` is assumed to be the training data.
#' @param nodeids (Optional) vector of node IDs of length equal to nrows in `x`.
#'   If provided, node IDs indicate the rows of A_full, corresponding to each
#'   sample. If not provided, the rows of A_full are assumed to be in the order
#'   of (x_train, x).
#' @param type Type of prediction to return; one of "response" (default) or
#'   "alpha". If "response", the predicted values are returned. If "alpha",
#'   the estimated individual node effects are returned.
#' @param return_all If `TRUE`, returns a list of predictions for each tree in
#'   the ensemble. If `FALSE` (default), returns the average prediction across
#'   all trees.
#'
#' @returns If `return_all = FALSE`, this function returns a vector of predicted
#'   values (if `type = "response"`) or estimated individual node effects (if
#'   `type = "alpha"`). If `return_all = TRUE`, this function returns a list of
#'   predicted values or estimated individual node effects, where each
#'   element in the list corresponds to a different tree in the ensemble.
#'
#' @examples
#' data(example_data)
#' nerfplus_out <- nerfplus(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1,
#'   lambda_embed = 0.1,
#'   lambda_raw = 2,
#'   lambda_stump = 3,
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#' predicted_y <- predict(
#'   nerfplus_out, x = example_data$xtest, A_full = example_data$A_full,
#'   type = "response"
#' )
#' estimated_alphas <- predict(
#'   nerfplus_out, x = example_data$xtest, A_full = example_data$A_full,
#'   type = "alpha"
#' )
#'
#' @export
predict.nerfplus <- function(object, x, x_embed = NULL, A_full, nodeids = NULL,
                             type = c("response", "alpha"),
                             return_all = FALSE) {
  type <- match.arg(type)

  rf_fit <- object$rf_fit
  nerfplus_fits <- object$nerfplus_fits
  tree_infos <- object$tree_infos
  pre_rf_preprocessing_info <- object$pre_rf_preprocessing_info
  include_raw <- object$model_info$include_raw
  unordered_factors <- object$unordered_factors

  x <- apply_pre_rf_preprocessing(
    pre_rf_preprocessing_info, x = x, x_embed = x_embed, A_full = A_full,
    nodeids = nodeids
  )
  x_numeric <- apply_post_rf_preprocessing(rf_fit, x)

  node_preds <- stats::predict(
    rf_fit, x, type = "terminalNodes", num.threads = 1
  )$predictions |>
    as.data.frame()
  forest_paths <- get_forest_paths(tree_infos)

  preds_ls <- purrr::pmap(
    list(
      tree_fit = nerfplus_fits,
      tree_info = tree_infos,
      tree_node_preds = node_preds,
      tree_paths = forest_paths
    ),
    function(tree_fit, tree_info, tree_node_preds, tree_paths) {
      psi <- apply_psi(
        x = x_numeric,
        tree_info = tree_info,
        tree_paths = tree_paths,
        node_preds = tree_node_preds,
        unordered_factors = unordered_factors,
        psi_unique_values = tree_fit$preprocessing_info$psi_unique_values
      )
      x_augmented <- apply_augmentation(
        x = x,
        psi = psi,
        tree_info = tree_info,
        include_raw = include_raw,
        dummy_fit = tree_fit$preprocessing_info$dummy_fit
      )
      if (ncol(x_augmented) == 0) {
        x_augmented <- cbind(
          x_augmented,
          matrix(1, nrow = nrow(x_augmented), ncol = 1)
        )
      }
      predict_tree(
        tree_object = tree_fit, x = x_augmented, A_full = A_full,
        nodeids = nodeids, type = type
      )
    }
  )

  if (!return_all) {
    preds <- purrr::reduce(preds_ls, `+`) / rf_fit$num.trees
    if (isTRUE(ncol(preds) == 1)) {
      preds <- c(preds)
    }
  } else {
    preds <- preds_ls
  }
  return(preds)
}
