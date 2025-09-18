#' Simulate from additive blockwise network DGP
#'
#' @param n Number of samples.
#' @param p Number of features.
#' @param f A function that takes a matrix X and additional arguments
#' @param x_fun Function to generate the features X. If NULL, X is generated
#'   from a standard normal distribution.
#' @param network_fun Function to generate the network adjacency matrix.
#' @param network_args A list of additional arguments to pass to the
#'   network function.
#' @param centroids_scale Scale of the centroids for each block.
#' @param X_sd Standard deviation of the features.
#' @param err_sd Standard deviation of the errors. Ignored if `pve` is provided.
#' @param pve Proportion of variance explained (optional).
#' @param train_prop Proportion of data to use for training (default is 0.8).
#' @param return_details Logical indicating if additional details should be
#'   returned (default is FALSE).
#' @param ... Additional arguments to pass to the function `f`.
#'
#' @returns A list containing the training and test data, the adjacency matrix
#'   `A`, the full adjacency matrix `A_full`, and optionally the alphas and
#'   block IDs if `return_details = TRUE`.
additive_blockwise_network_dgp_fun <- function(n, p, f, x_fun = NULL,
                                               network_fun, network_args = NULL,
                                               centroids_scale = 1,
                                               X_sd = 1,
                                               err_sd = 1, pve = NULL,
                                               n_outliers = 0, outliers_scale = 3,
                                               train_prop = 0.8,
                                               return_details = FALSE, ...) {

  # generate X
  if (is.null(x_fun)) {
    X <- matrix(rnorm(n * p, mean = 0, sd = X_sd), nrow = n, ncol = p)
  } else {
    X <- x_fun(n, p)
  }
  X <- scale(X, center = TRUE, scale = FALSE)

  # generate network
  network <- R.utils::doCall(network_fun, args = c(list(n = n), network_args))
  A <- network$A
  block_ids <- network$block_ids

  # generate alphas
  alphas <- generate_alphas(block_ids = block_ids, scale = centroids_scale)
  centroids <- sort(unique(alphas))

  # generate y
  y <- alphas + f(X, ...)

  # add errors
  if (!is.null(pve)) {
    # choose err_sd to meet proportion of variance explained
    err_sd <- sqrt(var(y) * ((1 - pve) / pve))
  }
  errs <- rnorm(n, sd = err_sd)
  y <- y + errs
  y <- scale(y, center = TRUE, scale = FALSE)

  if (return_details) {
    out <- load_data(
      X = X, y = y, A = A, alphas = alphas, block_ids = block_ids,
      train_prop = train_prop
    )
  } else {
    out <- load_data(X = X, y = y, A = A, train_prop = train_prop)
  }

  # add outliers (if specified)
  if (n_outliers > 0) {
    y_sd <- sd(out$y)
    out$y[1:n_outliers] <- dplyr::case_when(
      out$y[1:n_outliers] > 0 ~ out$y[1:n_outliers] + outliers_scale * y_sd,
      TRUE ~ out$y[1:n_outliers] - outliers_scale * y_sd
    )
    out$verbose_data_out$outlier_idxs <- 1:n_outliers
  }

  return(out)
}


#' Simulate from network autocorrelation DGP
#'
#' @param n Number of samples.
#' @param p Number of features.
#' @param f A function that takes a matrix X and additional arguments
#' @param x_fun Function to generate the features X. If NULL, X is generated
#'   from a standard normal distribution.
#' @param network_fun Function to generate the network adjacency matrix.
#' @param network_args A list of additional arguments to pass to the
#'   network function.
#' @param omega Parameter controlling the strength of network autocorrelation.
#'   Should be between -1 and 1.
#' @param X_sd Standard deviation of the features.
#' @param err_sd Standard deviation of the errors. Ignored if `pve` is provided.
#' @param pve Proportion of variance explained (optional).
#' @param train_prop Proportion of data to use for training (default is 0.8).
#' @param return_details Logical indicating if additional details should be
#'   returned (default is FALSE).
#' @param ... Additional arguments to pass to the function `f`.
#'
#' @returns A list containing the training and test data, the adjacency matrix
#'   `A`, the full adjacency matrix `A_full`, and optionally the alphas and
#'   block IDs if `return_details = TRUE`.
network_autocorrelation_dgp_fun <- function(n, p, f, x_fun = NULL,
                                            network_fun, network_args = NULL,
                                            omega = 0.5,
                                            X_sd = 1,
                                            err_sd = 1, pve = NULL,
                                            n_outliers = 0, outliers_scale = 3,
                                            train_prop = 0.8,
                                            return_details = FALSE, ...) {

  # generate X
  if (is.null(x_fun)) {
    X <- matrix(rnorm(n * p, mean = 0, sd = X_sd), nrow = n, ncol = p)
  } else {
    X <- x_fun(n, p)
  }
  X <- scale(X, center = TRUE, scale = FALSE)

  # generate network
  network <- R.utils::doCall(network_fun, args = c(list(n = n), network_args))
  A <- network$A

  # generate y
  I <- diag(n)
  D_inv <- diag(1 / rowSums(A))
  autocor_mat <- solve(I - omega * D_inv %*% A)
  y <- autocor_mat %*% f(X, ...)

  # add errors
  if (!is.null(pve)) {
    # choose err_sd to meet proportion of variance explained
    err_sd <- sqrt(var(y) * ((1 - pve) / pve))
  }
  errs <- autocor_mat %*% rnorm(n, sd = err_sd)
  y <- y + errs
  y <- scale(y, center = TRUE, scale = FALSE)

  if (return_details) {
    out <- load_data(X = X, y = y, A = A, train_prop = train_prop)
  } else {
    out <- load_data(X = X, y = y, A = A, train_prop = train_prop)
  }

  # add outliers (if specified)
  if (n_outliers > 0) {
    y_sd <- sd(out$y)
    out$y[1:n_outliers] <- dplyr::case_when(
      out$y[1:n_outliers] > 0 ~ out$y[1:n_outliers] + outliers_scale * y_sd,
      TRUE ~ out$y[1:n_outliers] - outliers_scale * y_sd
    )
    out$verbose_data_out$outlier_idxs <- 1:n_outliers
  }

  return(out)
}


#' Generate a linear function of the features
#'
#' @param X A matrix of features.
#' @param s Number of features to use in the linear combination.
#' @param beta Coefficients for the linear combination. If a single value is
#'   provided, it is repeated `s` times. If a vector is provided, its length
#'   must match `s`.
#'
#' @returns A vector of length `nrow(X)`.
f_linear <- function(X, s, beta = 1) {
  if (length(beta) == 1) {
    beta <- rep(beta, s)
  } else if (length(beta) != s) {
    stop("Length of beta must match s")
  }
  y <- X[, 1:s, drop = FALSE] %*% beta
  return(y)
}


#' Generate locally spiky sparse function of the features
#'
#' @param X A matrix of features.
#' @param m Number of blocks.
#' @param k Number of features per block.
#' @param beta Coefficients for the linear combination. If a single value is
#'   provided, it is repeated `m` times. If a vector is provided, its length
#'   must match `m`.
#'
#' @returns A vector of length `nrow(X)`.
f_lss <- function(X, m, k, beta = 1) {
  if (length(beta) == 1) {
    beta <- rep(beta, m)
  } else if (length(beta) != m) {
    stop("Length of beta must match m")
  }
  X_lss <- purrr::map(
    1:m,
    ~ apply(X[, ((.x - 1) * k + 1):(.x * k)], 1, function(x) all(x > 0))
  )
  X_lss <- do.call(cbind, X_lss)
  y <- X_lss %*% beta
  return(y)
}


#' Generate hierarchical polynomial function of the features
#'
#' @param X A matrix of features.
#' @param m Number of blocks.
#' @param k Number of features per block.
#' @param beta Coefficients for the polynomial terms. If a single value is
#'   provided, it is repeated `m * k` times. If a vector is provided, its length
#'   must match `m * k`.
#'
#' @returns A vector of length `nrow(X)`.
f_hier_poly <- function(X, m, k, beta = 1) {
  if (length(beta) == 1) {
    beta <- rep(beta, m * k)
  } else if (length(beta) != m * k) {
    stop("Length of beta must match m * k")
  }
  X_poly <- purrr::map(
    1:m,
    function(i) {
      purrr::map(
        1:k,
        function(j) {
          apply(
            X[, ((i - 1) * k + 1):((i - 1) * k + j), drop = FALSE],
            1,
            function(x) prod(x)
          )
        }
      )
    }
  ) |>
    purrr::flatten()
  X_poly <- do.call(cbind, X_poly)
  y <- X_poly %*% beta
  return(y)
}


#' Load real data
#'
#' @param n Number of samples to subsample
#' @param p Number of features to subsample
#' @param data_fname File path to data
#'
#' @returns Matrix containing n rows/samples and p columns/features
load_real_data <- function(n = NULL, p = NULL,
                           data_fname = here::here("data", "ccle_prot.csv"), ...) {
  data <- data.table::fread(data_fname, data.table = FALSE, ...)
  if (!is.null(n)) {
    sample_idxs <- sample(1:nrow(data), n, replace = FALSE)
    data <- data[sample_idxs, , drop = FALSE]
  }
  if (!is.null(p)) {
    feature_idxs <- sample(1:ncol(data), p, replace = FALSE)
    data <- data[, feature_idxs, drop = FALSE]
  }
  data <- scale(data, center = TRUE, scale = TRUE)
  rownames(data) <- 1:nrow(data)
  colnames(data) <- paste0("X", seq_len(ncol(data)))
  return(as.matrix(data))
}


#' Load and preprocess Philadelphia crime data
#'
#' @param data_dir Directory containing the Philadelphia crime data files.
#' @param keep_tract_var Logical indicating whether to keep the census tract
#'   variable.
#' @param include_weather Logical indicating whether to include weather
#'   covariates.
#' @param split_mode Character string indicating the mode of splitting the data.
#'   Options are "time", "random", or "location".
#' @param subsample Proportion of data to subsample (default is 1, meaning no
#'   subsampling).
#' @param train_prop Proportion of data to use for training (default is 0.8).
#' @param test_all Logical indicating whether to include all test data (default
#'   is TRUE).
#'
#' @returns A list containing the training and test data, the adjacency matrix
#'   `A`, the full adjacency matrix `A_full`, and additional verbose data.
load_philly_crime_data <- function(data_dir = here::here("data/philly_crime"),
                                   keep_tract_var = FALSE,
                                   include_weather = TRUE,
                                   split_mode = c("time", "random", "location"),
                                   subsample = 1,
                                   train_prop = 0.8,
                                   test_all = TRUE) {

  split_mode <- match.arg(split_mode)
  load(file.path(data_dir, "philly_crime_data.RData"))
  # weather data from https://scacis.rcc-acis.org/
  weather_data <- data.table::fread(
    file.path(data_dir, "philly_weather.csv"), na.strings = "M"
  ) |>
    as.data.frame() |>
    dplyr::mutate(
      Date = 1:dplyr::n(),
      TotalSnowfall = as.numeric(ifelse(TotalSnowfall == "T", 0, TotalSnowfall))
    ) |>
    dplyr::select(-HighestAvgTemperature, -LowestAvgTemperature) |>
    setNames(
      c(
        "time", "MeanAvgTemperature",
        "TotalPrecipitation", "NumberofDaysPrecipitation",
        "TotalSnowfall", "NumberofDaysSnowfall"
      )
    ) |>
    dplyr::select(-NumberofDaysSnowfall)
  if (keep_tract_var) {
    crime_data <- data.frame(
      y = c(Y_all),
      time = c(X_cont_all),
      tract = c(X_cat_all)
    )
  } else {
    crime_data <- data.frame(
      y = c(Y_all),
      time = c(X_cont_all)
    )
  }
  crime_data <- dplyr::left_join(crime_data, weather_data, by = "time")
  na_idx <- apply(crime_data, 1, function(x) any(is.na(x)))
  X <- crime_data[!na_idx, , drop = FALSE] |>
    dplyr::select(-y)
  Y_all <- c(scale(crime_data$y[!na_idx], center = TRUE, scale = FALSE))
  vertex_id_all <- vertex_id_all[!na_idx]

  if (!include_weather) {
    X <- X |>
      dplyr::select(
        -tidyselect::any_of(setdiff(colnames(weather_data), "time"))
      )
  }

  X <- as.data.frame(scale(X, center = TRUE, scale = TRUE))

  if (subsample < 1) {
    if (split_mode == "time") {
      n_times <- length(unique(X$time))
      keep_times <- sort(sample(unique(X$time), round(subsample * n_times)))
      in_subsample_idxs <- which(X$time %in% keep_times)
      out_subsample_idxs <- which(!(X$time %in% keep_times))
    } else if (split_mode == "random") {
      in_subsample_idxs <- sort(sample(1:nrow(X), round(subsample * nrow(X))))
      out_subsample_idxs <- setdiff(1:nrow(X), in_subsample_idxs)
    } else if (split_mode == "location") {
      n_locs <- nrow(A_tract)
      keep_locs <- sort(sample(1:n_locs, round(subsample * n_locs)))
      in_subsample_idxs <- which(vertex_id_all %in% keep_locs)
      out_subsample_idxs <- which(!(vertex_id_all %in% keep_locs))
    }
    Y_in <- Y_all[in_subsample_idxs]
    Y_out <- Y_all[out_subsample_idxs]
    vertex_id_in <- vertex_id_all[in_subsample_idxs]
    vertex_id_out <- vertex_id_all[out_subsample_idxs]
    X_in <- X[in_subsample_idxs, , drop = FALSE]
    X_out <- X[out_subsample_idxs, , drop = FALSE]
    rm_cols <- apply(X, 2, function(x) length(unique(x)) == 1)
    X_in <- X_in[, !rm_cols, drop = FALSE]
    X_out <- X_out[, !rm_cols, drop = FALSE]
  } else {
    X_in <- X
    X_out <- NULL
  }

  rownames(A_tract) <- 1:nrow(A_tract)
  colnames(A_tract) <- 1:nrow(A_tract)

  # split train/test by census tract (i.e., location)
  train_idxs <- sample(1:nrow(A_tract), round(train_prop * nrow(A_tract)))
  train_vertex_idxs <- vertex_id_in %in% train_idxs
  test_vertex_idxs <- !(vertex_id_in %in% train_idxs)
  X_train <- X_in[train_vertex_idxs, , drop = FALSE]
  y_train <- Y_in[train_vertex_idxs]
  train_vertex <- unique(vertex_id_in[train_vertex_idxs])
  if (test_all) {
    X_test <- rbind(X_in[test_vertex_idxs, , drop = FALSE], X_out)
    y_test <- c(Y_in[test_vertex_idxs], Y_out)
    test_vertex <- setdiff(
      unique(c(vertex_id_in[test_vertex_idxs], vertex_id_out)),
      train_vertex
    )
  } else {
    X_test <- X_in[test_vertex_idxs, , drop = FALSE]
    y_test <- Y_in[test_vertex_idxs]
    test_vertex <- unique(vertex_id_in[test_vertex_idxs])
  }

  A_train <- A_tract[train_vertex, train_vertex]
  A_full <- A_tract[c(train_vertex, test_vertex), c(train_vertex, test_vertex)]

  vertex_merged_df <- tibble::tibble(
    A_rowid = 1:nrow(A_full),
    vertex_id = as.numeric(rownames(A_full))
  )
  if (test_all) {
    vertex_ids <- tibble::tibble(
      vertex_id = c(
        vertex_id_in[train_vertex_idxs],
        vertex_id_in[test_vertex_idxs],
        vertex_id_out
      )
    ) |>
      dplyr::left_join(vertex_merged_df, by = "vertex_id") |>
      dplyr::pull(A_rowid)
  } else {
    vertex_ids <- tibble::tibble(
      vertex_id = c(
        vertex_id_in[train_vertex_idxs],
        vertex_id_in[test_vertex_idxs]
      )
    ) |>
      dplyr::left_join(vertex_merged_df, by = "vertex_id") |>
      dplyr::pull(A_rowid)
  }

  dimnames(A_train) <- NULL
  dimnames(A_full) <- NULL

  g <- igraph::graph_from_adjacency_matrix(A_full, mode = "undirected")
  igraph::E(g)$eid <- as.integer(1:igraph::ecount(g))  # edge id
  igraph::E(g)$weight <- 1
  igraph::V(g)$vid <- as.integer(1:igraph::vcount(g))  # vertex id

  out <- list(
    x = X_train,
    y = y_train,
    x_test = X_test,
    y_test = y_test,
    A = A_train,
    A_full = A_full,
    verbose_data_out = list(
      nodeids = vertex_ids[1:nrow(X_train)],
      nodeids_test = vertex_ids[(nrow(X_train) + 1):(length(vertex_ids))],
      graph = g,
      in_subsample_idxs = in_subsample_idxs,
      out_subsample_idxs = out_subsample_idxs,
      train_idxs = train_idxs
    )
  )
  return(out)
}


#' Load and preprocess school conflict data
#'
#' @param data_dir Directory containing the school conflict data files.
#' @param response_type Character string indicating the type of response
#'   variable to load. Options are "PNW2", "PNW2_all", "PNdiff", and
#'   "PNdiff_all".
#' @param network_type Character string indicating the type of network to load.
#'   Options are "A", "A1", "A2", "B", "B1", "DA", and "DA1".
#' @param keep_schools Character string or vector indicating which schools to
#'   keep. Options are "treated" (default) to keep only treated schools,
#'   or a vector of school IDs to keep specific schools.
#' @param include_covariates Character vector of covariates to include in the
#'   data. If NULL, a default set of covariates is used.
#' @param include_w1 Logical indicating whether to include the W1 variable
#'   in the covariates data.
#' @param impute_mode Character string indicating the mode of imputation for
#'   response variables. Options are "none", "mean", "median", and "rf".
#' @param max_n Maximum number of samples to keep in the data. If NULL, all
#'   samples are kept.
#' @param train_prop Proportion of data to use for training (default is 0.8).
#' @param connected Logical indicating if the training data should be restricted
#'   to the largest connected component of the graph (default is TRUE).
#'
#' @returns A list containing the training and test data, the adjacency matrix
#'   `A`, and the full adjacency matrix `A_full`.
load_school_conflict_data <- function(
    data_dir = here::here("data/SchoolConflictData/"),
    response_type = c("PNW2", "PNW2_all", "PNdiff", "PNdiff_all"),
    network_type = c("A", "A1", "A2", "B", "B1", "DA", "DA1"),
    keep_schools = "treated",
    include_covariates = NULL,
    include_w1 = FALSE,
    impute_mode = c("none", "mean", "median", "rf"),
    max_n = NULL,
    train_prop = 0.8,
    connected = TRUE
) {
  response_type <- match.arg(response_type)
  network_type <- match.arg(network_type)
  impute_mode <- match.arg(impute_mode)
  load(file.path(data_dir, "ProcessedData.Rda"))

  # remove empty schools
  nonempty_schools <- sapply(
    lcc.list, FUN = function(x) isTRUE(nrow(x$A) == nrow(x$df))
  )
  networks_ls <- purrr::map(
    lcc.list[nonempty_schools],
    function(.x) {
      if (network_type %in% c("B", "B1")) {
        (.x[[network_type]] + t(.x[[network_type]])) / 2
      } else {
        .x[[network_type]]
      }
    }
  )
  Xs_ls <- purrr::map(lcc.list[nonempty_schools], "df")

  # further selection of schools (e.g., by id or treatment)
  if (!is.null(keep_schools)) {
    if (identical(keep_schools, "treated")) {
      trt_school_id <- "(1) Treatment school (Roots meetings 2012-2013)"
      keep_schools_idx <- sapply(
        Xs_ls,
        FUN = function(x) {
          isTRUE(x$SCHTREAT[1] %in% trt_school_id) && (nrow(x) > 1)
        }
      )
    } else {
      keep_schools_idx <- sapply(
        Xs_ls,
        FUN = function(x) {
          isTRUE(x$SCHID[1] %in% keep_schools) && (nrow(x) > 1)
        }
      )
    }
  } else {
    keep_schools_idx <- rep(TRUE, length(networks_ls))
  }
  network_sp <- Matrix::bdiag(networks_ls[keep_schools_idx])
  X_all <- dplyr::bind_rows(Xs_ls[keep_schools_idx])

  # get cleaned X
  if (is.null(include_covariates)) {
    include_covariates <- c(
      "SCHID", "GENC", "GRC", "TREAT", "RETURN", "LIVEWB",
      "ETHW", "ETHB", "ETHH", "ETHA"
    )
  }
  X <- X_all |>
    tibble::as_tibble() |>
    dplyr::mutate(
      SCHID = as.factor(SCHID)
    ) |>
    dplyr::select(tidyselect::all_of(include_covariates), UID) |>
    droplevels()

  # helper functions to clean response variables
  clean_pn_response <- function(wave_id, rm_cols = NULL, summarize = TRUE) {
    y_all <- X_all |>
      tibble::as_tibble() |>
      dplyr::select(tidyselect::matches(sprintf("^PN[0-9]*%s$", wave_id))) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::everything(),
          ~ as.numeric(.x) - 1
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(paste0(c("PN3", "PN4", "PN5", "PN6", "PN9"), wave_id)),
          ~ 5 - .x
        )
      )
    if (!is.null(rm_cols)) {
      y_all <- y_all |>
        dplyr::select(-tidyselect::all_of(rm_cols))
    }
    if (identical(impute_mode, "mean")) {
      y_all_imp <- y_all |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::everything(),
            ~ tidyr::replace_na(.x, replace = mean(.x, na.rm = TRUE))
          )
        )
    } else if (identical(impute_mode, "median")) {
      y_all_imp <- y_all |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::everything(),
            ~ tidyr::replace_na(.x, replace = median(.x, na.rm = TRUE))
          )
        )
    } else if (identical(impute_mode, "rf")) {
      missforest_fit <- missForest::missForest(as.matrix(y_all))
      y_all_imp <- tibble::as_tibble(missforest_fit$ximp)
    } else {
      y_all_imp <- y_all
    }
    if (summarize) {
      y_all <- y_all |>
        dplyr::mutate(
          y = rowMeans(y_all_imp, na.rm = TRUE),
          sd = apply(y_all_imp, 1, sd, na.rm = TRUE)
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          nacount = sum(is.na(dplyr::c_across(tidyselect::starts_with("PN"))))
        )
    }
    return(y_all)
  }

  # get cleaned y
  if (stringr::str_starts(response_type, "PNW[1-2]")) {
    wave_id <- ifelse(stringr::str_starts(response_type, "PNW1"), "", "W2")
    if (keep_schools == "20") {
      # remove PN4W2 for school 20 since this is almost always missing
      y_all <- clean_pn_response(wave_id, "PN4W2")
    } else {
      y_all <- clean_pn_response(wave_id)
    }
    if (identical(impute_mode, "none")) {
      yna_idx <- (y_all$nacount != 0) | (y_all$sd == 0)
    } else {
      yna_idx <- (y_all$nacount > 4) | (y_all$sd == 0)
    }
    if (identical(response_type, "PNW2") || identical(response_type, "PNW1")) {
      y <- y_all$y
    } else {
      y <- y_all |>
        dplyr::select(-y, -sd, -nacount)
    }
  } else if (stringr::str_starts(response_type, "PNdiff")) {
    y_w1_all <- clean_pn_response("")
    y_w1 <- y_w1_all$y
    y_w1_all <- y_w1_all |>
      dplyr::select(-y, -sd, -nacount)
    y_w2_all <- clean_pn_response("W2", rm_cols = "PN13W2")
    y_w2 <- y_w2_all$y
    y_w2_all <- y_w2_all |>
      dplyr::select(-y, -sd, -nacount)
    y_all <- (y_w2_all - y_w1_all) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        y = mean(dplyr::c_across(tidyselect::starts_with("PN")), na.rm = TRUE),
        nacount = sum(is.na(dplyr::c_across(tidyselect::starts_with("PN"))))
      ) |>
      dplyr::rename_with(~ stringr::str_remove(.x, "W2$"))
    if (identical(impute_mode, "none")) {
      yna_idx <- (y_all$nacount != 0) |
        (apply(y_w1_all, 1, sd) == 0) |
        (apply(y_w2_all, 1, sd) == 0)
    } else {
      yna_idx <- (y_all$nacount > 4) |
        (apply(y_w1_all, 1, sd, na.rm = TRUE) == 0) |
        (apply(y_w2_all, 1, sd, na.rm = TRUE) == 0)
    }
    if (identical(response_type, "PNdiff")) {
      if (identical(impute_mode, "none")) {
        y <- y_all$y
      } else {
        y <- y_w2 - y_w1
      }
    } else {
      y <- y_all |>
        dplyr::bind_cols(
          y_w1_all |> dplyr::rename_with(~ paste0(.x, "W1")),
          y_w2_all
        ) |>
        dplyr::select(-y, -nacount)
    }
  }

  if (include_w1) {
    w1_response <- clean_pn_response("")$y
    X <- X |>
      dplyr::mutate(PNW1 = w1_response)
  }

  # remove NAs and other samples
  xna_idx <- apply(X, 1, FUN = function(x) any(is.na(x)))
  X <- X[!xna_idx & !yna_idx, ]
  if (isTRUE(length(dim(y)) == 2)) {
    y <- y[!xna_idx & !yna_idx, ]
  } else {
    y <- y[!xna_idx & !yna_idx]
  }
  network_sp <- network_sp[!xna_idx & !yna_idx, !xna_idx & !yna_idx]

  # remove schools with not enough students
  small_schools <- names(which(table(X$SCHID) <= 1))
  small_schools_idx <- X$SCHID %in% small_schools
  X <- X[!small_schools_idx, ] |> droplevels()
  if (isTRUE(length(dim(y)) == 2)) {
    y <- y[!small_schools_idx, ]
  } else {
    y <- y[!small_schools_idx]
  }
  network_sp <- network_sp[!small_schools_idx, !small_schools_idx]

  if (!is.null(max_n)) {
    X <- X[1:max_n, , drop = FALSE]
    if (isTRUE(length(dim(y)) == 2)) {
      y <- y[1:max_n, ]
    } else {
      y <- y[1:max_n]
    }
    network_sp <- network_sp[1:max_n, 1:max_n]
  }

  # clean rownames/UIDs
  X <- X |>
    tibble::column_to_rownames("UID")
  A <- as.matrix(network_sp)
  rownames(A) <- rownames(X)
  colnames(A) <- rownames(X)
  factor_cols <- names(X)[sapply(X, class) == "factor"]
  for (factor_col in factor_cols) {
    if (nlevels(X[[factor_col]]) == 1) {
      X <- X |> dplyr::select(-tidyselect::all_of(factor_col))
    }
  }

  if (connected &&
      !is.null(keep_schools) && !identical(keep_schools, "treated")) {
    connected_out <- get_connected_graph(A, X, y)
    A <- connected_out$A
    X <- connected_out$X
    y <- connected_out$y
  }

  # center y
  y <- scale(y, center = TRUE, scale = FALSE)

  if (train_prop == 1) {
    out <- list(
      x = X, y = y, A = A,
      x_test = NULL, y_test = NULL, A_full = A
    )
  } else {
    out <- load_data(
      X = X, y = y, A = A, train_prop = train_prop, connected = connected
    )
  }

  # remove constant columns or factors with very low variance
  const_cols <- apply(out$x, 2, function(x) length(unique(x)) == 1)
  factor_cols <- sapply(out$x, class) == "factor"
  lowvar_cols <- purrr::map2_lgl(
    out$x, factor_cols,
    function(x, is_factor_col) {
      is_factor_col && (nlevels(x) == 2) && (min(table(x)) < 4)
    }
  )
  if (any(const_cols) || any(lowvar_cols)) {
    out$x <- out$x[, !const_cols & !lowvar_cols, drop = FALSE]
    out$x_test <- out$x_test[, !const_cols & !lowvar_cols, drop = FALSE]
  }
  # remove factor levels with zero number of samples and adjust factors
  out$x <- out$x |>
    droplevels()
  for (col in names(factor_cols)[factor_cols]) {
    x_levels <- levels(out$x[[col]])
    x_test_levels <- levels(out$x_test[[col]])
    if (!identical(x_levels, x_test_levels)) {
      refactored_col <- factor(
        as.character(out$x_test[[col]]), levels = x_levels
      )
      refactored_col[is.na(refactored_col)] <- names(
        which.max(table(out$x[[col]]))
      )
      out$x_test[[col]] <- refactored_col
    }
  }

  return(out)
}
