#' Preprocessing function
#'
#' @description This function will perform some preprocessing steps including
#'   augmenting the embedding features, standardizing, and dummy coding
#'   categorical variables.
#'
#' @param x Training data.
#' @param x_test Test data.
#' @param A Adjacency matrix for the training data.
#' @param A_full Full adjacency matrix for the training and test data.
#' @param verbose_data_out Additional data to be included in the output.
#' @param standardize Logical indicating whether to standardize the data.
#' @param dummy_code Logical indicating whether to dummy code categorical
#'   variables.
#' @param embedding Embedding type(s), at least one of "adjacency", "laplacian",
#'   score", or NULL (i.e., do not include any network embedding features).
#'   Alternatively, can directly input an n x d matrix of network embedding
#'   features corresponding to `x`.
#' @param embedding_options A list of options for the network embedding.
#'   Ignored if `embedding = NULL`. If provided, the list should contain the
#'   following components:
#'   - `ndim`: Number of dimensions in the embedding.
#'   - `regularization`: Regularization parameter for the adjacency matrix.
#'   - `varimax`: Whether to apply varimax rotation to the embedding.
#'   - `center`: Whether to center the embedding so that each column has mean 0.
#'   - `scale`: Whether to scale the embedding so that first embedding component column has SD 1.
#'     All other embedding components are scaled, proportional to their eigenvalues.
#'
#' @returns A list containing the preprocessed `x`, `x_test`, and the original
#'   column names of `x` (with the embedding columns).
preprocess_fun <- function(x, x_test, A, A_full, verbose_data_out,
                           standardize, dummy_code = TRUE,
                           embedding, embedding_options) {
  # get embeddings if manually inputted
  if (!is.null(embedding) && !is.character(embedding)) {
    oos_embedding <- embedding[(nrow(x) + 1):(nrow(embedding)), , drop = FALSE]
    embedding <- embedding[1:nrow(x), , drop = FALSE]
  } else {
    oos_embedding <- NULL
  }

  # augment and standardize raw features and embeddings
  preprocess_out <- nerfplus:::fit_pre_rf_preprocessing(
    x = x, A = A, standardize = standardize,
    embedding = embedding, embedding_options = embedding_options,
    nodeids = verbose_data_out$nodeids
  )
  x <- preprocess_out$x
  x_test <- nerfplus:::apply_pre_rf_preprocessing(
    preprocess_out, x = x_test, x_embed = oos_embedding, A_full = A_full,
    nodeids = verbose_data_out$nodeids_test
  )

  # dummy code categorical variables
  x_orig_colnames <- colnames(x)
  if (dummy_code) {
    dummy_out <- nerfplus:::fit_dummy_code(x)
    x <- as.data.frame(dummy_out$x)
    x_test <- nerfplus:::apply_dummy_code(dummy_out$dummy_fit, x = x_test) |>
      as.data.frame()
  }

  return(list(
    x = x, x_test = x_test, x_orig_colnames = x_orig_colnames
  ))
}


#' Get feature groups
#'
#' @param orig_colnames Original column names of the data.
#' @param x_colnames Column names of the data after preprocessing (e.g., after
#'   one-hot encoding).
#'
#' @returns A named list where each element corresponds to a feature group.
get_grouped_features <- function(orig_colnames, x_colnames) {
  if (any(stringr::str_starts(orig_colnames, ".embed"))) {
    out_colnames <- c(
      orig_colnames[!stringr::str_starts(orig_colnames, ".embed")],
      ".embed"
    )
  } else {
    out_colnames <- orig_colnames
  }
  grouped_features <- purrr::map(
    out_colnames,
    function(j) {
      if (j == ".embed") {
        xj_features <- x_colnames[stringr::str_starts(x_colnames, ".embed")]
      } else {
        xj_features <- x_colnames[
          stringr::str_detect(x_colnames, paste0("^", j, "$")) |
            stringr::str_detect(x_colnames, paste0("^", j, "\\.\\..*"))
        ]
      }
      xj_features
    }
  ) |>
    setNames(out_colnames)
  return(grouped_features)
}


#' Helper function to return consistent method output
#'
#' @param out The output list to be returned.
#' @param x The training data.
#' @param y The training labels.
#' @param x_test The test data.
#' @param y_test The test labels.
#' @param A Adjacency matrix for the training data.
#' @param A_full Full adjacency matrix for the training and test data.
#' @param fit The fitted model object.
#' @param verbose_data_out Additional data to be included in the output.
#' @param predictions Predictions made by the model.
#' @param classification Logical indicating whether the task is a classification
#'   task.
#' @param importance Global feature importance results.
#' @param local_importance Local feature importance results.
#' @param time_elapsed Time taken for the method to run.
#' @param return_features Features to be returned in the output.
#' @param return_fit Logical indicating whether to include the fitted model in
#'   the output.
#' @param return_data Logical indicating whether to return the data in the
#'   output. Alternatively, can specify vector of object names to return.
#'
#' @returns A list containing the specified outputs.
return_method_output <- function(out, x, y, x_test, y_test, A, A_full, fit,
                                 verbose_data_out,
                                 predictions, classification,
                                 importance, local_importance,
                                 time_elapsed,
                                 return_features, return_fit, return_data) {
  if (is.null(out)) {
    out <- list()
  }
  out[["y_test"]] <- y_test
  out[["predictions"]] <- predictions
  if (classification) {
    out[["class_predictions"]] <- as.integer(predictions >= 0.5)
  }
  out[["importance"]] <- importance
  out[["local_importance"]] <- local_importance
  out[["time_elapsed"]] <- time_elapsed
  if (!is.null(return_features)) {
    for (feature in return_features) {
      out[[feature]] <- x_test_orig[[feature]]
    }
  }
  if (return_fit) {
    out$fit <- fit
  }
  if (!isFALSE(return_data)) {
    if (isTRUE(return_data)) {
      out[["x"]] <- x
      out[["y"]] <- y
      out[["x_test"]] <- x_test
      out[["y_test"]] <- y_test
      out[["A"]] <- A
      out[["A_full"]] <- A_full
      for (feature in names(verbose_data_out)) {
        out[[feature]] <- verbose_data_out[[feature]]
      }
    } else {
      for (feature in return_data) {
        out[[feature]] <- get(feature)
      }
    }
  }
  return(out)
}
