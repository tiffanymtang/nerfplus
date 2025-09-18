#' @keywords internal
fiUI <- function(id, name,
                 choices = c("ggplot", "plotly", "table"),
                 footer = NULL) {
  prettyBox(
    selectVizUI(id, choices = choices),
    plotUI(id),
    dropdownMenu = options_dropdown(
      plotOptionsUI(
        id, multicol = TRUE, total_width = 9
      ),
      tableOptionsUI(
        id, digits = 3, sigfig = TRUE, digits_label = "Table Digits",
        total_width = 3
      ),
      width = "900px"
    ),
    title = sprintf("%s Feature Importance Plot", name),
    footer = footer
  )
}


#' @keywords internal
plotGlobalFeatureImportance <- function(fiTable,
                                        fi_display_mode,
                                        show_features,
                                        max_features) {
  # to avoid no visible binding note
  Importance <- NULL
  Variable <- NULL
  FeatureType <- NULL

  shiny::req(
    fiTable(), fi_display_mode()
  )

  plt_df <- fiTable()
  if (fi_display_mode() == "top") {
    shiny::req(max_features())
    plt_df <- plt_df |>
      dplyr::arrange(dplyr::desc(Importance)) |>
      dplyr::slice(1:min(nrow(plt_df), max_features()))
  } else if (fi_display_mode() == "manual") {
    shiny::req(show_features())
    plt_df <- plt_df |>
      dplyr::filter(Variable %in% show_features())
  }
  var_levels <- plt_df |>
    dplyr::arrange(Importance) |>
    dplyr::pull(Variable)
  plt_df <- plt_df |>
    dplyr::mutate(
      Variable = factor(Variable, levels = var_levels),
      FeatureType = ifelse(
        Variable %in% c("Network (alpha)", "Network (alpha, Z)", "Network (Z)"),
        "Network", "Non-Network"
      )
    )

  plt <- ggplot2::ggplot(plt_df) +
    ggplot2::aes(x = Variable, y = Importance, fill = FeatureType) +
    ggplot2::geom_bar(
      stat = "identity"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Network" = "#84BECF", "Non-Network" = "gray")
    ) +
    ggplot2::scale_x_discrete(
      labels = c(
        "Network (alpha, Z)" = expression(paste("Network (", alpha, ", Z)")),
        "Network (alpha)" = expression(paste("Network (", alpha, ")"))
      )
    ) +
    ggplot2::labs(
      fill = "Feature Type"
    )
  return(plt)
}


#' @keywords internal
plotLocalFeatureImportance <- function(fiTable,
                                       dataList,
                                       lfi_local_vars,
                                       local_vars,
                                       shared_guide,
                                       show_splits,
                                       edge_alpha = 0.125,
                                       point_size = 3.5,
                                       title_size = 16) {
  shiny::req(fiTable(), dataList(), show_splits())

  # to avoid no visible binding note
  x <- NULL
  y <- NULL
  .data <- NULL

  X_train <- dataList()$x
  y_train <- dataList()$y
  nodeids_train <- dataList()$nodeids
  X_test <- dataList()$xtest
  y_test <- dataList()$ytest
  nodeids_test <- dataList()$nodeids_test
  if (identical(show_splits(), "train")) {
    keep_nodeids <- nodeids_train
  } else if (identical(show_splits(), "test")) {
    keep_nodeids <- nodeids_test
  } else {
    keep_nodeids <- c(nodeids_train, nodeids_test)
  }
  X <- rbind(X_train, X_test)[keep_nodeids, , drop = FALSE]
  response <- c(y_train, y_test)[keep_nodeids]
  A <- dataList()$A_full[keep_nodeids, keep_nodeids]
  g <- igraph::graph_from_adjacency_matrix(
    A > 0, mode = "undirected"
  )

  if (length(lfi_local_vars()) > 0) {
    lfi_df <- fiTable()[keep_nodeids, , drop = FALSE] |>
      dplyr::select(tidyselect::any_of(lfi_local_vars())) |>
      dplyr::rename_with(~ paste0(".lfi_", .x))
  } else {
    lfi_df <- NULL
  }

  if (length(local_vars()) > 0) {
    x_df <- as.data.frame(X) |>
      dplyr::select(tidyselect::any_of(local_vars())) |>
      dplyr::rename_with(~ paste0(".x_", .x))
  } else {
    x_df <- NULL
  }

  set.seed(1234)
  plt <- ggraph::ggraph(g, "igraph", algorithm = "nicely")
  plt_df <- plt$data |>
    dplyr::mutate(
      .response = response
    ) |>
    dplyr::bind_cols(lfi_df, x_df)

  if (shared_guide()) {
    data_vec <- plt_df |>
      dplyr::select(
        tidyselect::starts_with(".response"),
        tidyselect::starts_with(".lfi_"),
        tidyselect::starts_with(".x_")
      ) |>
      unlist()
    color_limits <- c(min(data_vec), max(data_vec))
  } else {
    color_limits <- NULL
  }

  plt_vars <- c(".response", colnames(lfi_df), colnames(x_df))
  plt_ls <- list()
  for (plt_var in plt_vars) {
    if (plt_var == ".response") {
      plt_title <- "Observed Response"
    } else if (stringr::str_starts(plt_var, ".lfi_")) {
      plt_title <- sprintf(
        "Local %s Importance", stringr::str_remove(plt_var, ".lfi_")
      )
    } else if (stringr::str_starts(plt_var, ".x_")) {
      plt_title <- sprintf(
        "Observed %s", stringr::str_remove(plt_var, ".x_")
      )
    }
    plt_ls[[plt_var]] <- plt +
      ggraph::geom_edge_link(
        edge_alpha = edge_alpha
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(x = x, y = y, color = .data[[plt_var]]),
        data = plt_df, size = point_size
      ) +
      ggplot2::labs(color = "", title = plt_title) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, size = title_size, face = "bold"
        )
      )
    if (is.numeric(plt_df[[plt_var]])) {
      plt_ls[[plt_var]] <- plt_ls[[plt_var]] +
        ggplot2::scale_color_viridis_c(
          option = "C", begin = 0, end = 0.95, limits = color_limits
        )
    }
  }
  if (shared_guide()) {
    guides <- "collect"
  } else {
    guides <- "auto"
  }
  plt <- patchwork::wrap_plots(plt_ls, guides = guides)
  return(plt)
}

#' @keywords internal
plotSampleInfluences <- function(looResults,
                                 dataList,
                                 edge_alpha = 0.125,
                                 point_size = 3.5,
                                 title_size = 16) {
  shiny::req(looResults(), dataList())

  # to avoid no visible binding note
  x <- NULL
  y <- NULL
  .data <- NULL

  nodeids <- dataList()$nodeids
  A <- dataList()$A_full[nodeids, nodeids]
  g <- igraph::graph_from_adjacency_matrix(
    A > 0, mode = "undirected"
  )

  set.seed(1234)
  plt <- ggraph::ggraph(
    g, "igraph", algorithm = "nicely"
  )
  plt_df <- plt$data |>
    dplyr::mutate(
      # all mean abs differences
      `Change in Alpha Estimates` = looResults()$mean_change_alphas,
      `Change in Beta Estimates` = looResults()$mean_change_betas,
      `Change in Test Error` = looResults()$mean_change_error_test,
      `Change in Test Predictions` = looResults()$mean_change_preds_test
    )

  plt_vars <- c(
    "Change in Alpha Estimates", "Change in Beta Estimates",
    "Change in Test Error", "Change in Test Predictions"
  )
  plt_ls <- list()
  for (plt_var in plt_vars) {
    plt_ls[[plt_var]] <- plt +
      ggraph::geom_edge_link(
        edge_alpha = edge_alpha
      ) +
      ggraph::geom_node_point(
        ggplot2::aes(x = x, y = y, color = .data[[plt_var]]),
        data = plt_df, size = point_size
      ) +
      ggplot2::scale_color_viridis_c(
        option = "A", begin = 0, end = 0.95
      ) +
      ggplot2::labs(color = "", title = plt_var) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, size = title_size, face = "bold"
        )
      )
  }
  plt <- patchwork::wrap_plots(plt_ls)
  return(plt)
}

