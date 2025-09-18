plot_pred_err_wrapper <- function(fit_results, eval_results,
                                  vary_params = NULL,
                                  keep_methods = NULL,
                                  eval_name,
                                  ...) {
  if (!is.null(keep_methods)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::filter(
        .method_name %in% !!keep_methods
      ) |>
      dplyr::mutate(
        .method_name = factor(.method_name, levels = keep_methods)
      )
  }
  out <- plot_pred_err(
    fit_results = fit_results, eval_results = eval_results,
    vary_params = vary_params, eval_name = eval_name, ...
  )
  return(out)
}


plot_feature_importance_wrapper <- function(fit_results, eval_results,
                                            vary_params = NULL,
                                            keep_methods = NULL,
                                            eval_name,
                                            feature_col,
                                            feature_order = NULL,
                                            ...) {
  if (!is.null(keep_methods)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::filter(
        .method_name %in% !!keep_methods
      ) |>
      dplyr::mutate(
        .method_name = factor(.method_name, levels = keep_methods)
      )
  }
  eval_results[[eval_name]] <- eval_results[[eval_name]] |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(vary_params),
        ~ forcats::fct_inorder(as.character(.x))
      ),
      .network_group = stringr::str_starts(.data[[feature_col]], "\\."),
    )
  if (!is.null(feature_order)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(feature_col),
          ~ factor(.x, levels = feature_order)
        )
      )
  }
  feature_vals <- eval_results[[eval_name]][[feature_col]]
  if (all(stringr::str_detect(feature_vals, "^(V\\d+$|\\.)"))) {
    max_var <- max(as.numeric(stringr::str_extract(feature_vals, "\\d+")), na.rm = TRUE)
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(feature_col),
          ~ factor(
            stringr::str_replace(.x, "^V", "X"), 
            levels = c(
              ".network", ".alpha", ".embed",
              paste0("X", 1:max_var)
            )
          ) |>
            droplevels()
        )
      )
  }

  out <- plot_feature_importance(
    fit_results = fit_results, eval_results = eval_results,
    vary_params = vary_params, eval_name = eval_name, feature_col = feature_col,
    ...
  )
  out <- patchwork::wrap_plots(out, ncol = 1) +
    patchwork::plot_layout(axis_titles = "collect_x")
    # patchwork::plot_layout(guides = "collect", axis_titles = "collect_x")
  return(out)
}


plot_lfi <- function(fit_results, eval_results = NULL, 
                     vary_params = NULL, 
                     rep = NULL,
                     keep_method = "NeRF+",
                     mode = c(".network", ".alpha", ".embed"),
                     point_size = 6,
                     fill_var = NULL,
                     fill_label = fill_var,
                     fill_values = NULL,
                     seed = 1234) {
  mode <- match.arg(mode)
  mode_lab <- dplyr::case_when(
    mode == ".network" ~ "Network",
    mode == ".alpha" ~ "Network Cohesion",
    mode == ".embed" ~ "Network Embedding"
  )

  # filter lfi results to desired replicates and methods
  lfi_fit_results <- fit_results
  if (!is.null(rep)) {
    lfi_fit_results <- fit_results |>
      dplyr::filter(
        .rep == !!rep
      )
  }
  lfi_fit_results <- lfi_fit_results |>
    dplyr::filter(
      .method_name == !!keep_method
    )
  # filter out null/empty lfi results
  lfi_fit_results <- lfi_fit_results[
    !sapply(lfi_fit_results[["local_importance"]], is.null),
  ]

  # get data from study
  if (nrow(lfi_fit_results) == 1) {
    x_full <- rbind(lfi_fit_results$x[[1]], lfi_fit_results$x_test[[1]])
    y_full <- c(lfi_fit_results$y[[1]], lfi_fit_results$y_test[[1]])
    A_full <- lfi_fit_results$A_full[[1]]
    nodeids_full <- c(lfi_fit_results$nodeids[[1]], lfi_fit_results$nodeids_test[[1]])
  } else {
    data_list_ls <- lfi_fit_results |>
      dplyr::rowwise() |>
      dplyr::mutate(
        y_full = list(c(y, y_test)),
        y_full_dim = length(y_full),
        A_full_dim = nrow(A_full)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        .rowid = 1:dplyr::n()
      ) |>
      dplyr::filter(
        y_full_dim == max(y_full_dim),
        A_full_dim == max(A_full_dim)
      )
    x_full <- rbind(data_list_ls$x[[1]], data_list_ls$x_test[[1]])
    y_full <- data_list_ls$y_full[[1]]
    A_full <- data_list_ls$A_full[[1]]
    nodeids_full <- c(data_list_ls$nodeids[[1]], data_list_ls$nodeids_test[[1]])
  }
  sample_ids <- rownames(A_full)
  if (is.null(sample_ids)) {
    sample_ids <- 1:nrow(A_full)
  }

  if (!is.null(nodeids_full)) {
    # nodeids provided, must aggregate results per node
    x_full <- x_full |>
      dplyr::mutate(
        .nodeid = factor(nodeids_full, levels = sample_ids)
      ) |>
      dplyr::group_by(.nodeid) |>
      dplyr::summarise(
        dplyr::across(tidyselect::everything(), ~ mean(.x))
      ) |>
      dplyr::arrange(.nodeid) |>
      dplyr::select(-.nodeid)
    y_full <- tibble::tibble(
      .nodeid = factor(nodeids_full, levels = sample_ids),
      y_full = y_full
    ) |>
      dplyr::group_by(.nodeid) |>
      dplyr::summarise(
        dplyr::across(tidyselect::everything(), ~ mean(.x))
      ) |>
      dplyr::arrange(.nodeid) |>
      dplyr::pull(y_full)
    # extract lfi results, with associated nodeids
    lfi_results_ls <- lfi_fit_results |>
      dplyr::mutate(
        local_importance = purrr::map2(
          .x = local_importance,
          .y = nodeids_test,
          function(.x, .y) {
            if (any(!(.y %in% sample_ids))) {
              warning("Missing sample in graph adjacency matrix.")
            }
            data.frame(.id = sample_ids) |> 
              dplyr::left_join(
                dplyr::bind_cols(.id = .y, .x) |>
                  dplyr::group_by(.id) |>
                  dplyr::summarise(
                    dplyr::across(tidyselect::everything(), ~ mean(.x))
                  ),
                by = ".id"
              )
          }
        )
      ) |> 
        dplyr::pull(local_importance)
  } else {
    # extract lfi results, with associated sampleids
    get_test_ids <- function(A, A_full) {
      n <- nrow(A)
      return(rownames(A_full)[(n + 1):nrow(A_full)])
    }
    lfi_results_ls <- lfi_fit_results |>
      dplyr::mutate(
        test_ids = purrr::map2(A, A_full, ~ get_test_ids(.x, .y)),
        local_importance = purrr::map2(
          .x = local_importance,
          .y = test_ids,
          function(.x, .y) {
            if (any(!(.y %in% sample_ids))) {
              warning("Missing sample in graph adjacency matrix.")
            }
            data.frame(.id = sample_ids) |> 
              dplyr::left_join(dplyr::bind_cols(.id = .y, .x), by = ".id")
          }
        )
      ) |> 
        dplyr::pull(local_importance)
  }
  # aggregate lfi results across replicates
  if (length(lfi_results_ls) == 1) {
    lfi_results <- lfi_results_ls[[1]][[mode]]
  } else {
    lfi_results <- purrr::map(lfi_results_ls, ~ .x[[mode]]) |> 
      purrr::reduce(cbind) |> 
      rowSums(na.rm = TRUE)
    lfi_results <- lfi_results / length(lfi_results_ls)
  }  
  
  # plot lfi results on network alongside observed responses (y)
  set.seed(seed)
  g <- igraph::graph_from_adjacency_matrix(A_full > 0, mode = "undirected")
  plt <- ggraph::ggraph(
    g, "igraph", algorithm = "nicely"
  ) +
    ggplot2::theme_void() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(size = 6))
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)
    )
  plt_df <- plt$data |> 
    dplyr::mutate(
      response = y_full,
      lfi = lfi_results
    )
  if (!is.null(fill_var)) {
    if (fill_var %in% colnames(x_full)) {
      plt_df <- plt_df |>
        dplyr::mutate(
          .fill = x_full[[fill_var]]
        )
    } else {
      plt_df <- plt_df |>
        dplyr::mutate(.fill = " ")
    }
    plt <- plt +
      ggplot2::geom_point(
        ggplot2::aes(x = x, y = y, fill = .fill),
        data = plt_df, size = 20, pch = 21, color = "#ffffff00"
      )
    if (!is.null(fill_values)) {
      plt <- plt + ggplot2::scale_fill_manual(values = fill_values)
    }
  }
  
  plt_lfi <- plt +
    ggraph::geom_edge_link(
      edge_alpha = 0.125
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(x = x, y = y, color = lfi),
      data = plt_df, size = point_size
    ) +
    ggplot2::scale_color_viridis_c(
      option = "C", begin = 0, end = 0.95
    ) +
    ggplot2::labs(
      title = sprintf("Local %s Importance", mode_lab),
      color = sprintf("Local %s\nImportance", mode_lab),
      fill = fill_label
    )
  plt_y <- plt +
    ggraph::geom_edge_link(
      edge_alpha = 0.125
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(x = x, y = y, color = response),
      data = plt_df, size = point_size
    ) +
    ggplot2::scale_color_viridis_c(
      option = "C", begin = 0, end = 0.95
    ) +
    ggplot2::labs(
      title = "Observed y",
      color = "Observed y", 
      fill = fill_label
    )
  plt <- patchwork::wrap_plots(plt_y, plt_lfi, nrow = 1)
  return(plt)
}


plot_influence_outliers <- function(fit_results, eval_results = NULL,
                                    vary_params, outlier_idxs = 1, ...) {
  loo_results <- fit_results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      loo_df = list(
        data.frame(
          .sample_id = 1:length(loo$mean_change_alphas),
          .is_outlier = 1:length(loo$mean_change_alphas) %in% outlier_idxs,
          change_alphas = loo$mean_change_alphas,
          change_alphas_rank = rank(-loo$mean_change_alphas),
          change_betas = loo$mean_change_betas,
          change_betas_rank = rank(-loo$mean_change_betas),
          change_error_test = loo$mean_change_error_test,
          change_error_test_rank = rank(-loo$mean_change_error_test),
          change_preds_test = loo$mean_change_preds_test,
          change_preds_test_rank = rank(-loo$mean_change_preds_test)
        )
      )
    ) |>
    tidyr::unnest(loo_df) |>
    dplyr::ungroup()

  loo_summary_df <- loo_results |>
    tidyr::pivot_longer(
      cols = c(
        change_alphas,
        change_alphas_rank,
        change_betas,
        change_betas_rank,
        change_error_test,
        change_error_test_rank,
        change_preds_test,
        change_preds_test_rank
      ),
      names_to = ".metric",
      values_to = "value"
    ) |>
    dplyr::group_by(
      dplyr::across(
        c(
          .dgp_name, .method_name, tidyselect::any_of(vary_params), 
          # .sample_id, 
          .is_outlier, .metric
        )
      )
    ) |>
    dplyr::summarise(
      prop1_value = mean(value == 1),
      mean_value = mean(value),
      se_value = sd(value) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # rank_plt <- loo_summary_df |>
  #   dplyr::filter(.is_outlier) |>
  #   ggplot2::ggplot() +
  #   ggplot2::aes(x = .data[[vary_params]], y = mean_value) +
  #   ggplot2::geom_point() +
  #   ggplot2::geom_line() +
  #   ggplot2::facet_wrap(~ .metric, scales = "free_y") +
  #   vthemes::theme_vmodern()
  # ggplot2::ggsave(rank_plt, filename = "test.pdf")
  prop1_plt <- loo_summary_df |>
    dplyr::filter(
      .is_outlier,
      stringr::str_detect(.metric, "rank")
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[vary_params]], y = prop1_value) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .metric, scales = "free_y", nrow = 1) +
    vthemes::theme_vmodern()
  # ggplot2::ggsave(prop1_plt, filename = "test.pdf")

  value_plt <- loo_summary_df |>
    dplyr::filter(
      !stringr::str_detect(.metric, "rank")
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[vary_params]], y = mean_value, color = .is_outlier) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .metric, scales = "free_y", nrow = 1) +
    vthemes::theme_vmodern()

  plt <- patchwork::wrap_plots(prop1_plt, value_plt, ncol = 1)
 
  return(plt)
}