rm(list = ls())
set.seed(331)

EXP_NAME <- "Figures and Tables"
N_REPS <- 100
source(here::here(file.path("meals", "setup.R")))

RESULTS_DIR <- here::here("results")
FIG_DIR <- here::here("results", "figures")
if (!dir.exists(FIG_DIR)) {
  dir.create(FIG_DIR, recursive = TRUE)
}
TAB_DIR <- here::here("results", "tables")
if (!dir.exists(TAB_DIR)) {
  dir.create(TAB_DIR, recursive = TRUE)
}

EXP_NAMES_MAIN <- c(
  "Linear Additive Blockwise Network DGP",
  "Polynomial Additive Blockwise Network DGP",
  "LSS Additive Blockwise Network DGP",
  "Linear Network Autocorrelation DGP",
  "Polynomial Network Autocorrelation DGP",
  "LSS Network Autocorrelation DGP"
)
EXP_NAMES_REAL <- c(
  "Linear Additive Blockwise Network with Real Data DGP",
  "Polynomial Additive Blockwise Network with Real Data DGP",
  "Linear Network Autocorrelation with Real Data DGP",
  "Polynomial Network Autocorrelation with Real Data DGP"
)
EXP_NAMES_OUTLIERS <- c(
  "Linear Additive Blockwise Network with Outliers DGP",
  "Polynomial Additive Blockwise Network with Outliers DGP",
  "LSS Additive Blockwise Network with Outliers DGP",
  "Linear Network Autocorrelation with Outliers DGP",
  "Polynomial Network Autocorrelation with Outliers DGP",
  "LSS Network Autocorrelation with Outliers DGP"
)

get_vary_param_name <- function(exp_name) {
  dplyr::case_when(
    stringr::str_detect(exp_name, "Blockwise") ~ "centroids_scale",
    stringr::str_detect(exp_name, "Autocorrelation") ~ "omega"
  )
}

get_exp_dir <- function(exp_name) {
  vary_param_name <- get_vary_param_name(exp_name)
  file.path(
    RESULTS_DIR, "Main Simulations", exp_name,
    sprintf("Varying %s-pve", vary_param_name)
  )
}

save_figure <- function(plt, filename, ...) {
  ggplot2::ggsave(
    filename = file.path(FIG_DIR, sprintf("%s.pdf", filename)),
    plot = plt,
    ...
  )
}

METHOD_LEVELS <- c(
  "NeRF+",
  "NeRF+ (Cohesion Only)",
  "NeRF+ (Embedding Only)",
  "RNC",
  "Network BART",
  "RF+",
  "Linear Regression",
  "BART"
)
KEEP_METHODS <- c(
  "NeRF+" = "NeRF+",
  "RNC" = "RNC",
  "Network BART" = "Network BART",
  "RF+" = "RF+",
  "Linear Regression" = "Linear",
  "BART" = "BART"
)
COLORS <- c(
  "NeRF+" = "black",
  "RNC" = "#DE68A1",
  "Network BART" = "#68A65E",
  "RF+" = "black",
  "Linear Regression" = "#DE68A1",
  "BART" = "#68A65E",
  # "NeRF+ (Cohesion Only)" = "#6aafe4",
  # "NeRF+ (Embedding Only)" = "#1c3145"
  "NeRF+ (Cohesion Only)" = "#714F9D",
  "NeRF+ (Embedding Only)" = "#FF9301"
)
LINETYPES <- c(
  "NeRF+" = "solid",
  "RNC" = "solid",
  "Network BART" = "solid",
  "RF+" = "dashed",
  "Linear Regression" = "dashed",
  "BART" = "dashed",
  "NeRF+ (Cohesion Only)" = "solid",
  "NeRF+ (Embedding Only)" = "solid"
)

DGP_LABS <- list(
  "Linear Additive Blockwise Network DGP" = "Linear",
  "Polynomial Additive Blockwise Network DGP" = "Polynomial",
  "LSS Additive Blockwise Network DGP" = "Locally Spiky Sparse",
  "Linear Network Autocorrelation DGP" = "Linear",
  "Polynomial Network Autocorrelation DGP" = "Polynomial",
  "LSS Network Autocorrelation DGP" = "Locally Spiky Sparse",
  "Linear Additive Blockwise Network with Real Data DGP" = "Linear",
  "Polynomial Additive Blockwise Network with Real Data DGP" = "Polynomial",
  "Linear Network Autocorrelation with Real Data DGP" = "Linear",
  "Polynomial Network Autocorrelation with Real Data DGP" = "Polynomial"
)
facet_labeller <- function(variable, value) {
  if (variable == ".dgp_name") {
    return(DGP_LABS[as.character(value)])
  } else if (variable == ".fi_mode") {
    return(stringr::str_replace_all(value, " ", "\n"))
  } else if (variable == "pve") {
    return(paste("PVE =", value))
  } else {
    return(value)
  }
}
facet_labeller_abbrv <- function(variable, value) {
  if (variable == ".dgp_name") {
    value <- DGP_LABS[as.character(value)] |>
      stringr::str_replace("Locally Spiky Sparse", "LSS")
    return(value)
  } else {
    return(facet_labeller(variable, value))
  }
}

legend_key_width <- 1.9

###########################################################################
######################### Main Simulation Results #########################
###########################################################################
eval_results_ls <- purrr::map(
  EXP_NAMES_MAIN,
  function(exp_name) {
    vary_param_name <- get_vary_param_name(exp_name)
    exp_dir <- get_exp_dir(exp_name)
    eval_results <- readRDS(file.path(exp_dir, "eval_results.rds")) |>
      purrr::map(
        ~ .x |>
          dplyr::mutate(
            .vary_param = .data[[vary_param_name]],
            .dgp_mode = dplyr::case_when(
              stringr::str_detect(exp_name, "Blockwise") ~
                "Additive Blockwise\nNetwork Effect",
              stringr::str_detect(exp_name, "Autocorrelation") ~
                "Network Autocorrelation\nEffect"
            )
          )
      )
  }
)

#### Prediction Accuracy Plots ####
pred_results_df <- purrr::map(eval_results_ls, "Prediction Accuracy") |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .method_name = factor(.method_name, levels = rev(METHOD_LEVELS)),
    .dgp_name = forcats::fct_inorder(.dgp_name)
  )

## Main Prediction Accuracy Plot (use pve = 0.4)
metric_val <- "rsq"
for (pve_val in unique(pred_results_df$pve)) {
  plt_ls <- pred_results_df |>
    dplyr::group_by(
      .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          xlab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- sprintf("(A) %s", dgp_mode)
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          xlab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- sprintf("(B) %s", dgp_mode)
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val,
            .metric == !!metric_val,
            .method_name %in% names(KEEP_METHODS)
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = .vary_param,
            y = mean_pred_err,
            color = .method_name,
            linetype = .method_name
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .vary_param,
              ymin = mean_pred_err - se_pred_err,
              ymax = mean_pred_err + se_pred_err,
              fill = .method_name
            ),
            inherit.aes = FALSE,
            alpha = 0.2
          ) +
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free", nrow = 1, labeller = facet_labeller
          ) +
          ggplot2::scale_color_manual(
            values = rev(COLORS), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::scale_fill_manual(
            values = rev(COLORS), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::scale_linetype_manual(
            values = rev(LINETYPES), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::labs(
            x = xlab,
            y = "Mean Test R-squared",
            color = "Method", fill = "Method", linetype = "Method",
            tag = tag
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          ggplot2::theme(
            legend.key.width = ggplot2::unit(legend_key_width, "cm"),
            plot.tag = ggplot2::element_text(
              size = 20, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = c(-.055, 0.52),
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
          )
      }
    )
  plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
  save_figure(plt, filename = sprintf("prediction_pve%s", pve_val), width = 14, height = 7)
}

## Supplementary Prediction Accuracy Plot (across all PVEs)
metric_val <- "rsq"
plt_ls <- pred_results_df |>
  dplyr::group_by(
    .dgp_mode
  ) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      dgp_mode <- .y$.dgp_mode[[1]]
      if (stringr::str_detect(dgp_mode, "Blockwise")) {
        xlab <- expression(
          bold(paste("Network Effect (", eta, ")"))
        )
        tag <- sprintf("(A) %s", stringr::str_replace(dgp_mode, "\n", " "))
      } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
        xlab <- expression(
          bold(paste("Network Effect (", omega, ")"))
        )
        tag <- sprintf("(B) %s", stringr::str_replace(dgp_mode, "\n", " "))
      }
      .x |>
        dplyr::filter(
          .metric == !!metric_val,
          .method_name %in% names(KEEP_METHODS)
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .vary_param,
          y = mean_pred_err,
          color = .method_name,
          linetype = .method_name
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .vary_param,
            ymin = mean_pred_err - se_pred_err,
            ymax = mean_pred_err + se_pred_err,
            fill = .method_name
          ),
          inherit.aes = FALSE,
          alpha = 0.2
        ) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::facet_grid(
          .dgp_name ~ pve, scales = "free", labeller = facet_labeller_abbrv
        ) +
        ggplot2::scale_color_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_fill_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_linetype_manual(
          values = rev(LINETYPES), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Mean Test R-squared",
          color = "Method", fill = "Method", linetype = "Method",
          title = tag
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 20, face = "italic", hjust = 0.5, vjust = 1
          ),
          legend.key.width = ggplot2::unit(legend_key_width, "cm")
        )
    }
  )
plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
save_figure(plt, filename = "prediction_full", width = 13, height = 14)

## Supplementary Prediction Accuracy Plot (across all PVEs, with different y scale)
metric_val <- "rsq"
plt_ls <- pred_results_df |>
  dplyr::group_by(
    .dgp_mode, pve
  ) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      dgp_mode <- .y$.dgp_mode[[1]]
      pve_val <- .y$pve[[1]]
      if (stringr::str_detect(dgp_mode, "Blockwise")) {
        xlab <- expression(
          bold(paste("Network Effect (", eta, ")"))
        )
        tag_pos <- -0.05
        tag <- sprintf("(A) %s", stringr::str_replace(dgp_mode, "\n", " "))
      } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
        xlab <- expression(
          bold(paste("Network Effect (", omega, ")"))
        )
        tag_pos <- -0.15
        tag <- sprintf("(B) %s", stringr::str_replace(dgp_mode, "\n", " "))
      }
      plt <- .x |>
        dplyr::filter(
          .metric == !!metric_val,
          .method_name %in% names(KEEP_METHODS)
        ) |>
        dplyr::mutate(
          pve = !!pve_val
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .vary_param,
          y = mean_pred_err,
          color = .method_name,
          linetype = .method_name
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .vary_param,
            ymin = mean_pred_err - se_pred_err,
            ymax = mean_pred_err + se_pred_err,
            fill = .method_name
          ),
          inherit.aes = FALSE,
          alpha = 0.2
        ) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::facet_grid(
          .dgp_name ~ pve, scales = "free_y", labeller = facet_labeller_abbrv
        ) +
        ggplot2::scale_color_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_fill_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_linetype_manual(
          values = rev(LINETYPES), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Mean Test R-squared",
          color = "Method", fill = "Method", linetype = "Method",
          title = tag
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 20, face = "italic", hjust = tag_pos, vjust = 1
          ),
          legend.key.width = ggplot2::unit(legend_key_width, "cm")
        )
      if (pve_val != 0.2) {
        plt <- plt +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank()
          )
      }
      if (pve_val != 0.4) {
        plt <- plt +
          ggplot2::theme(
            plot.title = ggplot2::element_blank()
          )
      }
      if (pve_val != 0.8) {
        plt <- plt +
          ggplot2::theme(
            strip.background.y = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_blank()
          )
      }
      return(plt)
    }
  )
plt <- patchwork::wrap_plots(plt_ls, ncol = 4, guides = "collect", axes = "collect_x")
save_figure(plt, filename = "prediction_full_free", width = 14.5, height = 14)

#### Feature Importance Plots ####
fi_results_df <- purrr::map(
  eval_results_ls,
  ~ dplyr::bind_rows(
    list(
      `Permutation Importance` = .x$`Permutation Feature Importances`,
      `MDI+ Importance` = .x$`MDI+ Feature Importances`
    ),
    .id = ".fi_mode"
  )
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .method_name = factor(.method_name, levels = METHOD_LEVELS),
    .dgp_name = forcats::fct_inorder(.dgp_name),
    var = dplyr::case_when(
      var == ".network" ~ "Network",
      var == ".alpha" ~ "Network Cohesion",
      var == ".embed" ~ "Network Embedding",
      TRUE ~ stringr::str_replace(var, "V", "X")
    ) |>
      factor(
        levels = c(
          "Network Cohesion", "Network Embedding", "Network",
          paste0("X", 1:10)
        )
      ),
    .fill = dplyr::case_when(
      stringr::str_detect(var, "Network") ~ "Network",
      stringr::str_detect(.dgp_name, "Linear") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 2 ~ "Signal",
      stringr::str_detect(.dgp_name, "LSS") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 6 ~ "Signal",
      stringr::str_detect(.dgp_name, "Polynomial") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 6 ~ "Signal",
      TRUE ~ "Non-signal"
    ) |>
      factor(levels = c("Network", "Signal", "Non-signal"))
  ) |>
  dplyr::filter(!is.na(var)) |>
  dplyr::arrange(.fill, centroids_scale, omega)

## Main Feature Importance Plot (use pve = 0.4)
blue_pal <- c(
  "#BCE0E9", "#89C9d9", "#57B2C8", "#235663", "#132E35"
)
orange_pal <- c(
  "#FFD499", "#FFB34D", "#FF9300", "#B36700", "#754400"
)
gray_pal <- c(
  "#CFCFCF", "#8f8f8f", "#5f5f5f", "#3B3B3B", "#242424"
)
color_pal3 <- c(blue_pal[c(1, 3, 4)], orange_pal[c(1, 3, 4)], gray_pal[c(1, 3, 5)])
color_pal4 <- c(blue_pal[1:4], orange_pal[1:4], gray_pal[1:4])
color_pal5 <- c(blue_pal, orange_pal, gray_pal)
plt_df <- fi_results_df |>
  dplyr::mutate(
    .bar_vary_param = dplyr::case_when(
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 0.5) ~ "Weak",
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 1) ~ "Moderate",
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 1.5) ~ "Strong",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.1) ~ "Weak",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.7) ~ "Moderate",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.9) ~ "Strong"
    ) |>
      factor(levels = c("Weak", "Moderate", "Strong"))
  ) |>
  dplyr::filter(
    !is.na(.bar_vary_param)
  )
for (pve_val in unique(fi_results_df$pve)) {
  plt_ls <- plt_df |>
    dplyr::filter(
      !(var %in% c("Network Cohesion", "Network Embedding", "X9", "X10")),
      .method_name == "NeRF+",
    ) |>
    dplyr::mutate(
      .dgp_name = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
        stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
        stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse (LSS)"
      ) |>
        factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse (LSS)"))
    ) |>
    dplyr::group_by(
      .fi_mode, .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        fi_mode <- .y$.fi_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- dgp_mode
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = ggplot2::element_blank()
          )
          tag_position <- c(-.065, 0.57)
          legend_position <- "none"
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- dgp_mode
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
          tag_position <- c(-.065, 0.67)
          legend_position <- "bottom"
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val
          ) |>
          dplyr::mutate(
            .fill = forcats::fct_inorder(paste(.fill, .bar_vary_param))
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = var,
            y = mean_feature_importance,
            fill = .fill
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              x = var,
              ymin = mean_feature_importance - se_feature_importance,
              ymax = mean_feature_importance + se_feature_importance,
              color = .fill,
              group = .fill
            ),
            inherit.aes = FALSE,
            width = 0,
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::geom_bar(
            linewidth = 0.2,
            stat = "identity",
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free_y", nrow = 1
          ) +
          ggplot2::scale_color_manual(
            values = color_pal
          ) +
          ggplot2::scale_fill_manual(
            values = color_pal
          ) +
          ggplot2::scale_x_discrete(
            labels = c(
              "Network" = expression(paste("(", alpha, ", Z)")),
              "Network Cohesion" = expression(alpha),
              "Network Embedding" = "Z"
            )
          ) +
          ggplot2::labs(
            x = "Feature",
            y = stringr::str_replace(fi_mode, " ", "\n"),
            tag = tag
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          x_axis_theme +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 12),
            plot.tag = ggplot2::element_text(
              size = 20, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = tag_position,
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in"),
            legend.position = legend_position
          )
      }
    )

  plt_network_ls <- plt_df |>
    dplyr::filter(
      .method_name == "NeRF+"
    ) |>
    dplyr::mutate(
      .dgp_name = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
        stringr::str_detect(.dgp_name, "Polynomial") ~ "Poly.",
        stringr::str_detect(.dgp_name, "LSS") ~ "LSS"
      ) |>
        factor(levels = c("Linear", "Poly.", "LSS")),
    ) |>
    dplyr::group_by(
      .fi_mode, .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        fi_mode <- .y$.fi_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- sprintf("(A) %s", dgp_mode)
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.title.x = ggplot2::element_blank()
          )
          legend_position <- "none"
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- sprintf("(B) %s", dgp_mode)
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme()
          legend_position <- "bottom"
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val,
            var %in% c("Network Cohesion", "Network Embedding")
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = .bar_vary_param,
            y = mean_feature_importance,
            color = .bar_vary_param,
            group = var,
            pattern = var,
            pattern_color = .bar_vary_param,
            pattern_fill = .bar_vary_param
          ) +
          ggplot2::geom_point(
            ggplot2::aes(
              x = .bar_vary_param,
              y = mean_feature_importance + se_feature_importance
            ),
            size = 0,
            color = "transparent",
            data = .x |>
              dplyr::filter(pve == !!pve_val),
            inherit.aes = FALSE
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              x = .bar_vary_param,
              ymin = mean_feature_importance,
              ymax = mean_feature_importance + se_feature_importance,
              color = .bar_vary_param,
              group = var
            ),
            inherit.aes = FALSE,
            width = 0,
            position = ggplot2::position_dodge(0.9)
          ) +
          ggpattern::geom_bar_pattern(
            pattern_density = 0.1,
            pattern_spacing = 0.1,
            fill = "transparent",
            stat = "identity",
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free_y", nrow = 1
          ) +
          ggplot2::scale_color_manual(
            values = color_pal
          ) +
          ggplot2::scale_fill_manual(
            values = color_pal
          ) +
          ggpattern::scale_pattern_manual(
            values = c(
              "Network Cohesion" = "none",
              "Network Embedding" = "stripe"
            ),
            labels = c(
              "Network Cohesion" = expression(alpha),
              "Network Embedding" = "Z"
            )
          ) +
          ggpattern::scale_pattern_color_manual(
            values = color_pal
          ) +
          ggpattern::scale_pattern_fill_manual(
            values = color_pal
          ) +
          ggplot2::labs(
            x = "Network Effect",
            y = stringr::str_replace(fi_mode, " ", "\n"),
            color = "", fill = "", pattern = "", pattern_color = ""
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          x_axis_theme +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title.y = ggplot2::element_blank(),
            legend.position = legend_position
          ) +
          ggplot2::guides(
            color = "none", fill = "none"
          )
      }
    )
  
  plt <- patchwork::wrap_plots(
    c(
      plt_ls[1:2],
      list(patchwork::plot_spacer(), patchwork::plot_spacer()),
      plt_network_ls[1:2]
    ),
    nrow = 2, ncol = 3, byrow = FALSE, widths = c(0.73, 0.02, 0.25)
  )
  save_figure(
    plt, filename = sprintf("fi_mdiplus_%s", pve_val),
    width = 16, height = 9
  )

  plt <- patchwork::wrap_plots(
    c(
      plt_ls[3:4],
      list(patchwork::plot_spacer(), patchwork::plot_spacer()),
      plt_network_ls[3:4]
    ),
    nrow = 2, ncol = 3, byrow = FALSE, widths = c(0.73, 0.02, 0.25)
  )
  save_figure(
    plt, filename = sprintf("fi_permutation_%s", pve_val),
    width = 16, height = 9
  )
}

###########################################################################
################## Main Simulation with Real Data Results #################
###########################################################################
eval_results_ls <- purrr::map(
  EXP_NAMES_REAL,
  function(exp_name) {
    vary_param_name <- get_vary_param_name(exp_name)
    exp_dir <- get_exp_dir(exp_name)
    eval_results <- readRDS(file.path(exp_dir, "eval_results.rds")) |>
      purrr::map(
        ~ .x |>
          dplyr::mutate(
            .vary_param = .data[[vary_param_name]],
            .dgp_mode = dplyr::case_when(
              stringr::str_detect(exp_name, "Blockwise") ~
                "Additive Blockwise\nNetwork Effect",
              stringr::str_detect(exp_name, "Autocorrelation") ~
                "Network Autocorrelation\nEffect"
            )
          )
      )
  }
)

#### Prediction Accuracy Plots ####
pred_results_df <- purrr::map(eval_results_ls, "Prediction Accuracy") |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .method_name = factor(.method_name, levels = rev(METHOD_LEVELS)),
    .dgp_name = forcats::fct_inorder(.dgp_name)
  )

## Main Prediction Accuracy Plot (use pve = 0.4)
metric_val <- "rsq"
for (pve_val in unique(pred_results_df$pve)) {
  plt_ls <- pred_results_df |>
    dplyr::group_by(
      .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          xlab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- sprintf("(A) %s", dgp_mode)
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          xlab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- sprintf("(B) %s", dgp_mode)
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val,
            .metric == !!metric_val,
            .method_name %in% names(KEEP_METHODS)
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = .vary_param,
            y = mean_pred_err,
            color = .method_name,
            linetype = .method_name
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .vary_param,
              ymin = mean_pred_err - se_pred_err,
              ymax = mean_pred_err + se_pred_err,
              fill = .method_name
            ),
            inherit.aes = FALSE,
            alpha = 0.2
          ) +
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free", nrow = 1, labeller = facet_labeller
          ) +
          ggplot2::scale_color_manual(
            values = rev(COLORS), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::scale_fill_manual(
            values = rev(COLORS), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::scale_linetype_manual(
            values = rev(LINETYPES), labels = KEEP_METHODS,
            guide = ggplot2::guide_legend(reverse = TRUE)
          ) +
          ggplot2::labs(
            x = xlab,
            y = "Mean Test R-squared",
            color = "Method", fill = "Method", linetype = "Method",
            tag = tag
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          ggplot2::theme(
            legend.key.width = ggplot2::unit(legend_key_width, "cm"),
            plot.tag = ggplot2::element_text(
              size = 20, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = c(-.055, 0.52),
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
          )
      }
    )
  plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
  save_figure(plt, filename = sprintf("prediction_real_data_pve%s", pve_val), width = 10, height = 7)
}

## Supplementary Prediction Accuracy Plot (across all PVEs)
metric_val <- "rsq"
plt_ls <- pred_results_df |>
  dplyr::group_by(
    .dgp_mode
  ) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      dgp_mode <- .y$.dgp_mode[[1]]
      if (stringr::str_detect(dgp_mode, "Blockwise")) {
        xlab <- expression(
          bold(paste("Network Effect (", eta, ")"))
        )
        tag <- sprintf("(A) %s", stringr::str_replace(dgp_mode, "\n", " "))
      } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
        xlab <- expression(
          bold(paste("Network Effect (", omega, ")"))
        )
        tag <- sprintf("(B) %s", stringr::str_replace(dgp_mode, "\n", " "))
      }
      .x |>
        dplyr::filter(
          .metric == !!metric_val,
          .method_name %in% names(KEEP_METHODS)
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .vary_param,
          y = mean_pred_err,
          color = .method_name,
          linetype = .method_name
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .vary_param,
            ymin = mean_pred_err - se_pred_err,
            ymax = mean_pred_err + se_pred_err,
            fill = .method_name
          ),
          inherit.aes = FALSE,
          alpha = 0.2
        ) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::facet_grid(
          .dgp_name ~ pve, scales = "free", labeller = facet_labeller_abbrv
        ) +
        ggplot2::scale_color_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_fill_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_linetype_manual(
          values = rev(LINETYPES), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Mean Test R-squared",
          color = "Method", fill = "Method", linetype = "Method",
          title = tag
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 20, face = "italic", hjust = 0.5, vjust = 1
          ),
          legend.key.width = ggplot2::unit(legend_key_width, "cm")
        )
    }
  )
plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
save_figure(plt, filename = "prediction_full_real_data", width = 13, height = 11)

## Supplementary Prediction Accuracy Plot (across all PVEs, with different y scale)
metric_val <- "rsq"
plt_ls <- pred_results_df |>
  dplyr::group_by(
    .dgp_mode, pve
  ) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      dgp_mode <- .y$.dgp_mode[[1]]
      pve_val <- .y$pve[[1]]
      if (stringr::str_detect(dgp_mode, "Blockwise")) {
        xlab <- expression(
          bold(paste("Network Effect (", eta, ")"))
        )
        tag_pos <- -0.05
        tag <- sprintf("(A) %s", stringr::str_replace(dgp_mode, "\n", " "))
      } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
        xlab <- expression(
          bold(paste("Network Effect (", omega, ")"))
        )
        tag_pos <- -0.15
        tag <- sprintf("(B) %s", stringr::str_replace(dgp_mode, "\n", " "))
      }
      plt <- .x |>
        dplyr::filter(
          .metric == !!metric_val,
          .method_name %in% names(KEEP_METHODS)
        ) |>
        dplyr::mutate(
          pve = !!pve_val
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .vary_param,
          y = mean_pred_err,
          color = .method_name,
          linetype = .method_name
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            x = .vary_param,
            ymin = mean_pred_err - se_pred_err,
            ymax = mean_pred_err + se_pred_err,
            fill = .method_name
          ),
          inherit.aes = FALSE,
          alpha = 0.2
        ) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::facet_grid(
          .dgp_name ~ pve, scales = "free_y", labeller = facet_labeller_abbrv
        ) +
        ggplot2::scale_color_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_fill_manual(
          values = rev(COLORS), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::scale_linetype_manual(
          values = rev(LINETYPES), labels = KEEP_METHODS,
          guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Mean Test R-squared",
          color = "Method", fill = "Method", linetype = "Method",
          title = tag
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 20, face = "italic", hjust = tag_pos, vjust = 1
          ),
          legend.key.width = ggplot2::unit(legend_key_width, "cm")
        )
      if (pve_val != 0.2) {
        plt <- plt +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank()
          )
      }
      if (pve_val != 0.4) {
        plt <- plt +
          ggplot2::theme(
            plot.title = ggplot2::element_blank()
          )
      }
      if (pve_val != 0.8) {
        plt <- plt +
          ggplot2::theme(
            strip.background.y = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_blank()
          )
      }
      return(plt)
    }
  )
plt <- patchwork::wrap_plots(plt_ls, ncol = 4, guides = "collect", axes = "collect_x")
save_figure(plt, filename = "prediction_full_free_real_data", width = 14.5, height = 11)

#### Feature Importance Plots ####
fi_results_df <- purrr::map(
  eval_results_ls,
  ~ dplyr::bind_rows(
    list(
      `Permutation Importance` = .x$`Permutation Feature Importances`,
      `MDI+ Importance` = .x$`MDI+ Feature Importances`
    ),
    .id = ".fi_mode"
  )
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .method_name = factor(.method_name, levels = METHOD_LEVELS),
    .dgp_name = forcats::fct_inorder(.dgp_name),
    var = dplyr::case_when(
      var == ".network" ~ "Network",
      var == ".alpha" ~ "Network Cohesion",
      var == ".embed" ~ "Network Embedding",
      TRUE ~ stringr::str_replace(var, "V", "X")
    ) |>
      factor(
        levels = c(
          "Network Cohesion", "Network Embedding", "Network",
          paste0("X", 1:10)
        )
      ),
    .fill = dplyr::case_when(
      stringr::str_detect(var, "Network") ~ "Network",
      stringr::str_detect(.dgp_name, "Linear") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 2 ~ "Signal",
      stringr::str_detect(.dgp_name, "LSS") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 6 ~ "Signal",
      stringr::str_detect(.dgp_name, "Polynomial") &
        as.numeric(stringr::str_extract(var, "[0-9]+")) <= 6 ~ "Signal",
      TRUE ~ "Non-signal"
    ) |>
      factor(levels = c("Network", "Signal", "Non-signal"))
  ) |>
  dplyr::filter(!is.na(var)) |>
  dplyr::arrange(.fill, centroids_scale, omega)

## Main Feature Importance Plot (use pve = 0.4)
blue_pal <- c(
  "#BCE0E9", "#89C9d9", "#57B2C8", "#235663", "#132E35"
)
orange_pal <- c(
  "#FFD499", "#FFB34D", "#FF9300", "#B36700", "#754400"
)
gray_pal <- c(
  "#CFCFCF", "#8f8f8f", "#5f5f5f", "#3B3B3B", "#242424"
)
color_pal3 <- c(blue_pal[c(1, 3, 4)], orange_pal[c(1, 3, 4)], gray_pal[c(1, 3, 5)])
color_pal4 <- c(blue_pal[1:4], orange_pal[1:4], gray_pal[1:4])
color_pal5 <- c(blue_pal, orange_pal, gray_pal)
plt_df <- fi_results_df |>
  dplyr::mutate(
    .bar_vary_param = dplyr::case_when(
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 0.5) ~ "Weak",
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 1) ~ "Moderate",
      stringr::str_detect(.dgp_mode, "Blockwise") & (.vary_param == 1.5) ~ "Strong",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.1) ~ "Weak",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.7) ~ "Moderate",
      stringr::str_detect(.dgp_mode, "Autocorrelation") & (.vary_param == 0.9) ~ "Strong"
    ) |>
      factor(levels = c("Weak", "Moderate", "Strong"))
  ) |>
  dplyr::filter(
    !is.na(.bar_vary_param)
  )
for (pve_val in unique(fi_results_df$pve)) {
  plt_ls <- plt_df |>
    dplyr::filter(
      !(var %in% c("Network Cohesion", "Network Embedding", "X9", "X10")),
      .method_name == "NeRF+",
    ) |>
    dplyr::mutate(
      .dgp_name = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
        stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
        stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse (LSS)"
      ) |>
        factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse (LSS)"))
    ) |>
    dplyr::group_by(
      .fi_mode, .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        fi_mode <- .y$.fi_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- dgp_mode
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.title.x = ggplot2::element_blank()
          )
          tag_position <- c(-.065, 0.57)
          legend_position <- "none"
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- dgp_mode
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
          )
          tag_position <- c(-.065, 0.67)
          legend_position <- "bottom"
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val
          ) |>
          dplyr::mutate(
            .fill = forcats::fct_inorder(paste(.fill, .bar_vary_param))
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = var,
            y = mean_feature_importance,
            fill = .fill
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              x = var,
              ymin = mean_feature_importance - se_feature_importance,
              ymax = mean_feature_importance + se_feature_importance,
              color = .fill,
              group = .fill
            ),
            inherit.aes = FALSE,
            width = 0,
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::geom_bar(
            linewidth = 0.2,
            stat = "identity",
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free_y", nrow = 1
          ) +
          ggplot2::scale_color_manual(
            values = color_pal
          ) +
          ggplot2::scale_fill_manual(
            values = color_pal
          ) +
          ggplot2::scale_x_discrete(
            labels = c(
              "Network" = expression(paste("(", alpha, ", Z)")),
              "Network Cohesion" = expression(alpha),
              "Network Embedding" = "Z"
            )
          ) +
          ggplot2::labs(
            x = "Feature",
            y = stringr::str_replace(fi_mode, " ", "\n"),
            tag = tag
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          x_axis_theme +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 12),
            plot.tag = ggplot2::element_text(
              size = 20, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = tag_position,
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in"),
            legend.position = legend_position
          )
      }
    )

  plt_network_ls <- plt_df |>
    dplyr::filter(
      .method_name == "NeRF+"
    ) |>
    dplyr::mutate(
      .dgp_name = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
        stringr::str_detect(.dgp_name, "Polynomial") ~ "Poly.",
        stringr::str_detect(.dgp_name, "LSS") ~ "LSS"
      ) |>
        factor(levels = c("Linear", "Poly.", "LSS")),
    ) |>
    dplyr::group_by(
      .fi_mode, .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        fi_mode <- .y$.fi_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", eta, ")"))
          )
          tag <- sprintf("(A) %s", dgp_mode)
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme(
            axis.title.x = ggplot2::element_blank()
          )
          legend_position <- "none"
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          fill_lab <- expression(
            bold(paste("Network Effect (", omega, ")"))
          )
          tag <- sprintf("(B) %s", dgp_mode)
          color_pal <- color_pal3
          x_axis_theme <- ggplot2::theme()
          legend_position <- "bottom"
        }
        .x |>
          dplyr::filter(
            pve == !!pve_val,
            var %in% c("Network Cohesion", "Network Embedding")
          ) |>
          ggplot2::ggplot() +
          ggplot2::aes(
            x = .bar_vary_param,
            y = mean_feature_importance,
            color = .bar_vary_param,
            group = var,
            pattern = var,
            pattern_color = .bar_vary_param,
            pattern_fill = .bar_vary_param
          ) +
          ggplot2::geom_point(
            ggplot2::aes(
              x = .bar_vary_param,
              y = mean_feature_importance + se_feature_importance
            ),
            size = 0,
            color = "transparent",
            data = .x |>
              dplyr::filter(pve == !!pve_val),
            inherit.aes = FALSE
          ) +
          ggplot2::geom_errorbar(
            ggplot2::aes(
              x = .bar_vary_param,
              ymin = mean_feature_importance,
              ymax = mean_feature_importance + se_feature_importance,
              color = .bar_vary_param,
              group = var
            ),
            inherit.aes = FALSE,
            width = 0,
            position = ggplot2::position_dodge(0.9)
          ) +
          ggpattern::geom_bar_pattern(
            pattern_density = 0.1,
            pattern_spacing = 0.1,
            fill = "transparent",
            stat = "identity",
            position = ggplot2::position_dodge(0.9)
          ) +
          ggplot2::facet_wrap(
            ~ .dgp_name, scales = "free_y", nrow = 1
          ) +
          ggplot2::scale_color_manual(
            values = color_pal
          ) +
          ggplot2::scale_fill_manual(
            values = color_pal
          ) +
          ggpattern::scale_pattern_manual(
            values = c(
              "Network Cohesion" = "none",
              "Network Embedding" = "stripe"
            ),
            labels = c(
              "Network Cohesion" = expression(alpha),
              "Network Embedding" = "Z"
            )
          ) +
          ggpattern::scale_pattern_color_manual(
            values = color_pal
          ) +
          ggpattern::scale_pattern_fill_manual(
            values = color_pal
          ) +
          ggplot2::labs(
            x = "Network Effect",
            y = stringr::str_replace(fi_mode, " ", "\n"),
            color = "", fill = "", pattern = "", pattern_color = ""
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          x_axis_theme +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y = ggplot2::element_text(size = 12),
            axis.title.y = ggplot2::element_blank(),
            legend.position = legend_position
          ) +
          ggplot2::guides(
            color = "none", fill = "none"
          )
      }
    )
  
  plt <- patchwork::wrap_plots(
    c(
      plt_ls[1:2],
      list(patchwork::plot_spacer(), patchwork::plot_spacer()),
      plt_network_ls[1:2]
    ),
    nrow = 2, ncol = 3, byrow = FALSE, widths = c(0.73, 0.02, 0.25)
  )
  save_figure(
    plt, filename = sprintf("fi_mdiplus_real_data_%s", pve_val),
    width = 14, height = 9
  )

  plt <- patchwork::wrap_plots(
    c(
      plt_ls[3:4],
      list(patchwork::plot_spacer(), patchwork::plot_spacer()),
      plt_network_ls[3:4]
    ),
    nrow = 2, ncol = 3, byrow = FALSE, widths = c(0.73, 0.02, 0.25)
  )
  save_figure(
    plt, filename = sprintf("fi_permutation_real_data_%s", pve_val),
    width = 14, height = 9
  )
}

###########################################################################
####################### Outliers Simulation Results #######################
###########################################################################
outlier_idxs <- 1
eval_results_summary <- purrr::map(
  EXP_NAMES_OUTLIERS,
  function(exp_name) {
    fit_results <- readRDS(
      file.path(SAVE_DIR, "results", "Main Simulations", exp_name, "Varying outliers_scale", "fit_results.rds")
    ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        loo_df = list(
          data.frame(
            .sample_id = 1:length(loo$mean_change_alphas),
            .is_outlier = ifelse(
              1:length(loo$mean_change_alphas) %in% outlier_idxs,
              "Outlier", "Not Outlier"
            ),
            change_alphas = loo$mean_change_alphas,
            change_alphas_rank = rank(-loo$mean_change_alphas),
            change_betas = loo$mean_change_betas,
            change_betas_rank = rank(-loo$mean_change_betas),
            change_preds_test = loo$mean_change_preds_test,
            change_preds_test_rank = rank(-loo$mean_change_preds_test)
          )
        )
      ) |>
      tidyr::unnest(loo_df) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        cols = c(
          change_alphas,
          change_alphas_rank,
          change_betas,
          change_betas_rank,
          change_preds_test,
          change_preds_test_rank
        ),
        names_to = ".metric",
        values_to = "value"
      ) |>
      dplyr::group_by(
        dplyr::across(
          c(
            .dgp_name, .method_name, outliers_scale,
            .is_outlier, .metric
          )
        )
      ) |>
      dplyr::summarise(
        prop1_value = mean(value == 1),
        mean_value = mean(value),
        se_value = sd(value) / sqrt(dplyr::n()),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        outliers_scale = unlist(outliers_scale),
        .dgp_name = stringr::str_replace(.dgp_name, "with Outliers DGP", "Effect")
      )
  }
) |>
  dplyr::bind_rows()

keep_metrics <- unique(eval_results_summary$.metric)[
  stringr::str_detect(unique(eval_results_summary$.metric), "rank")
]
for (metric_name in keep_metrics) {
  plt_ls <- eval_results_summary |>
    dplyr::filter(
      .is_outlier == "Outlier",
      .metric == !!metric_name
    ) |>
    dplyr::mutate(
      .dgp_mode = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Blockwise") ~ "Additive Blockwise\nNetwork Effect",
        stringr::str_detect(.dgp_name, "Autocorrelation") ~ "Network Autocorrelation\nEffect"
      ) |>
        factor(levels = c("Additive Blockwise\nNetwork Effect", "Network Autocorrelation\nEffect")),
      .dgp_name = dplyr::case_when(
        stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
        stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
        stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse"
      ) |>
        factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse"))
    ) |>
    dplyr::group_by(
      .dgp_mode
    ) |>
    dplyr::group_map(
      .f = function(.x, .y) {
        dgp_mode <- .y$.dgp_mode[[1]]
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          tag <- sprintf("(A) %s", dgp_mode)
        } else if (stringr::str_detect(dgp_mode, "Autocorrelation")) {
          tag <- sprintf("(B) %s", dgp_mode)
        }
        plt <- ggplot2::ggplot(.x) +
          ggplot2::aes(
            x = outliers_scale, 
            y = mean_value
          ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = outliers_scale,
              ymin = mean_value - se_value,
              ymax = mean_value + se_value
            ),
            inherit.aes = FALSE,
            alpha = 0.2
          ) +
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::facet_wrap(~ .dgp_name, nrow = 1) +
          ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
          ggplot2::scale_y_reverse() +
          ggplot2::labs(
            x = expression(bold(paste("Outlier Strength (", kappa, ")"))), 
            y = "Influence Rank\nof Outlier",
            tag = tag
          ) +
          vthemes::theme_vmodern(size_preset = "large") +
          ggplot2::theme(
            plot.tag = ggplot2::element_text(
              size = 18, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = c(-.065, 0.52),
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
          )
        if (stringr::str_detect(dgp_mode, "Blockwise")) {
          plt <- plt +
            ggplot2::theme(
              axis.title.x = ggplot2::element_blank()
            )
        } else {
          plt <- plt +
            ggplot2::theme(
              strip.background.x = ggplot2::element_blank(),
              strip.text.x = ggplot2::element_blank()
            )
        }
        return(plt)
      }
    )
  plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
  save_figure(
    plt,
    filename = sprintf("outlier_%s_plot", metric_name),
    width = 11, height = 6
  )
}

###########################################################################
######################## Sample Influences Results ########################
###########################################################################
results_ls <- purrr::map(
  EXP_NAMES_MAIN,
  ~ readRDS(file.path(SAVE_DIR, "results", "LOO Simulations", .x, "results.rds"))
) |>
  setNames(EXP_NAMES_MAIN)
aloo_ls <- purrr::map(results_ls, "aloo_out")
full_loo_ls <- purrr::map(results_ls, "nerfplus_loo_out")

point_size <- 0.2
loo_sample_ids <- 1:50
tree_ids <- 1

## look at LOO alpha parameters
indiv_alphas_aloo <- purrr::map(
  aloo_ls,
  function(aloo_out) {
    purrr::map(
      aloo_out$loo_params,
      function(aloo_tree_out) {
        purrr::map(
          1:ncol(aloo_tree_out$alpha_loo),
          function(i) {
            tibble::tibble(
              .loo_sample_id = i,
              .sample_id = 1:nrow(aloo_tree_out$alpha_loo),
              `Approximate LOO` = aloo_tree_out$alpha_loo[, i] - mean(aloo_tree_out$alpha_loo[, i])
            )[-i, ]
          }
        ) |> 
          purrr::list_rbind()
      }
    ) |> 
      purrr::list_rbind(names_to = ".tree_id")
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

indiv_alphas_full <- purrr::map(
  full_loo_ls,
  function(full_loo_out) {
    purrr::imap(
      full_loo_out,
      function(sample_out, loo_sample_id) {
        purrr::map(
          1:length(sample_out$alphas_ls),
          function(tree_id) {
            tibble::tibble(
              .tree_id = tree_id,
              .loo_sample_id = loo_sample_id,
              .sample_id = setdiff(1:length(full_loo_out), loo_sample_id),
              `LOO (full)` = c(sample_out$alphas_ls[[tree_id]]) - mean(sample_out$alphas_ls[[tree_id]])
            )
          }
        ) |> 
          purrr::list_rbind()
      }
    ) |> 
      purrr::list_rbind()
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

plt_df_indiv_alphas <- indiv_alphas_aloo |> 
  dplyr::left_join(
    indiv_alphas_full, 
    by = c(".dgp_name", ".tree_id", ".loo_sample_id", ".sample_id")
  ) |> 
  dplyr::filter(
    .loo_sample_id %in% !!loo_sample_ids,
    .tree_id %in% !!tree_ids
  )

# alphas averaged across forest
mean_alphas_aloo <- indiv_alphas_aloo |> 
  dplyr::group_by(.dgp_name, .loo_sample_id, .sample_id) |> 
  dplyr::summarize(
    `Approximate LOO` = mean(`Approximate LOO`),
    .groups = "drop"
  )
mean_alphas_full <- indiv_alphas_full |> 
  dplyr::group_by(.dgp_name, .loo_sample_id, .sample_id) |> 
  dplyr::summarize(
    `LOO (full)` = mean(`LOO (full)`),
    .groups = "drop"
  )
plt_df_mean_alphas <- mean_alphas_aloo |> 
  dplyr::left_join(
    mean_alphas_full, 
    by = c(".dgp_name", ".loo_sample_id", ".sample_id")
  )

# plot alphas per tree and across forest
plt_ls <- dplyr::bind_rows(  
  plt_df_indiv_alphas |> dplyr::mutate(.mode = "Individual Tree"),
  plt_df_mean_alphas |> dplyr::mutate(.mode = "Averaged Across Forest")
) |> 
  dplyr::mutate(
    .mode = factor(.mode, levels = c("Individual Tree", "Averaged Across Forest")),
    .dgp_mode = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Blockwise") ~ "(A) Additive Blockwise Network Effect",
      TRUE ~ "(B) Network Autocorrelation Effect"
    ),
    .dgp_name = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
      stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
      stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse"
    ) |>
      factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse"))
  ) |>
  dplyr::group_by(.dgp_name, .dgp_mode) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      cur_dgp_name <- .y$.dgp_name[[1]]
      cur_dgp_mode <- .y$.dgp_mode[[1]]
      limits <- c(
        -max(abs(c(.x$`Approximate LOO`, .x$`LOO (full)`))),
        max(abs(c(.x$`Approximate LOO`, .x$`LOO (full)`)))
      )
      plt <- .x |>
        dplyr::mutate(
          .cur_dgp_name = cur_dgp_name
        ) |>
        ggplot2::ggplot() +
        ggplot2::geom_abline(slope = 1, intercept = 0, color = "black") +
        ggplot2::geom_point(
          ggplot2::aes(
            x = `Approximate LOO`,
            y = `LOO (full)`
          ),
          size = point_size
        ) +
        ggplot2::facet_grid(.mode ~ .cur_dgp_name) +
        vthemes::scale_color_vmodern(discrete = FALSE) +
        ggplot2::xlim(limits) +
        ggplot2::ylim(limits) +
        ggplot2::labs(
          x = expression(
            bold(paste("Estimated LOO ", alpha, ""))
          ),
          y = expression(
            bold(paste("True LOO ", alpha, " (fully-refitted)"))
          ),
          color = "LOO\nSample ID"
        ) +
        vthemes::theme_vmodern(size_preset = "large")
      if (cur_dgp_name != "Locally Spiky Sparse") {
        plt <- plt + 
          ggplot2::theme(
            strip.background.y = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_blank()
          )
      }
      if (cur_dgp_name != "Linear") {
        plt <- plt +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank()
          )
      }
      if (cur_dgp_name == "Linear") {
        plt <- plt +
          ggplot2::labs(
            tag = cur_dgp_mode
          ) +
          ggplot2::theme(
            plot.tag = ggplot2::element_text(
              size = 20, face = "italic", angle = 90, hjust = 0.5, vjust = 1
            ),
            plot.tag.position = c(-.1, 0.52),
            plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
          )
      }
      return(plt)
    }
  )
plt_alpha <- patchwork::wrap_plots(plt_ls, nrow = 2, byrow = FALSE) +
  patchwork::plot_layout(axes = "collect_x", guides = "collect")
ggplot2::ggsave(
  plt_alpha,
  filename = file.path(FIG_DIR, "loo_alphas.pdf"),
  width = 13, height = 14
)

## look at LOO training and test predictions
preds_aloo <- purrr::map(
  aloo_ls,
  function(aloo_out) {
    tibble::tibble(
      .loo_sample_id = 1:nrow(aloo_out$loo_preds),
      `Approximate LOO` = diag(aloo_out$loo_preds)
    )
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

preds_full <- purrr::map(
  full_loo_ls, 
  function(full_loo_out) {
    purrr::map2(
      full_loo_out, 1:length(full_loo_out),
      function(sample_out, i) {
        tibble::tibble(
          .loo_sample_id = i,
          `LOO (full)` = sample_out$loo_pred
        )
      }
    ) |> 
      purrr::list_rbind()
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

plt_df <- preds_aloo |> 
  dplyr::left_join(
    preds_full, 
    by = c(".dgp_name", ".loo_sample_id")
  ) |>
  dplyr::mutate(
    .dgp_mode = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Blockwise") ~ "Additive Blockwise\nNetwork Effect",
      TRUE ~ "Network Autocorrelation\nEffect"
    ),
    .dgp_name = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
      stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
      stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse"
    ) |>
      factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse"))
  )

plt_train_ls <- plt_df |> 
  dplyr::group_by(.dgp_mode) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      cur_dgp_mode <- .y$.dgp_mode[[1]]
      plt <- ggplot2::ggplot(.x) +
        ggplot2::geom_abline(slope = 1, intercept = 0, color = "black") +
        ggplot2::geom_point(
          ggplot2::aes(
            x = `Approximate LOO`,
            y = `LOO (full)`
          ),
          size = point_size * 2.5
        ) +
        ggplot2::facet_wrap(~ .dgp_name, scales = "free", nrow = 1) +
        vthemes::scale_color_vmodern(discrete = FALSE) +
        ggplot2::labs(
          tag = cur_dgp_mode,
          x = "Estimated LOO Predictions",
          y = "True LOO Predictions (fully-refitted)",
          color = "LOO\nSample ID"
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.tag = ggplot2::element_text(
            size = 18, face = "italic", angle = 90, hjust = 0.5, vjust = 1
          ),
          plot.tag.position = c(-.0815, 0.52),
          plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
        )
      if (stringr::str_detect(cur_dgp_mode, "Autocorrelation")) {
        plt <- plt +
          ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank()
          )
      }
      return(plt)
    }
  )

## look at LOO test predictions and prediction error
preds_aloo <- purrr::map(
  aloo_ls,
  function(aloo_out) {
    purrr::map(
      1:ncol(aloo_out$loo_preds_test), 
      function(i) {
        tibble::tibble(
          .sample_id = 1:nrow(aloo_out$loo_preds_test),
          `Approximate LOO` = aloo_out$loo_preds_test[, i]
        )
      }
    ) |> 
      purrr::list_rbind(names_to = ".loo_sample_id")
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

preds_full <- purrr::map(
  full_loo_ls,
  function(full_loo_out) {
    purrr::map(
      full_loo_out, 
      function(sample_out) {
        preds <- purrr::reduce(sample_out$preds_ls, `+`) / length(sample_out$preds_ls)
        tibble::tibble(
          .sample_id = 1:length(preds),
          `LOO (full)` = c(preds)
        )
      }
    ) |> 
      purrr::list_rbind(names_to = ".loo_sample_id")
  }
) |>
  dplyr::bind_rows(.id = ".dgp_name")

plt_df <- preds_aloo |> 
  dplyr::left_join(
    preds_full, 
    by = c(".dgp_name", ".loo_sample_id", ".sample_id")
  ) |>
  dplyr::mutate(
    .dgp_mode = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Blockwise") ~ "Additive Blockwise\nNetwork Effect",
      TRUE ~ "Network Autocorrelation\nEffect"
    ),
    .dgp_name = dplyr::case_when(
      stringr::str_detect(.dgp_name, "Linear") ~ "Linear",
      stringr::str_detect(.dgp_name, "Polynomial") ~ "Polynomial",
      stringr::str_detect(.dgp_name, "LSS") ~ "Locally Spiky Sparse"
    ) |>
      factor(levels = c("Linear", "Polynomial", "Locally Spiky Sparse"))
  )

plt_test_ls <- plt_df |> 
  dplyr::group_by(.dgp_mode) |>
  dplyr::group_map(
    .f = function(.x, .y) {
      cur_dgp_mode <- .y$.dgp_mode[[1]]
      plt <- ggplot2::ggplot(.x) +
        ggplot2::geom_abline(slope = 1, intercept = 0, color = "black") +
        ggplot2::geom_point(
          ggplot2::aes(
            x = `Approximate LOO`,
            y = `LOO (full)`
          ),
          size = point_size
        ) +
        ggplot2::facet_wrap(~ .dgp_name, scales = "free", nrow = 1) +
        vthemes::scale_color_vmodern(discrete = FALSE) +
        ggplot2::labs(
          tag = cur_dgp_mode,
          x = "Estimated LOO Test Predictions",
          y = "True LOO Test Predictions (fully-refitted)",
          color = "LOO\nSample ID"
        ) +
        vthemes::theme_vmodern(size_preset = "large") +
        ggplot2::theme(
          plot.tag = ggplot2::element_text(
            size = 18, face = "italic", angle = 90, hjust = 0.5, vjust = 1
          ),
          plot.tag.position = c(-.0815, 0.52),
          plot.margin = ggplot2::unit(c(0.05, 0, 0.1, 0.75), units = "in")
        )
      if (stringr::str_detect(cur_dgp_mode, "Autocorrelation")) {
        plt <- plt +
          ggplot2::theme(
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank()
          )
      }
      return(plt)
    }
  )

plt_train <- patchwork::wrap_plots(plt_train_ls, nrow = 2) +
  patchwork::plot_layout(axes = "collect")
plt_test <- patchwork::wrap_plots(plt_test_ls, nrow = 2) +
  patchwork::plot_layout(axes = "collect")
plt <- patchwork::wrap_plots(
  plt_train, patchwork::plot_spacer(), plt_test,
  nrow = 3, heights = c(1, 0.02, 1)
)
ggplot2::ggsave(
  plt,
  filename = file.path(FIG_DIR, "loo_predictions_all.pdf"),
  width = 10.5, height = 13
)

###########################################################################
####################### School Conflict Case Study #######################
###########################################################################
impute_mode_dir <- file.path(RESULTS_DIR, "School Conflict (new)")
schoolids <- setdiff(list.files(impute_mode_dir), "docs")
eval_results_ls <- purrr::map(schoolids, ~ readRDS(file.path(impute_mode_dir, .x, "eval_results.rds")))

## Prediction Results
pred_results_df <- purrr::map(eval_results_ls, "Prediction Accuracy") |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .schid = purrr::map_chr(.dgp_name, ~ stringr::str_extract(.x, "[0-9]+"))
  ) |>
  dplyr::filter(
    .metric == "rmse"
  )
nerf_preds_df <- pred_results_df |> 
  dplyr::filter(
    stringr::str_detect(.method_name, "NeRF")
  ) |> 
  dplyr::group_by(.schid) |> 
  dplyr::filter(
    mean_pred_err == min(mean_pred_err)
  ) |> 
  dplyr::mutate(
    .method_name = "NeRF+"
  )
pred_results_df <- pred_results_df |> 
  dplyr::filter(
    !stringr::str_detect(.method_name, "NeRF"),
  ) |>
  dplyr::bind_rows(nerf_preds_df)

# get basic stats about each school
SCHIDS <- c(
  "1", "10", "13", "19", 
  "20", "21", "22", "24", "26", "27", "29",
  "3", "31", "33", "34", "35",
  "40", "42", "44", "45", "48", "49",
  "51", "56", "58", "6", "60", "9"
)
data_stats_ls <- list()
for (SCHID in SCHIDS) {
  print(SCHID)
  dgp_name <- "School Conflict"
  dgp <- create_dgp(
    .dgp_fun = load_school_conflict_data, 
    .name = dgp_name,
    keep_schools = SCHID,
    include_w1 = TRUE,
    impute_mode = "none",
    train_prop = 1,
    response_type = "PNW2",
    network_type = "A",
    connected = TRUE
  )
  data_list <- dgp$generate()
  data_stats_ls[[as.character(SCHID)]] <- data.frame(
    `# Samples` = nrow(data_list$x), 
    `Var(y)` = var(data_list$y),
    check.names = FALSE
  )
}
data_stats_df <- dplyr::bind_rows(data_stats_ls, .id = ".schid")

# convert RMSE to R^2
pred_results_r2_df <- pred_results_df |>
  dplyr::left_join(data_stats_df, by = ".schid") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    raw_pred_err = list(1 - (raw_pred_err^2 / `Var(y)`) * (`# Samples` / (`# Samples` - 1))),
    mean_pred_err = mean(raw_pred_err),
    se_pred_err = sd(raw_pred_err) / sqrt(length(raw_pred_err))
  )

# number of schools where RF performs better than linear regression
pred_results_r2_df |>
  tidyr::pivot_wider(
    names_from = ".method_name",
    values_from = "mean_pred_err",
    id_cols = ".dgp_name"
  ) |>
  dplyr::filter(`RF` >= `Linear Regression`)

# method ranks
rank_df <- pred_results_r2_df |>
  # dplyr::filter(
  #   .method_name != "RF"
  # ) |>
  dplyr::group_by(.schid) |>
  dplyr::mutate(
    rank = paste0("Rank ", rank(-mean_pred_err))
  ) |>
  dplyr::select(
    .schid, .dgp_name, .method_name, rank
  ) |>
  dplyr::group_by(.method_name, rank) |>
  dplyr::summarize(
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = rank,
    values_from = n,
    values_fill = 0
  ) |>
  dplyr::select(
    .method_name, tidyselect::any_of(paste0("Rank ", as.character(1:10)))
  ) |>
  dplyr::mutate(
    .method_name = factor(
      .method_name, 
      levels = c(names(KEEP_METHODS), "RF")
    )
  ) |>
  dplyr::arrange(
    .method_name
  ) |>
  dplyr::rename(
    "Method" = ".method_name"
  )
print(rank_df)
vthemes::pretty_kable(rank_df, format = "latex")

# method r2 averages
metric_df <- pred_results_r2_df |> 
  dplyr::group_by(.method_name) |>
  dplyr::summarize(
    mean_r2 = mean(mean_pred_err)
  ) |>
  tidyr::pivot_wider(
    names_from = ".method_name",
    values_from = "mean_r2"
  )
print(metric_df)

# full table of prediction performances
tab <- pred_results_r2_df |> 
  dplyr::mutate(
    err = sprintf("%.3f (%.3f)", mean_pred_err, se_pred_err)
  ) |> 
  tidyr::pivot_wider(
    id_cols = .schid, names_from = .method_name, values_from = err
  ) |> 
  dplyr::left_join(data_stats_df, by = ".schid") |> 
  dplyr::select(
    School = .schid,
    n = `# Samples`,
    # `Var(y)`,
    `NeRF+`, RNC, `Network BART`, `RF+`, `Linear Regression`, BART, RF
  ) |> 
  dplyr::arrange(as.numeric(School)) |>
  dplyr::bind_rows(
    metric_df |> 
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~ sprintf("%.3f", .x)))
  )
print(tab)
tab |> 
  vthemes::pretty_kable(format = "latex")

## Global feature importance Plot
var_labels <- c(
  ".network" = expression(paste("Network (", alpha, ", Z)")),
  ".alpha" = expression(paste("Network (", alpha, ")")),
  ".embed" = "Network (Z)",
  "PNW1" = "Beginning Friendliness",
  "GRC" = "Grade",
  "GENC" = "Gender",
  "ETHA" = "Ethnicity - Asian",
  "ETHB" = "Ethnicity - Black",
  "ETHH" = "Ethnicity - Hispanic",
  "ETHW" = "Ethnicity - White",
  "LIVEWB" = "Live w/ both parents",
  "RETURN" = "Returning student",
  "TREAT" = "Treatment"
)
plt_df <- list(
  `Permutation Importance` = purrr::map(
    eval_results_ls, "Permutation Feature Importances"
  ) |> 
    dplyr::bind_rows(.id = ".schid"),
  `MDI+ Importance` = purrr::map(
    eval_results_ls, "MDI+ Feature Importances"
  ) |> 
    dplyr::bind_rows(.id = ".schid")
) |> 
  dplyr::bind_rows(.id = ".fi_mode") |>
  dplyr::mutate(
    .schid = factor(.dgp_name, levels = paste("School", SCHIDS)),
    var =  factor(var, levels = names(var_labels)),
    .is_network = dplyr::case_when(
      var %in% c(".alpha", ".embed", ".network") ~ "Network",
      TRUE ~ "Non-Network"
    ),
    .pattern = dplyr::case_when(
      var == ".alpha" ~ "Network Cohesion",
      var == ".embed" ~ "Network Embedding",
      TRUE ~ "none"
    ),
    .pattern_fill = dplyr::case_when(
      var == ".network" ~ "Network",
      var %in% c(".alpha", ".embed") ~ "white",
      TRUE ~ "Non-Network"
    )
  ) |> 
  dplyr::filter(
    !is.na(var)
  )
plt_ls <- list()
for (fi_mode in c("Permutation Importance", "MDI+ Importance")) {
  plt_ls[[fi_mode]] <- plt_df |>
    dplyr::filter(
      .fi_mode == !!fi_mode,
      .method_name == "NeRF+"
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = var,
      y = mean_feature_importance,
      group = var,
      color = as.factor(.is_network),
      pattern = as.factor(.pattern),
      fill = as.factor(.pattern_fill),
      pattern_fill = as.factor(.is_network),
      pattern_color = as.factor(.is_network)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = var,
        ymin = mean_feature_importance,
        ymax = mean_feature_importance + se_feature_importance,
        color = as.factor(.is_network),
        group = var
      ),
      inherit.aes = FALSE,
      width = 0.4,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggpattern::geom_bar_pattern(
      stat = "identity",
      position = ggplot2::position_dodge(0.9),
      pattern_angle = 40,
      pattern_density = 0.1,
      pattern_spacing = 0.05,
      # linewidth = 0.1,
      width = 0.8
    ) +
    ggplot2::facet_wrap(~ .schid, scales = "free_y") +
    ggplot2::scale_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_manual(
      values = c(
        "Network Cohesion" = "none",
        "Network Embedding" = "stripe",
        "none" = "none"
      ),
    ) +
    ggplot2::scale_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray", "white" = "white")
    ) +
    ggpattern::scale_pattern_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggplot2::scale_x_discrete(
      labels = var_labels
    ) +
    ggplot2::labs(
      x = "Feature",
      y = fi_mode
    ) +
    vthemes::theme_vmodern(size_preset = "large") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1
      )
    ) +
    ggplot2::guides(
      color = "none", fill = "none", pattern = "none", pattern_fill = "none", pattern_color = "none"
    )
}
plt <- patchwork::wrap_plots(plt_ls, ncol = 1, guides = "collect")
save_figure(
  plt, "school_conflict_globalfi_nerfplus",
  width = 18, height = 22
)

# schools 33 and 21 only
keep_schools <- sprintf("School %s", c("33", "21"))
y_limits <- plt_df |>
  dplyr::filter(
    .schid %in% keep_schools,
    .fi_mode == "Permutation Importance",
    .method_name == "NeRF+"
  ) |>
  dplyr::pull(mean_feature_importance) |>
  range()
plt_ls <- list()
for (schid in c("33", "21")) {
  plt_ls[[schid]] <- plt_df |>
    dplyr::filter(
      .schid == sprintf("School %s", schid),
      .fi_mode == "Permutation Importance",
      .method_name == "NeRF+"
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = var,
      y = mean_feature_importance,
      group = var,
      color = as.factor(.is_network),
      pattern = as.factor(.pattern),
      fill = as.factor(.pattern_fill),
      pattern_fill = as.factor(.is_network),
      pattern_color = as.factor(.is_network)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = var,
        ymin = mean_feature_importance,
        ymax = mean_feature_importance + se_feature_importance,
        color = as.factor(.is_network),
        group = var
      ),
      inherit.aes = FALSE,
      width = 0.4,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggpattern::geom_bar_pattern(
      stat = "identity",
      position = ggplot2::position_dodge(0.9),
      pattern_angle = 40,
      pattern_density = 0.25,
      pattern_spacing = 0.025,
      linewidth = 1,
      width = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_manual(
      values = c(
        "Network Cohesion" = "none",
        "Network Embedding" = "stripe",
        "none" = "none"
      ),
    ) +
    ggplot2::scale_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray", "white" = "white")
    ) +
    ggpattern::scale_pattern_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggplot2::scale_x_discrete(
      labels = var_labels
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits
    ) +
    ggplot2::labs(
      x = "Feature",
      y = "Permutation Importance"
    ) +
    vthemes::theme_vmodern(size_preset = "large") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, vjust = 0.5, hjust = 1
      )
    ) +
    ggplot2::guides(
      color = "none", fill = "none", pattern = "none", pattern_fill = "none", pattern_color = "none"
    )
}
plt <- patchwork::wrap_plots(plt_ls, ncol = 1)
save_figure(
  plt, "school_conflict_globalfi_main",
  width = 6, height = 11
)

## LFI Plot - Schools 21 and 33 only
impute_mode <- "none"
keep_schools <- sprintf("School %s", c("33", "21"))
plt_df_ls <- list()
for (fdir in file.path(impute_mode_dir, keep_schools)) {
  fit_results <- readRDS(file.path(fdir, "fit_results.rds"))
  schid <- stringr::str_extract(basename(fdir), "[0-9]+")
  print(schid)
  
  dgp <- create_dgp(
    .dgp_fun = load_school_conflict_data, 
    .name = schid,
    keep_schools = schid,
    include_w1 = TRUE,
    impute_mode = "none",
    train_prop = 1,
    response_type = "PNW2", 
    network_type = "A",
    connected = TRUE
  )
  data_list <- dgp$generate()
  
  sample_ids <- rownames(data_list$A_full)
  
  get_test_ids <- function(A, A_full) {
    n <- nrow(A)
    return(rownames(A_full)[(n + 1):nrow(A_full)])
  }
  
  lfi_results <- fit_results |> 
    dplyr::filter(
      .method_name == "NeRF+"
    ) |> 
    dplyr::mutate(
      test_ids = purrr::map2(A, A_full, ~ get_test_ids(.x, .y)),
      local_importance = purrr::map2(
        .x = local_importance,
        .y = test_ids,
        function(.x, .y) {
          data.frame(id = sample_ids) |> 
            dplyr::left_join(dplyr::bind_cols(id = .y, .x), by = "id")
        }
      )
    ) |> 
    dplyr::pull(local_importance)
  
  network_lfi <- purrr::map(
    lfi_results,
    function(lfi) {
      lfi$.network
    }
  ) |> 
    purrr::reduce(cbind) |> 
    rowSums(na.rm = TRUE)
  network_lfi <- network_lfi / length(lfi_results)
  
  set.seed(1234)
  g <- igraph::graph_from_adjacency_matrix(
    data_list$A_full > 0, mode = "undirected"
  )
  
  plt <- ggraph::ggraph(
    g, "igraph", algorithm = "nicely"
  )
  
  plt_df <- plt$data |> 
    dplyr::mutate(
      response = data_list$y,
      lfi = network_lfi
    )
  if ("GRC" %in% colnames(data_list$x)) {
    plt_df <- plt_df |>
      dplyr::mutate(
        grade = stringr::str_remove(data_list$x$GRC, "\\(.*\\)") |> 
          stringr::str_remove("grade") |> 
          stringr::str_trim()
      )
  } else {
    plt_df <- plt_df |>
      dplyr::mutate(grade = " ")
  }
  plt_df_ls[[schid]] <- plt_df
}
lfi_limits <- purrr::map(plt_df_ls, ~ range(.x$lfi, na.rm = TRUE)) |>
  purrr::reduce(c) |>
  abs() |>
  max()
lfi_limits <- c(-lfi_limits, lfi_limits)
y_limits <- purrr::map(plt_df_ls, ~ range(.x$response, na.rm = TRUE)) |>
  purrr::reduce(c) |>
  range()

plt_ls <- list()
for (fdir in file.path(impute_mode_dir, keep_schools)) {
  fit_results <- readRDS(file.path(fdir, "fit_results.rds"))
  schid <- stringr::str_extract(basename(fdir), "[0-9]+")
  print(schid)
  
  dgp <- create_dgp(
    .dgp_fun = load_school_conflict_data, 
    .name = schid,
    keep_schools = schid,
    include_w1 = TRUE,
    impute_mode = "none",
    train_prop = 1,
    response_type = "PNW2", 
    network_type = "A",
    connected = TRUE
  )
  data_list <- dgp$generate()
  
  sample_ids <- rownames(data_list$A_full)
  
  get_test_ids <- function(A, A_full) {
    n <- nrow(A)
    return(rownames(A_full)[(n + 1):nrow(A_full)])
  }
  
  lfi_results <- fit_results |> 
    dplyr::filter(
      .method_name == "NeRF+"
    ) |> 
    dplyr::mutate(
      test_ids = purrr::map2(A, A_full, ~ get_test_ids(.x, .y)),
      local_importance = purrr::map2(
        .x = local_importance,
        .y = test_ids,
        function(.x, .y) {
          data.frame(id = sample_ids) |> 
            dplyr::left_join(dplyr::bind_cols(id = .y, .x), by = "id")
        }
      )
    ) |> 
    dplyr::pull(local_importance)
  
  network_lfi <- purrr::map(
    lfi_results,
    function(lfi) {
      lfi$.network
    }
  ) |> 
    purrr::reduce(cbind) |> 
    rowSums(na.rm = TRUE)
  network_lfi <- network_lfi / length(lfi_results)
  
  set.seed(1234)
  g <- igraph::graph_from_adjacency_matrix(
    data_list$A_full > 0, mode = "undirected"
  )
  
  plt <- ggraph::ggraph(
    g, "igraph", algorithm = "nicely"
  )
  
  plt_df <- plt$data |> 
    dplyr::mutate(
      response = data_list$y,
      lfi = network_lfi
    )
  if ("GRC" %in% colnames(data_list$x)) {
    plt_df <- plt_df |>
      dplyr::mutate(
        grade = stringr::str_remove(data_list$x$GRC, "\\(.*\\)") |> 
          stringr::str_remove("grade") |> 
          stringr::str_trim()
      )
  } else {
    plt_df <- plt_df |>
      dplyr::mutate(grade = " ")
  } 
  plt1 <- plt +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = y, fill = grade),
      data = plt_df, size = 20, pch = 21, color = "#ffffff00"
    ) +
    ggraph::geom_edge_link(
      edge_alpha = 0.125
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(x = x, y = y, color = lfi),
      data = plt_df, size = 6
    ) +
    ggplot2::scale_color_viridis_c(
      option = "C", begin = 0, end = 0.95,
      limits = lfi_limits, breaks = c(-0.1, 0, 0.1)
    ) +
    ggplot2::scale_fill_manual(
      values = c("6th" = "#e8f8e5", "5th" = "#ececec", "7th" = "#dbeef4", "8th" = "#fcebfd")
    ) +
    ggplot2::labs(color = "Local Network\nImportance", fill = "Grade") +
    ggplot2::theme_void() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(size = 6))
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    )
  
  plt2 <- plt +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = y, fill = grade),
      data = plt_df, size = 20, pch = 21, color = "#ffffff00"
    ) +
    ggraph::geom_edge_link(
      edge_alpha = 0.125
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(x = x, y = y, color = response),
      data = plt_df, size = 6
    ) +
    ggplot2::scale_color_viridis_c(
      option = "C", begin = 0, end = 0.95, 
      limits = y_limits
    ) +
    ggplot2::scale_fill_manual(
      values = c("6th" = "#e8f8e5", "5th" = "#ececec", "7th" = "#dbeef4", "8th" = "#fcebfd")
    ) +
    ggplot2::labs(color = "Observed y", fill = "Grade") +
    ggplot2::theme_void() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(size = 6))
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      legend.position = "bottom"
    )
  
  plt_ls[[schid]] <- patchwork::wrap_plots(plt2, plt1, nrow = 1, guides = "collect") & 
    ggplot2::theme(legend.position = 'bottom')
}
plt <- patchwork::wrap_plots(plt_ls, nrow = 2, guides = "collect")
save_figure(
  plt, "school_conflict_lfi_main", width = 16, height = 15
)

###########################################################################
######################### Philly Crime Case Study #########################
###########################################################################
## prediction results
subsamples <- c(0.001, 0.005, 0.01, 0.05, 0.1)
pred_results_df <- purrr::map(
  subsamples,
  function(subsample) {
    fit_results <- readRDS(
      file.path(
        RESULTS_DIR,
        "Philly Crime (Predictions)",
        sprintf("Philly Crime (random, with weather, %s)", subsample),
        "fit_results.rds"
      )
    )
    tract_groups <- fit_results |>
      dplyr::filter(.method_name == "Linear Regression") |>
      dplyr::mutate(
        tract_groups = purrr::map(
          verbose_data_out,
          ~ ifelse(.x$nodeids_test %in% unique(.x$nodeids), "Training Tracts", "Test Tracts")
        )
      ) |>
      dplyr::select(.rep, tract_groups)
    fit_results <- dplyr::left_join(fit_results, tract_groups, by = ".rep")
    eval_results <- summarize_pred_err(
      fit_results, 
      truth_col = "y_test", 
      estimate_col = "predictions",
      group_cols = "tract_groups",
      custom_summary_funs = list(
        "se_pred_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
      )
    ) |>
      dplyr::mutate(
        .subsample = !!subsample
      )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    .method_name = factor(.method_name, levels = rev(METHOD_LEVELS)),
    .dgp_name = forcats::fct_inorder(.dgp_name),
    tract_groups = factor(tract_groups, levels = c("Training Tracts", "Test Tracts"))
  )

metric_val <- "rsq"
plt <- pred_results_df |>
  dplyr::filter(
    .metric == !!metric_val,
    !is.na(.method_name)
  ) |>
  dplyr::mutate(
    tract_groups = factor(tract_groups, levels = c("Training Tracts", "Test Tracts"))
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = .subsample, 
    y = mean_pred_err,
    color = .method_name,
    linetype = .method_name
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      x = .subsample,
      ymin = mean_pred_err - se_pred_err,
      ymax = mean_pred_err + se_pred_err,
      fill = .method_name
    ),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::facet_wrap(~ tract_groups, nrow = 1) +
  ggplot2::scale_color_manual(
    values = rev(COLORS), labels = KEEP_METHODS,
    guide = ggplot2::guide_legend(reverse = TRUE)
  ) +
  ggplot2::scale_fill_manual(
    values = rev(COLORS), labels = KEEP_METHODS,
    guide = ggplot2::guide_legend(reverse = TRUE)
  ) +
  ggplot2::scale_linetype_manual(
    values = rev(LINETYPES), labels = KEEP_METHODS,
    guide = ggplot2::guide_legend(reverse = TRUE)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(subsamples), by = 0.05),
    labels = seq(0, max(subsamples), by = 0.05)
  ) +
  ggplot2::labs(
    x = "Training Proportion",
    y = "R-squared",
    color = "Method", fill = "Method", linetype = "Method"
  ) +
  vthemes::theme_vmodern(size_preset = "large")
save_figure(
  plt, filename = "philly_crime_predictions", width = 10, height = 3.5
)

## global feature importance results
subsample <- 0.01
eval_results <- readRDS(
  file.path(
    RESULTS_DIR, 
    "Philly Crime (Global Importance)", 
    sprintf("Philly Crime (random, with weather, %s)", subsample),
    "eval_results.rds"
  )
)

var_labels <- c(
  ".network" = expression(paste("Network (", alpha, ", Z)")),
  ".alpha" = expression(paste("Network (", alpha, ")")),
  ".embed" = "Network (Z)",
  "time" = "Time",
  "MeanAvgTemperature" = "Temperature",
  "TotalPrecipitation" = "Total Precipitation",
  "NumberofDaysPrecipitation" = "Precipitation Days",
  "TotalSnowfall" = "Total Snowfall"
)
for (fi_mode in c("permutation", "mdiplus")) {
  fi_name <- ifelse(fi_mode == "permutation", "Permutation", "MDI+")
  plt <- eval_results[[sprintf("%s Feature Importances", fi_name)]] |>
    dplyr::filter(
      .method_name == "NeRF+"
    ) |>
    dplyr::mutate(
      var = factor(
        var,
        levels = rev(names(var_labels))
      ),
      .is_network = dplyr::case_when(
        var %in% c(".alpha", ".embed", ".network") ~ "Network",
        TRUE ~ "Non-Network"
      ),
      .pattern = dplyr::case_when(
        var == ".alpha" ~ "Network Cohesion",
        var == ".embed" ~ "Network Embedding",
        TRUE ~ "none"
      ),
      .pattern_fill = dplyr::case_when(
        var == ".network" ~ "Network",
        var %in% c(".alpha", ".embed") ~ "white",
        TRUE ~ "Non-Network"
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = var,
      y = mean_feature_importance,
      group = var,
      color = as.factor(.is_network),
      pattern = as.factor(.pattern),
      fill = as.factor(.pattern_fill),
      pattern_fill = as.factor(.is_network),
      pattern_color = as.factor(.is_network)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = var,
        ymin = mean_feature_importance,
        ymax = mean_feature_importance + se_feature_importance,
        color = as.factor(.is_network),
        group = var
      ),
      inherit.aes = FALSE,
      width = 0.4,
      position = ggplot2::position_dodge(0.9)
    ) +
    ggpattern::geom_bar_pattern(
      stat = "identity",
      position = ggplot2::position_dodge(0.9),
      pattern_angle = 40,
      pattern_density = 0.25,
      pattern_spacing = 0.025,
      linewidth = 1,
      width = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_manual(
      values = c(
        "Network Cohesion" = "none",
        "Network Embedding" = "stripe",
        "none" = "none"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray", "white" = "white")
    ) +
    ggpattern::scale_pattern_fill_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggpattern::scale_pattern_color_manual(
      values = c("Network" = "#57B2C8", "Non-Network" = "gray")
    ) +
    ggplot2::scale_x_discrete(
      labels = var_labels
    ) +
    ggplot2::labs(
      x = "Feature",
      y = sprintf("%s Importance", fi_name),
      color = "", fill = "", pattern = "", pattern_color = ""
    ) +
    ggplot2::coord_flip() +
    vthemes::theme_vmodern(size_preset = "large") +
    ggplot2::guides(
      color = "none", fill = "none", pattern = "none", pattern_fill = "none", pattern_color = "none"
    )
  save_figure(
    plt, sprintf("philly_crime_feature_importance_%s", fi_mode),
    width = 8, height = 3.5
  )
}

## local feature importance results
subsample <- 0.01
rep <- 1
fit_results <- readRDS(
  file.path(
    RESULTS_DIR, 
    "Philly Crime (Local Importance)", 
    sprintf("Philly Crime (random, with weather, %s)", subsample),
    "fit_results.rds"
  )
) |>
  dplyr::filter(
    .rep == !!rep
  )

y <- fit_results$y[[1]]
y_test <- fit_results$y_test[[1]]
A <- fit_results$A[[1]]
A_full <- fit_results$A_full[[1]]
lfi_results <- fit_results$local_importance[[1]]
nodeids_test <- fit_results$nodeids_test[[1]]

response_df <- tibble::tibble(
  y = y_test,
  tract_id = nodeids_test
) |>
  dplyr::group_by(tract_id) |>
  dplyr::summarise(
    y = mean(y)
  ) |> 
  dplyr::arrange(tract_id)

lfi_df <- lfi_results |>
  dplyr::mutate(
    tract_id = nodeids_test
  ) |>
  dplyr::group_by(tract_id) |>
  dplyr::summarise(
    dplyr::across(tidyselect::everything(), ~ mean(.x))
  ) |> 
  dplyr::arrange(tract_id)

set.seed(123456)
g <- igraph::graph_from_adjacency_matrix(
  A_full > 0, mode = "undirected"
)
plt <- ggraph::ggraph(
  g, "igraph", algorithm = "nicely"
)
plt_df <- plt$data |> 
  dplyr::mutate(
    response = response_df$y,
    lfi = lfi_df$.alpha
  )
color_limits <- c(
  floor(min(c(plt_df$response, plt_df$lfi))),
  ceiling(max(c(plt_df$response, plt_df$lfi)))
)

plt_lfi <- plt +
  ggraph::geom_edge_link(
    edge_alpha = 0.125
  ) +
  ggraph::geom_node_point(
    ggplot2::aes(x = x, y = y, color = lfi),
    data = plt_df, size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    option = "C", limits = color_limits, begin = 0, end = 0.95
  ) +
  ggplot2::labs(color = "") +
  ggplot2::theme_void()
plt_observed <- plt +
  ggraph::geom_edge_link(
    edge_alpha = 0.125
  ) +
  ggraph::geom_node_point(
    ggplot2::aes(x = x, y = y, color = response),
    data = plt_df, size = 3.5
  ) +
  ggplot2::scale_color_viridis_c(
    option = "C", limits = color_limits, begin = 0, end = 0.95
  ) +
  ggplot2::labs(color = "") +
  ggplot2::theme_void()

plt <- patchwork::wrap_plots(plt_observed, plt_lfi, ncol = 1, guides = "collect")
save_figure(
  plt, "philly_crime_local_importance", width = 8, height = 8
)
