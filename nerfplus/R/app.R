#' Run Shiny NeRF+ Interpret application
#'
#' @param data_list A list containing the data used for interpretation. Should
#'   have the same format as the `data_list` output from [interpret_nerfplus()].
#' @param object The fitted NeRF+ model. Should have the same format as the
#'   `object` output from [interpret_nerfplus()].
#' @param fi_results A list containing the feature importance results. Should
#'   have the same format as the `fi_results` output from [interpret_nerfplus()].
#' @param loo_results A list containing the sample influence results. Should
#'   have the same format as the `loo_results` output from [interpret_nerfplus()].
#' @param max_request_size Maximum request size for file uploads. Defaults to
#'   100 MB.
#' @param ... Additional arguments to pass to [shiny::shinyApp()]
#'
#' @returns A shiny application
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # launch app
#'   run_app()
#' }
#'
#' # or run with arguments
#' data(example_data)
#' nerfplus_out <- nerfplus(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1,
#'   lambda_embed = 0.1,
#'   lambda_raw = 2,
#'   lambda_stump = 3,
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#' out <- interpret_nerfplus(
#'   nerfplus_out, x = example_data$x, y = example_data$y, A = example_data$A,
#'   xtest = example_data$xtest, ytest = example_data$ytest,
#'   A_full = example_data$A_full
#' )
#'
#' if (interactive()) {
#'   run_app(
#'     data_list = out$data_list,
#'     object = out$object,
#'     fi_results = out$fi_results,
#'     loo_results = out$loo_results
#'   )
#' }
#' }
#'
#' @export
run_app <- function(data_list = NULL,
                    object = NULL,
                    fi_results = NULL,
                    loo_results = NULL,
                    max_request_size = 100 * 1024^2,
                    ...) {
  app_dependencies <- c(
    "DT", "fresh", "ggplot2", "ggraph", "htmltools", "igraph",
    "kableExtra", "knitr", "patchwork", "plotly",
    "shiny", "shinycssloaders", "shinydashboard", "shinydashboardPlus",
    "shinyjs", "shinyWidgets", "tippy"
  )
  rlang::check_installed(app_dependencies)

  options(shiny.maxRequestSize = max_request_size)

  title <- "NeRF+ Interpreter"

  # Helper Variables -----------------------------------------------------------
  picker_options <- list(
    `live-search` = TRUE,
    size = 5,
    `selected-text-format` = "count > 3",
    title = "Nothing selected",
    multipleSeparator = ", ",
    `actions-box` = TRUE
  )

  # Tooltips -------------------------------------------------------------------
  data_tooltip <- "Upload data. Should be a list with the following named components: x, xtest, y, ytest, A, A_full. Optional components include x_embed, xtest_embed, nodeids, and nodeids_test. See ? nerfplus::interpret_nerfplus for details."
  imp_tooltip <- "Upload (global and/or local) feature importance results, i.e., the `fi_results.rds` file from nerfplus::interpret_nerfplus(save = TRUE)."
  infl_tooltip <- "Upload LOO/sample influences results, i.e., the `loo_results.rds` file from nerfplus::interpret_nerfplus(save = TRUE)."
  fit_tooltip <- "Upload NeRF+ model."
  fi_display_mode_tooltip <- "Choose whether to display the top features or a user-specificed set of features."

  # Captions -------------------------------------------------------------------
  infl_plot_caption <- "We visualize the influence of each training sample according to the (i) change in alpha estimates, (ii) change in beta estimates, (iii) change in test error, and (iv) change in test predictions when leaving out each sample from the training process. Changes are measured using absolute differences."

  # UI -------------------------------------------------------------------------
  ui <- shinydashboard::dashboardPage(
    title = title,

    # Header -------------------------------------------------------------------
    prettyDashboardHeader(
      title = title,
      title_position = "center",
      title_style = list(`font-variant` = "small-caps", `font-size` = "24px")
    ),

    # Sidebar ------------------------------------------------------------------
    shinydashboard::dashboardSidebar(
      # Data Input Section ---------------------------------------------------
      input_header_style(shiny::icon("database"), " Data Input"),
      radio_group_buttons(
        "input_type", label = NULL,
        choices = c("NeRF+ Results Upload", "NeRF+ Fit Upload"),
        selected = "NeRF+ Results Upload", size = "normal"
      ),

      # File Upload ----------------------------------------------------------
      fileInputUI(
        "data_list",
        label = "Data Upload",
        accept = ".rds",
        tooltip = data_tooltip
      ),
      shiny::conditionalPanel(
        condition = "input.input_type == 'NeRF+ Fit Upload'",
        fileInputUI(
          "fit",
          label = "NeRF+ Model Upload",
          accept = ".rds",
          tooltip = fit_tooltip
        ),
        checkbox_group(
          "methods",
          label = "Interpretability Methods to Run",
          choices = c(
            "Global Permutation Feature Importances" = "permute",
            "Global MDI+ Feature Importances" = "mdi+",
            "Local Feature Importances" = "local",
            "Sample Influences" = "loo"
          ),
          selected = c("permute", "mdi+", "local", "loo")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.input_type == 'NeRF+ Results Upload'",
        fileInputUI(
          "imp",
          label = "Feature Importances Upload",
          accept = ".rds",
          tooltip = imp_tooltip
        ),
        fileInputUI(
          "infl",
          label = "Sample Influences Upload",
          accept = ".rds",
          tooltip = infl_tooltip
        )
      ),

      # Reset Button -----------------------------------------------------------
      hr_short() |>
        set_margins(top = "2em", bottom = "1.75em"),
      reset_button(),
      run_button()
    ),

    # Body ---------------------------------------------------------------------
    shinydashboard::dashboardBody(
      shinyWidgets::useSweetAlert(),
      shinyjs::useShinyjs(),
      use_pretty_style(),

      # Instructions -----------------------------------------------------------
      prettyBox(
        shiny::htmlOutput("instructions"),
        title = "Instructions",
        color = "slategray"
      ),

      # Tabset -----------------------------------------------------------------
      shiny::tabsetPanel(
        id = "tabset",
        type = "pills",

        # Global Feature Importance --------------------------------------------
        shiny::tabPanel(
          title = "Global Feature Importances",
          shiny::br(),

          # Global Feature Importance Options ----------------------------------
          prettyBox(
            shiny::column(
              3,
              radio_buttons(
                "fi_display_mode",
                label = "Show" |>
                  add_tooltip_icon(id = "show"),
                choices = c(
                  "Top Features" = "top",
                  "User-specified Feature Set" = "manual"
                )
              ),
              add_tooltip("fi_display_mode", fi_display_mode_tooltip),
            ),
            shiny::column(
              3,
              shiny::conditionalPanel(
                condition = "input.fi_display_mode == 'manual'",
                picker_input(
                  "show_features",
                  label = "Choose features to display",
                  choices = NULL,
                  multiple = TRUE,
                  options = picker_options
                )
              ),
              shiny::conditionalPanel(
                condition = "input.fi_display_mode == 'top'",
                shiny::numericInput(
                  "max_features",
                  label = "Number of top features to display",
                  value = 10
                )
              ),
            ),
            # shiny::column(
            #   4,
            #   shiny::numericInput(
            #     "B", label = "Number of permutations", value = 10
            #   ),
            # ),
            # color = "slategray",
            id = "globalfi_options_box"
          ),
          shiny::tags$head(
            shiny::tags$style(
              '#globalfi_options_box .box-header{ display: none}'
            )
          ),

          # Permutation Feature Importance -------------------------------------
          fiUI(
            "globalfi_permutation", name = "Permutation"
          ),
          # MDI+ Feature Importance --------------------------------------------
          fiUI(
            "globalfi_mdiplus", name = "MDI+"
          )
        ),

        # Local Feature Importance --------------------------------------------
        shiny::tabPanel(
          title = "Local Feature Importances",
          shiny::br(),

          # Local Feature Importance Options ----------------------------------
          prettyBox(
            shiny::column(
              3,
              picker_input(
                "show_lfi_features_local",
                label = "Show local importance for feature(s):",
                choices = "Network (alpha, Z)",
                selected = "Network (alpha, Z)",
                multiple = TRUE,
                options = picker_options
              )
            ),
            shiny::column(
              3,
              picker_input(
                "show_features_local",
                label = "Show feature(s):",
                choices = NULL,
                selected = NULL,
                multiple = TRUE,
                options = picker_options
              )
            ),
            shiny::column(
              3,
              material_switch(
                "shared_guides_local",
                label = "Share guides across plots",
                value = TRUE
              )
            ),
            shiny::column(
              3,
              checkbox_group(
                "show_splits_local",
                label = "Show for:",
                choices = c(
                  "Training Samples" = "train",
                  "Test Samples" = "test"
                ),
                selected = c("train", "test")
              )
            ),
            # color = "slategray",
            id = "localfi_options_box"
          ),
          shiny::tags$head(
            shiny::tags$style(
              '#localfi_options_box .box-header{ display: none}'
            )
          ),

          # Local Feature Importance -------------------------------------
          prettyBox(
            selectVizUI("localfi", choices = c("ggplot", "table")),
            plotUI("localfi"),
            dropdownMenu = options_dropdown(
              shiny::column(
                6,
                shiny::numericInput(
                  "localfi-edge_alpha", "Edge Transparency", 0.125
                ),
                shiny::numericInput(
                  "localfi-point_size", "Node Size", 3.5
                ),
                shiny::numericInput(
                  "localfi-title_size", "Title Text Size", 16
                ),
                shiny::numericInput(
                  "localfi-display_height", "Plot Height (px)", 500
                )
              ),
              tableOptionsUI(
                "localfi", digits = 3, sigfig = TRUE, digits_label = "Table Digits",
                total_width = 6
              ),
              width = "450px"
            ),
            title = "Local Feature Importance Plot"
          )
        ),

        # Sample Influences ----------------------------------------------------
        shiny::tabPanel(
          title = "Sample Influences",
          shiny::br(),
          prettyBox(
            selectVizUI("infl", choices = c("ggplot")),
            plotUI("infl"),
            dropdownMenu = options_dropdown(
              shiny::numericInput(
                "infl-edge_alpha", "Edge Transparency", 0.125
              ),
              shiny::numericInput(
                "infl-point_size", "Node Size", 3.5
              ),
              shiny::numericInput(
                "infl-title_size", "Title Text Size", 16
              ),
              shiny::numericInput(
                "infl-display_height", "Plot Height (px)", 600
              ),
              width = "300px"
            ),
            title = "Sample Influence Plot",
            footer = infl_plot_caption
          )
        )
      )
    )
  )

  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {
    # to avoid no visible binding note
    var <- NULL
    importance <- NULL
    Importance <- NULL
    .network <- NULL
    .alpha <- NULL
    .embed <- NULL

    # Update inputs ------------------------------------------------------------
    ## Update data input -------------------------------------------------------
    dataListInput <- fileInputServer("data_list", data_list)
    fit <- fileInputServer("fit", object)
    impResultsInput <- fileInputServer("imp", fi_results)
    looResultsInput <- fileInputServer("infl", loo_results)
    fitResults <- shiny::eventReactive(input$run_methods, {
      shiny::req(fit(), dataListInput())
      data_ls <- dataListInput()
      results_ls <- interpret_nerfplus(
        fit(),
        x = data_ls$x, x_embed = data_ls$x_embed,
        y = data_ls$y, A = data_ls$A, nodeids = data_ls$nodeids,
        xtest = data_ls$xtest, xtest_embed = data_ls$xtest_embed,
        ytest = data_ls$ytest, A_full = data_ls$A_full,
        nodeids_test = data_ls$nodeids_test,
        methods = input$methods, B = 25
      )
      return(results_ls)
    })
    dataList <- shiny::reactive({
      shiny::req(dataListInput())
      if (input$input_type == "NeRF+ Results Upload") {
        data_ls <- dataListInput()
        if (is.null(data_ls$nodeids)) {
          data_ls$nodeids <- 1:nrow(data_ls$A)
        }
        if (is.null(data_ls$nodeids_test)) {
          data_ls$nodeids_test <- (nrow(data_ls$A) + 1):nrow(data_ls$A_full)
        }
      } else {
        data_ls <- fitResults()$data_list
      }
      return(data_ls)
    })
    impResults <- shiny::reactive({
      if (input$input_type == "NeRF+ Results Upload") {
        shiny::req(impResultsInput())
        return(impResultsInput())
      } else {
        return(fitResults()$fi_results)
      }
    })
    looResults <- shiny::reactive({
      if (input$input_type == "NeRF+ Results Upload") {
        shiny::req(looResultsInput())
        return(looResultsInput())
      } else {
        return(fitResults()$loo_results)
      }
    })

    ## Update variable choices if data input changes ---------------------------
    shiny::observe({
      xvars <- sort(unique(c(impResults()$permute$var, impResults()$`mdi+`$var)))
      if (length(xvars) == 0) return(NULL)
      xvars <- dplyr::case_when(
        xvars == ".alpha" ~ "Network (alpha)",
        xvars == ".embed" ~ "Network (Z)",
        xvars == ".network" ~ "Network (alpha, Z)",
        TRUE ~ xvars
      )
      shinyWidgets::updatePickerInput(session, "show_features", choices = xvars)
      if (length(xvars) > 0) {
        shiny::updateNumericInput(
          session, "max_features",
          value = min(100, length(xvars)),
          min = 1,
          max = length(xvars)
        )
      }
    })
    shiny::observe({
      xvars <- sort(colnames(localfi_table()))
      shinyWidgets::updatePickerInput(
        session, "show_lfi_features_local", choices = xvars,
        selected = "Network (alpha, Z)"
      )
    })
    shiny::observe({
      xvars <- sort(colnames(as.data.frame(dataList()$x)))
      shinyWidgets::updatePickerInput(session, "show_features_local", choices = xvars)
    })

    ## Shared reactive variables -----------------------------------------------
    fi_display_mode <- shiny::reactive({input$fi_display_mode})
    show_features <- shiny::reactive({input$show_features})
    max_features <- shiny::reactive({input$max_features})
    lfi_local_vars <- shiny::reactive({input$show_lfi_features_local})
    local_vars <- shiny::reactive({input$show_features_local})
    shared_guides <- shiny::reactive({input$shared_guides_local})
    show_splits <- shiny::reactive({input$show_splits_local})

    ## Instructions ------------------------------------------------------------
    output$instructions <- shiny::renderText({
      '1. On the left side panel, choose whether you would like to upload pre-computed NeRF+ results ("NeRF+ Results Upload") or the NeRF+ model fit ("NeRF+ Fit Upload").<br>
      2. Upload the data and results/fit files using the file upload buttons.<br>
      3. If uploading the NeRF+ model fit, select the interpretability methods you would like to run and click "Run".<br>
      4. Explore the results using the "Global Feature Importances", "Local Feature Importances", and "Sample Influences" tabs below.'
    })

    ## Run Method --------------------------------------------------------------
    globalfi_permutation_table <- shiny::reactive({
      shiny::req(impResults()$permute)
      impResults()$permute |>
        dplyr::select(var, importance) |>
        dplyr::mutate(
          var = dplyr::case_when(
            var == ".alpha" ~ "Network (alpha)",
            var == ".embed" ~ "Network (Z)",
            var == ".network" ~ "Network (alpha, Z)",
            TRUE ~ var
          )
        ) |>
        dplyr::rename(
          Importance = importance,
          Variable = var
        ) |>
        dplyr::arrange(-Importance)
    })
    globalfi_mdiplus_table <- shiny::reactive({
      shiny::req(impResults()$`mdi+`)
      impResults()$`mdi+` |>
        dplyr::select(var, importance) |>
        dplyr::mutate(
          var = dplyr::case_when(
            var == ".alpha" ~ "Network (alpha)",
            var == ".embed" ~ "Network (Z)",
            var == ".network" ~ "Network (alpha, Z)",
            TRUE ~ var
          )
        ) |>
        dplyr::rename(
          Importance = importance,
          Variable = var
        ) |>
        dplyr::arrange(-Importance)
    })
    localfi_table <- shiny::reactive({
      shiny::req(impResults()$local)
      impResults()$local |>
        dplyr::relocate(
          .network, .alpha, .embed, .before = 1
        ) |>
        dplyr::rename(
          `Network (alpha, Z)` = .network,
          `Network (alpha)` = .alpha,
          `Network (Z)` = .embed
        )
    })

    ## Global Feature Importance Plots -----------------------------------------
    globalfi_permutation_plot <- shiny::reactive({
      plotGlobalFeatureImportance(
        fiTable = globalfi_permutation_table,
        fi_display_mode = fi_display_mode,
        show_features = show_features,
        max_features = max_features
      )
    })
    tableServer(
      id = "globalfi_permutation",
      table_fun = globalfi_permutation_table,
      table_options = TRUE,
      mode = "DT"
    )
    plotServer(
      id = "globalfi_permutation",
      plot_fun = globalfi_permutation_plot,
      plot_options = TRUE,
      modes = c("ggplot", "plotly", "table")
    )

    globalfi_mdiplus_plot <- shiny::reactive({
      plotGlobalFeatureImportance(
        fiTable = globalfi_mdiplus_table,
        fi_display_mode = fi_display_mode,
        show_features = show_features,
        max_features = max_features
      )
    })
    tableServer(
      id = "globalfi_mdiplus",
      table_fun = globalfi_mdiplus_table,
      table_options = TRUE,
      mode = "DT"
    )
    plotServer(
      id = "globalfi_mdiplus",
      plot_fun = globalfi_mdiplus_plot,
      plot_options = TRUE,
      modes = c("ggplot", "plotly", "table")
    )

    ## Local Feature Importance Plots ------------------------------------------
    localfi_plot <- shiny::reactive({
      plotLocalFeatureImportance(
        fiTable = localfi_table,
        dataList = dataList,
        lfi_local_vars = lfi_local_vars,
        local_vars = local_vars,
        shared_guide = shared_guides,
        show_splits = show_splits,
        edge_alpha = input$`localfi-edge_alpha`,
        point_size = input$`localfi-point_size`,
        title_size = input$`localfi-title_size`
      )
    })
    tableServer(
      id = "localfi",
      table_fun = localfi_table,
      table_options = TRUE,
      mode = "DT"
    )
    plotServer(
      id = "localfi",
      plot_fun = localfi_plot,
      plot_options = FALSE,
      modes = c("ggplot", "table")
    )

    ## Sample Influence Plots --------------------------------------------------
    infl_plot <- shiny::reactive({
      plotSampleInfluences(
        looResults = looResults,
        dataList = dataList,
        edge_alpha = input$`infl-edge_alpha`,
        point_size = input$`infl-point_size`,
        title_size = input$`infl-title_size`
      )
    })
    plotServer(
      id = "infl",
      plot_fun = infl_plot,
      plot_options = FALSE,
      modes = c("ggplot")
    )

    # Reset all inputs if want to start over -----------------------------------
    reset_inputs(input, session)
    # Hide run button when uploading results -----------------------------------
    shiny::observe({
      shinyjs::toggle(
        "run_methods", condition = input$input_type == "NeRF+ Fit Upload"
      )
    })
  }

  shiny::shinyApp(ui, server, ...)
}


#' Interpret NeRF+ Model
#'
#' @description Wrapper function around `get_feature_importances()` and
#'   `get_loo()`. Helpful to format results correctly for input into NeRF+
#'   Interpreter Shiny application.
#'
#' @inheritParams get_loo
#' @param methods A character vector of methods to run. Can include any of
#'   "permute", "mdi+", "local", and "loo".
#' @param B Number of permutations to use for permutation feature importance.
#' @param metric A function to compute the metric used for global feature
#'   importances. Defaults to R-squared for regression and AUROC for
#'   classification.
#' @param save Whether to save the results to disk.
#' @param save_dir Directory to save results to if `save = TRUE`.
#'
#' @returns A list with the following components:
#' \item{object}{The fitted NeRF+ model.}
#' \item{data_list}{A list containing the data used for interpretation.}
#' \item{fi_results}{A list containing the feature importance results.}
#' \item{loo_results}{A list containing the sample influence results.}
#'
#' @examples
#' \donttest{
#' data(example_data)
#' nerfplus_out <- nerfplus(
#'   x = example_data$x, y = example_data$y, A = example_data$A,
#'   lambda_netcoh = 1,
#'   lambda_embed = 0.1,
#'   lambda_raw = 2,
#'   lambda_stump = 3,
#'   family = "linear", embedding = "laplacian", sample_split = "none"
#' )
#' out <- interpret_nerfplus(
#'   nerfplus_out, x = example_data$x, y = example_data$y, A = example_data$A,
#'   xtest = example_data$xtest, ytest = example_data$ytest,
#'   A_full = example_data$A_full
#' )
#' }
#'
#' @export
interpret_nerfplus <- function(object, x, x_embed = NULL, y, A, nodeids = NULL,
                               xtest, xtest_embed = NULL, ytest, A_full, nodeids_test = NULL,
                               methods = c("permute", "mdi+", "local", "loo"),
                               B = 10, metric = NULL,
                               save = FALSE, save_dir = "results_nerfplus") {
  methods <- match.arg(methods, several.ok = TRUE)
  loo_results <- NULL
  if ("loo" %in% methods) {
    loo_results <- get_loo(
      object, x = x, x_embed = x_embed, y = y, A = A,
      xtest = xtest, xtest_embed = xtest_embed, ytest = ytest, A_full = A_full,
      nodeids = nodeids, nodeids_test = nodeids_test
    )
    if (save) {
      saveRDS(loo_results, file.path(save_dir, "loo_results.rds"))
    }
  }

  if (is.null(nodeids)) {
    nodeids <- 1:nrow(A)
  }
  if (is.null(nodeids_test)) {
    nodeids_test <- (nrow(A) + 1):nrow(A_full)
  }

  fi_results <- list()
  for (method in setdiff(methods, "loo")) {
    if (method == "local") {
      fi_results[[method]] <- get_feature_importances(
        object, x = rbind(x, xtest), x_embed = rbind(x_embed, xtest_embed),
        y = c(y, ytest), A_full = A_full, nodeids = c(nodeids, nodeids_test),
        method = method
      )
    } else {
      fi_results[[method]] <- get_feature_importances(
        object, x = xtest, x_embed = xtest_embed, y = ytest, A_full = A_full,
        method = method, B = B, metric = metric
      )
    }
  }
  data_list <- list(
    x = x,
    xtest = xtest,
    y = y,
    ytest = ytest,
    A = A,
    A_full = A_full,
    nodeids = nodeids,
    nodeids_test = nodeids_test
  )
  if (save) {
    saveRDS(data_list, file.path(save_dir, "data_list.rds"))
    saveRDS(fi_results, file.path(save_dir, "fi_results.rds"))
    saveRDS(object, file.path(save_dir, "nerfplus_fit.rds"))
  }
  out <- list(
    object = object,
    data_list = data_list,
    fi_results = fi_results,
    loo_results = loo_results
  )
  return(out)
}
