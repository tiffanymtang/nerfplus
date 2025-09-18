#' File input module - UI
#' @rdname fileInputModule
#' @keywords internal
fileInputUI <- function(id,
                        label = "File Upload",
                        accept = c(".csv", ".txt", ".rds"),
                        show_accept = TRUE,
                        tooltip = NULL) {
  ns <- shiny::NS(id)
  indent_size <- "1.25em"

  if (show_accept) {
    label <- sprintf("%s (%s)", label, paste(accept, collapse = ", "))
  }
  if (!is.null(tooltip)) {
    label <- label |> add_tooltip_icon(id = ns("file"))
  }

  shiny::tagList(
    # file input
    shiny::fileInput(
      inputId = ns("file"),
      label = label,
      multiple = FALSE,
      accept = accept
    ),
    add_tooltip(ns("file"), tooltip),
    # file input options
    shiny::conditionalPanel(
      condition = sprintf(
        "output['%1$s'] == true && output['%2$s'] !== '.rds'",
        ns("fileuploaded"), ns("filetype")
      ),
      # options for .txt file
      shiny::conditionalPanel(
        condition = sprintf("output['%1$s'] == '.txt'", ns("filetype")),
        radio_buttons(
          inputId = ns("sep"),
          label = htmltools::tags$i("Separator"),
          choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
          inline = TRUE
        )
      ),
      # options for .csv/.txt file
      shiny::conditionalPanel(
        condition = sprintf(
          "output['%1$s'] == '.csv' | output['%1$s'] == '.txt'", ns("filetype")
        ),
        material_switch(
          inputId = ns("header"),
          label = htmltools::tags$i("Header"),
          value = TRUE,
          status = "primary"
        )
      )
    ) |>
      htmltools::tagAppendAttributes(
        style = css_styler(
          `margin-left` = indent_size,
          `margin-top` = "-0.75em"
        )
      )
  )
}

#' File input module - Server
#' @rdname fileInputModule
#' @keywords internal
fileInputServer <- function(id, default_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$filetype <- shiny::reactive({
      stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")
    })
    shiny::outputOptions(output, "filetype", suspendWhenHidden = FALSE)

    output$fileuploaded <- shiny::reactive({
      length(stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")) > 0
    })
    shiny::outputOptions(output, "fileuploaded", suspendWhenHidden = FALSE)

    shiny::reactive({
      file_type <- stringr::str_extract(input$file$datapath, "(\\.[^.]*)$")
      if (identical(file_type, character(0))) {
        file_type <- "default"
      }
      switch(
        file_type,
        "default" = default_data,
        ".rds" = readRDS(input$file$datapath),
        ".csv" = utils::read.csv(input$file$datapath, header = input$header),
        ".txt" = utils::read.table(input$file$datapath, header = input$header, sep = input$sep)
      )
    })
  })
}


#' Radio group button with icons
#'
#' @description Wrapper around \code{radio_group_buttons} with options to
#'   display icons and float the buttons to the right.
#'
#' @inheritParams shinyWidgets::radioGroupButtons
#' @param id Unique identifier.
#' @param choices Choices must be some subset of "ggplot", "plotly", "table"
#' @param float_right Logical. Whether or not to float the buttons to the right.
#' @param ... Additional arguments to pass to radio_group_buttons
#'
#' @keywords internal
iconRadioGroupUI <- function(id, ns_id, choices,
                             selected = NULL, individual = FALSE,
                             size = "normal", justified = FALSE,
                             float_right = FALSE, ...) {
  ns <- shiny::NS(id)

  if (float_right) {
    style <- css_styler(
      float = "right",
      `z-index` = 99,
      position = "relative",
      `margin-right` = "15px"
    )
  } else {
    style <- css_styler(
      position = "relative"
    )
  }

  out <- radio_group_buttons(
    inputId = ns(ns_id),
    label = NULL,
    choices = get_icons(choices),
    selected = selected,
    individual = individual,
    size = size,
    justified = justified,
    ...
  ) |>
    htmltools::tagAppendAttributes(class = "btn-margin", style = style) |>
    display_inline()

  if (float_right) {
    # hack so that plotly plot tools don't overlap with buttons
    hidden_out <- iconRadioGroupUI(
      id = paste0(id, "_hidden"),
      ns_id = ns_id,
      choices = choices,
      selected = selected,
      individual = individual,
      size = size,
      justified = justified,
      float_right = FALSE,
      ...
    ) |>
      htmltools::tagAppendAttributes(style = css_styler(visibility = "hidden"))
    out <- shiny::tagList(out, hidden_out)
  }

  return(out)
}


#' Select visualization type (table, ggplot, plotly)
#'
#' @inheritParams iconRadioGroupUI
#'
#' @keywords internal
selectVizUI <- function(id,
                        choices = c("ggplot", "plotly", "table"),
                        selected = NULL, individual = FALSE, size = "normal",
                        justified = FALSE, float_right = FALSE, ...) {
  iconRadioGroupUI(
    id = id,
    ns_id = "display_viz",
    choices = choices,
    selected = selected,
    individual = individual,
    size = size,
    justified = justified,
    float_right = float_right,
    ...
  )
}


#' Basic plot module - UI
#' @rdname plotModule
#' @keywords internal
plotUI <- function(id, border = FALSE, spinner = FALSE) {
  ns <- shiny::NS(id)
  out <- shiny::uiOutput(ns("plot")) |>
    add_spinner(spinner) |>
    add_border(border)
  return(out)
}


#' Basic Table Options
#'
#' @param id Unique identifier.
#' @param digits Default number of digits to display.
#' @param digits_label Displayed label for digits input.
#' @param sigfig Logical. If \code{TRUE}, use significant digits.
#' @param sigfig_label Displayed label for significant digits checkbox.
#' @param total_width Total width of html element.
#'
#' @return List of shiny tags.
#'
#' @keywords internal
tableOptionsUI <- function(id,
                           digits = NA,
                           digits_label = "Digits",
                           sigfig = FALSE,
                           sigfig_label = "Use Significant Digits",
                           total_width = NULL) {
  ns <- shiny::NS(id)

  opts <- shiny::tagList(
    shiny::numericInput(
      inputId = ns("display_digits"),
      label = digits_label,
      value = digits,
      min = 0, max = NA, step = 1
    ),
    checkbox(
      inputId = ns("display_sigfigs"),
      label = sigfig_label,
      value = sigfig
    )
  )

  if (!is.null(total_width)) {
    opts <- shiny::tagList(shiny::column(total_width, opts))
  }

  return(opts)
}


#' Basic Plot Options
#'
#' @param id Unique identifier.
#' @param multicol Logical. If \code{TRUE}, use multiple columns.
#' @param heatmap Logical. If \code{TRUE}, include heatmap options.
#' @param strip_options Logical. If \code{TRUE}, include strip options.
#' @param total_width Total width of html element.
#' @param height Height of html element.
#' @param x_axis_text_size Default size of x-axis text.
#' @param y_axis_text_size Default size of y-axis text.
#' @param legend_text_size Default size of legend text.
#' @param strip_text_size Default size of strip text.
#' @param x_axis_title_size Default size of x-axis title.
#' @param y_axis_title_size Default size of y-axis title.
#' @param legend_title_size Default size of legend title.
#' @param title_size Default size of title.
#' @param axis_line_width Default width of axis lines.
#' @param x_text_angle Default angle of x-axis text. If \code{FALSE}, x-axis
#'   text is horizontal. Otherwise, it is angled 45 degrees.
#' @param strip_text_color Default color of strip text.
#' @param bg_color Default background color.
#' @param other_options Additional shiny widgets/tags to add to plot options.
#'
#' @return List of shiny tags.
#'
#' @keywords internal
plotOptionsUI <- function(id, multicol = FALSE,
                          heatmap = FALSE, strip_options = FALSE,
                          total_width = 12, height = 500,
                          x_axis_text_size = 12, y_axis_text_size = 12,
                          legend_text_size = 12, strip_text_size = 14,
                          x_axis_title_size = 14, y_axis_title_size = 14,
                          legend_title_size = 14, title_size = 16,
                          axis_line_width = 1, x_text_angle = FALSE,
                          strip_text_color = "white", bg_color = "grey98",
                          other_options = NULL) {
  ns <- shiny::NS(id)

  if (!heatmap) {
    bg_options <- shiny::tagList(
      material_switch(
        ns("display_grid"), "Show grid lines", value = TRUE
      ),
      shiny::textInput(
        ns("display_bg"), "Background Color", bg_color
      )
    )
  } else {
    bg_options <- NULL
  }

  if (strip_options) {
    strip_text_options <- shiny::tagList(
      shiny::numericInput(
        ns("display_stext_size"), "Strip Text Size", strip_text_size
      )
    )
    strip_format_options <- shiny::tagList(
      shiny::textInput(
        ns("display_stext_color"), "Strip Text Color", "white"
      ),
      shiny::textInput(
        ns("display_sbg"), "Strip Background Color", "#2c3e50"
      )
    )
  } else {
    strip_text_options <- NULL
    strip_format_options <- NULL
  }

  title_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_xtitle_size"), "X-Axis Title Size", x_axis_title_size
    ),
    shiny::numericInput(
      ns("display_ytitle_size"), "Y-Axis Title Size", y_axis_title_size
    ),
    shiny::numericInput(
      ns("display_ltitle_size"), "Legend Title Size", legend_title_size
    ),
    shiny::numericInput(
      ns("display_title_size"), "Title Size", title_size
    )
  )

  text_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_xtext_size"), "X-Axis Text Size", x_axis_text_size
    ),
    shiny::numericInput(
      ns("display_ytext_size"), "Y-Axis Text Size", y_axis_text_size
    ),
    shiny::numericInput(
      ns("display_ltext_size"), "Legend Text Size", legend_text_size
    )
  )

  axis_width_options <- shiny::tagList(
    shiny::numericInput(
      ns("display_awidth"), "Axis Line Width", axis_line_width
    )
  )

  axis_options <- shiny::tagList(
    checkbox_group(
      ns("display_atext"), label = "Show Axis Text",
      choices = c("x", "y"), selected = c("x", "y"), inline = TRUE
    ),
    material_switch(
      ns("display_xangle"), "Angle X-Axis Text", x_text_angle
    ),
    material_switch(
      ns("display_coord_flip"), "Flip Coordinate Axes", value = FALSE
    )
  )

  height_options <- shiny::numericInput(
    ns("display_height"), "Plot Height (px)", height
  )

  if (!multicol) {
    opts <- shiny::tagList(
      text_options,
      strip_text_options,
      title_options,
      strip_format_options,
      axis_width_options,
      axis_options,
      bg_options,
      height_options,
      other_options
    )
  } else {
    opts_col1 <- title_options
    opts_col2 <- shiny::tagList(
      text_options,
      strip_text_options,
      axis_width_options
    )
    opts_col3 <- shiny::tagList(
      axis_options,
      bg_options,
      strip_format_options,
      height_options,
      other_options
    )
    opts <- shiny::tagList(
      shiny::column(total_width / 3, opts_col1),
      shiny::column(total_width / 3, opts_col2),
      shiny::column(total_width / 3, opts_col3)
    )
  }

  return(opts)
}


#' Basic table module - Server
#' @rdname tableModule
#' @keywords internal
tableServer <- function(id, table_fun, table_options = TRUE,
                        mode = c("DT", "kable"), caption = NULL,
                        border = FALSE, spinner = TRUE,
                        error_msg = "", ...) {
  mode <- match.arg(mode)

  shiny::moduleServer(id, function(input, output, session) {

    make_table <- shiny::reactive({
      tab <- table_fun()

      if (!is.null(caption)) {
        if (shiny::is.reactive(caption)) {
          caption <- caption()
        }
      }

      if (!table_options) {
        digits <- NULL
        sigfig <- FALSE
      } else {
        digits <- input$display_digits
        if (isTRUE(is.na(digits))) {
          digits <- NULL
        }
        sigfig <- input$display_sigfigs
        if (is.null(sigfig)) {
          sigfig <- FALSE
        }
      }
      if (sigfig) {
        dig_format <- "g"
      } else {
        dig_format <- "f"
      }

      tab <- tab |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(is.numeric),
            ~ kableExtra::cell_spec(
              formatC(.x, digits = digits, format = dig_format, flag = "#")
            )
          )
        )

      if (mode == "DT") {
        options <- list(
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(tab))
          )
        )
        generate_table <- purrr::partial(
          DT::datatable, escape = FALSE, caption = caption, options = options
        )
      } else if (mode == "kable") {
        generate_table <- function(tab) {
          knitr::kable(
            tab, align = "c", booktabs = TRUE, format = "html", linesep = "",
            caption = caption, escape = FALSE
          ) #|>
            # kableExtra::kable_styling(
            #   bootstrap_options = c("striped", "hover")
            # )
        }
      }
      out <- generate_table(tab)
    })

    if (mode == "DT") {
      output$dt <- DT::renderDT({make_table()})
    } else if (mode == "kable") {
      output$kable <- shiny::renderText({make_table()})
    }
    output$table_error <- shiny::renderText({error_msg})

    output$table <- shiny::renderUI({
      if (mode == "DT") {
        shiny::fluidPage(
          DT::DTOutput(session$ns("dt")) |>
            add_spinner(spinner) |>
            add_border(border),
          vspace()
        )
      } else if (mode == "kable") {
        shiny::fluidPage(
          shiny::htmlOutput(session$ns("kable")) |>
            add_spinner(spinner) |>
            add_border(border)
        )
      } else {
        shiny::htmlOutput(session$ns("table_error"))
      }
    })
  })
}


#' Basic plot module - Server
#' @rdname plotModule
#' @keywords internal
plotServer <- function(id, plot_fun, plot_options = TRUE,
                       modes = c("ggplot", "plotly"),
                       border = FALSE, spinner = TRUE,
                       error_msg = "") {
  shiny::moduleServer(id, function(input, output, session) {

    make_plot <- shiny::reactive({
      plot <- plot_fun()
      if (!plot_options) {
        return(plot)
      }

      if (!("ggplot" %in% class(plot))) {
        return(plot)
      }

      if (input$display_xangle) {
        x_angle <- 45
        x_hjust <- 1
      } else {
        x_angle <- 0
        x_hjust <- 0.5
      }

      theme_obj <- ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = input$display_xtitle_size, face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = input$display_ytitle_size, face = "bold"
        ),
        legend.title = ggplot2::element_text(
          size = input$display_ltitle_size, face = "bold"
        ),
        plot.title = ggplot2::element_text(
          size = input$display_title_size, face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = input$display_xtext_size, angle = x_angle, hjust = x_hjust
        ),
        axis.text.y = ggplot2::element_text(size = input$display_ytext_size),
        legend.text = ggplot2::element_text(size = input$display_ltext_size),
        axis.line = ggplot2::element_line(
          linewidth = input$display_awidth, color = "black"
        )
      )

      if (!is.null(input$display_bg)) {
        theme_obj <- theme_obj +
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = input$display_bg),
            legend.key = ggplot2::element_rect(fill = input$display_bg)
          )
        if (!input$display_grid) {
          theme_obj <- theme_obj +
            ggplot2::theme(panel.grid = ggplot2::element_blank())
        }
      }
      if (!is.null(input$display_sbg)) {
        theme_obj <- theme_obj +
          ggplot2::theme(
            strip.background = ggplot2::element_rect(
              fill = input$display_sbg, color = input$display_sbg
            ),
            strip.text = ggplot2::element_text(
              size = input$display_stext_size,
              color = input$display_stext_color,
              face = "bold"
            )
          )
      }

      if (!("x" %in% input$display_atext)) {
        theme_obj <- theme_obj +
          ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
          )
      }
      if (!("y" %in% input$display_atext)) {
        theme_obj <- theme_obj +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()
          )
      }

      if ("patchwork" %in% class(plot)) {
        plot <- plot & theme_obj
        if (input$display_coord_flip) {
          plot <- plot & ggplot2::coord_flip()
        }
      } else {
        plot <- plot + theme_obj
        if (input$display_coord_flip) {
          plot <- plot + ggplot2::coord_flip()
        }
      }

      return(plot)
    })

    plot_height <- shiny::reactive({
      if (is.null(input$display_height)) {
        return(500)
      } else {
        return(input$display_height)
      }
    })

    if ("plotly" %in% modes) {
      output$plotly <- plotly::renderPlotly({
        plt <- make_plot()
        plotly::ggplotly(plt, height = plot_height())
      })
    }
    if ("ggplot" %in% modes) {
      output$ggplot <- shiny::renderPlot({
        make_plot()
      }, height = function() height = plot_height())
    }
    output$plot_error <- shiny::renderText({error_msg})

    output$plot <- shiny::renderUI({
      if (!is.null(input$display_viz)) {
        mode <- input$display_viz
      } else {
        mode <- modes[1]
      }
      if ((mode == "ggplot") && (mode %in% modes)) {
        out <- shiny::plotOutput(session$ns("ggplot"), height = "auto")
      } else if ((mode == "plotly") && (mode %in% modes)) {
        out <- plotly::plotlyOutput(session$ns("plotly"), height = "100%")
      } else if ((mode == "table") && (mode %in% modes)) {
        out <- shiny::uiOutput(session$ns("table"))
      } else {
        out <- shiny::htmlOutput(session$ns("plot_error"))
      }
      out <- out |>
        add_spinner(spinner) |>
        add_border(border)
      return(out)
    })
  })
}
