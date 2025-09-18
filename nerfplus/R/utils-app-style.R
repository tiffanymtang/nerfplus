#' Pretty dashboard header
#'
#' @description Wrapper around `shinydashboard::dashboardHeader()` that allows
#'  for the title to be positioned to the left or center.
#'
#' @inheritParams shinydashboard::dashboardHeader
#' @param title_position Position of the title, either "left" or "center".
#' @param title_style (Optional) list of CSS styling options for the title.
#' @param ... Arguments passed to `shinydashboard::dashboardHeader()`.
#'
#' @keywords internal
prettyDashboardHeader <- function(title, title_position = c("left", "center"),
                                  title_style = list(), ...) {

  title_position <- match.arg(title_position)
  header <- shinydashboard::dashboardHeader(title = title, ...)

  if (title_position == "left") {
    style_ls <- list(
      `font-weight` = "bold"
    )
  } else if (title_position == "center") {
    style_ls <- list(
      display = "block",
      height = "50px",
      `font-size` = "20px",
      `line-height` = "50px",
      `text-align` = "center",
      `font-family` = "Helvetica Neue, Helvetica, Arial, sans-serif",
      `font-weight` = "bold",
      color = "white"
    )
  }
  if (!is.null(title_style)) {
    for (arg in names(title_style)) {
      style_ls[[arg]] <- title_style[[arg]]
    }
  }
  style <- do.call(css_styler, style_ls)

  if (title_position == "left") {
    header <- header |>
      htmltools::tagAppendAttributes(style = style, .cssSelector = ".logo")
  } else if (title_position == "center") {
    header <- header |>
      htmltools::tagAppendAttributes(
        style = css_styler(`margin-left` = "0px"),
        .cssSelector = ".navbar.navbar-static-top"
      ) |>
      htmltools::tagAppendAttributes(
        style = css_styler(position = "absolute", left = "0px"),
        .cssSelector = ".sidebar-toggle"
      )

    title_tag_idx <- 2
    title_tag <- header$children[[title_tag_idx]] |>
      htmltools::tagAppendAttributes(style = style)
    title_tag$attribs$class <- NULL
    header$children[[title_tag_idx]] <- NULL
    header$children[[title_tag_idx]] <- htmltools::tagAppendChild(
      header$children[[title_tag_idx]], title_tag
    )
  }

  return(header)
}


#' Add css styling
#'
#' @param ... Any number of named arguments with the name being the css
#'   selector and the value being the css style value.
#'
#' @keywords internal
css_styler <- function(...) {
  dots_ls <- rlang::dots_list(...) |>
    purrr::compact()
  if (length(dots_ls) > 0) {
    style_str <- paste(names(dots_ls), dots_ls, sep = ": ", collapse = "; ") |>
      paste0(";")
  } else {
    style_str <- ""
  }
  return(style_str)
}


#' Add a tooltip to an element
#'
#' @param id The id of the element to add the tooltip to.
#' @param tooltip The tooltip to add.
#' @param placement The placement of the tooltip.
#' @param allowHTML Whether to allow HTML in the tooltip.
#' @param use_id_only Whether to use the id only. Default is \code{FALSE} which
#'   will add the namespace 'tooltip-icon' to the id. Set to \code{TRUE} if the
#'   tooltip icon was not added with \code{add_tooltip_icon} (e.g., if
#'   the tooltip icon was added manually).
#' @param ... Arguments passed to \code{tippy_this}.
#'
#' @return The element with the tooltip.
#'
#' @keywords internal
add_tooltip <- function(id, tooltip, placement = "right", allowHTML = TRUE,
                        use_id_only = FALSE, ...) {
  if (!use_id_only) {
    ns <- shiny::NS(id)
    id <- ns("tooltip")
  }
  tippy::tippy_this(
    id, tooltip, placement = placement, allowHTML = allowHTML, ...
  )
}


#' Add a tooltip icon to a label
#'
#' @param ... Arguments passed to span element (e.g., the label to add the
#'   tooltip to).
#' @param id The id of the tooltip.
#' @param icon The icon to use for the tooltip.
#'
#' @return The label with the tooltip icon.
#'
#' @keywords internal
add_tooltip_icon <- function(..., id, icon = shiny::icon("info-circle")) {
  ns <- shiny::NS(id)
  htmltools::span(
    ...,
    htmltools::span(icon, id = ns("tooltip"))
  )
}


#' Short horizontal rule
#'
#' @param margin_left Left margin.
#' @param margin_right Right margin.
#' @param color Color of line.
#'
#' @keywords internal
hr_short <- function(margin_left = "105px", margin_right = margin_left,
                     color = "lightblue") {
  htmltools::hr() |>
    htmltools::tagAppendAttributes(
      style = css_styler(
        `margin-left` = margin_left,
        `margin-right` = margin_right,
        `border-color` = color
      )
    )
}


#' Set margins
#'
#' @param obj Object to set margins.
#' @param top Top margin.
#' @param bottom Bottom margin.
#' @param left Left margin.
#' @param right Right margin.
#'
#' @keywords internal
set_margins <- function(obj,
                        top = NULL, bottom = NULL, left = NULL, right = NULL) {
  obj |>
    htmltools::tagAppendAttributes(
      style = css_styler(
        `margin-top` = top,
        `margin-bottom` = bottom,
        `margin-left` = left,
        `margin-right` = right
      )
    )
}


#' Use pretty css style
#'
#' @keywords internal
use_pretty_style <- function() {

  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = utils::packageName())
  )

  my_theme <- fresh::create_theme(
    fresh::adminlte_color(light_blue = "#35a4bf"),
    fresh::adminlte_sidebar(width = "315px", dark_color = "#2c3b41"),
    fresh::adminlte_global(content_bg = "whitesmoke")
  )

  shiny::tagList(
    htmltools::includeCSS(
      system.file(file.path("www", "pretty_style.css"), package = utils::packageName())
    ),
    fresh::use_pretty(
      system.file(file.path("www", "pretty_fresh.css"), package = utils::packageName())
    ),
    fresh::use_theme(my_theme)
  )
}


#' Pretty box
#'
#' @description Wrapper around `shinydashboardPlus::box()`.
#'
#' @inheritParams shinydashboardPlus::box
#' @param color Box outline color.
#' @param box_only If `TRUE`, only return the box and not the box wrapped inside
#'   `shiny::fluidRow()`.
#' @param ... Arguments passed to `shinydashboardPlus::box()`.
#'
#' @keywords internal
prettyBox <- function(..., title = NULL, width = 12, color = NULL,
                      status = "primary", solidHeader = TRUE,
                      box_only = FALSE) {
  if (!is.null(title)) {
    title <- htmltools::tags$b(title)
  }
  if (!box_only) {
    shiny_wrapper <- shiny::fluidRow
  } else {
    shiny_wrapper <- function(x) x
  }
  if (!is.null(color)) {
    color_fun <- function(x) {
      x |>
        htmltools::tagAppendAttributes(
          style = css_styler(border = sprintf("1px solid %s", color)),
          .cssSelector = ".box"
        ) |>
        htmltools::tagAppendAttributes(
          style = css_styler(background = color),
          .cssSelector = ".box-header"
        )
    }
  } else {
    color_fun <- function(x) x
  }
  shiny_wrapper(
    shinydashboardPlus::box(
      ..., title = title, color = color,
      width = width, status = status, solidHeader = solidHeader
    ) |>
      color_fun()
  )
}


#' Add spinner when loading
#'
#' @description Wrapper around `shinycssloaders::withSpinner()` with a different
#'   default color.
#'
#' @inheritParams shinycssloaders::withSpinner
#' @param obj Object to add spinner to.
#' @param spinner Logical. Whether or not to add spinner.
#'
#' @examples
#' ## DON'T RUN
#' # shiny::htmlOutput({NAME OF UI ELEMENT}) |> add_spinner()
#'
#' @keywords internal
add_spinner <- function(obj, spinner = TRUE, color = "#18bc9c") {
  if (spinner) {
    obj |> shinycssloaders::withSpinner(color = color)
  } else {
    obj
  }
}


#' Add border class
#'
#' @param obj Object to add border to.
#' @param border Logical. Whether or not to add border.
#'
#' @keywords internal
add_border <- function(obj, border = TRUE) {
  if (border) {
    obj |> htmltools::tagAppendAttributes(class = "box-border")
  } else {
    obj
  }
}


#' Return custom icons
#'
#' @param icon_names Name(s) of built-in icons to return.
#' @param width Width of icon in pixels.
#' @param height Height of icon in pixels.
#' @param ... Additional arguments to pass to `htmltools::img()`.
#'
#' @return Named character vector of html snippets to display icons
#'
#' @keywords internal
get_icons <- function(icon_names, width = 20, height = 20, ...) {

  icons_vec <- c(
    ggplot = get_icon(
      src = "ggplot.png", width = width, height = height, ...
    ),
    plotly = get_icon(
      src = "plotly.svg", width = width, height = height, ...
    ),
    table = get_icon(
      src = "table.svg", width = width, height = height, ...
    ),
    histogram = get_icon(
      src = "chart-column.svg", width = width, height = height, ...
    ),
    density = get_icon(
      src = "chart-density.svg", width = width, height = height, ...
    ),
    boxplot = get_icon(
      src = "chart-boxplot.svg", width = width, height = height, ...
    ),
    point = get_icon(
      src = "chart-scatter.svg", width = width, height = height, ...
    ),
    line = get_icon(
      src = "chart-line.svg", width = width, height = height, ...
    )
  )

  out <- lapply(
    icon_names,
    function(icon_name) {
      ifelse(icon_name %in% names(icons_vec), icons_vec[icon_name], icon_name)
    }
  )

  return(stats::setNames(icon_names, out))
}


#' Get html snippet for built-in icons.
#'
#' @inheritParams get_icons
#' @param src Name of built-in icon.
#'
#' @return Character vector of icon path.
#'
#' @keywords internal
get_icon <- function(src, width = 20, height = 20, ...) {
  dots_ls <- rlang::dots_list(...)
  if (length(dots_ls) > 0) {
    style_args <- paste(names(dots_ls), dots_ls, sep = "=", collapse = " ")
  } else {
    style_args <- ""
  }
  sprintf(
    "<img src='%s' width=%spx height=%spx %s></img>",
    file.path("www", src), width, height, style_args
  )
}


#' Display inline block
#'
#' @param obj Object to display inline.
#'
#' @keywords internal
display_inline <- function(obj) {
  obj |>
    htmltools::tagAppendAttributes(style = css_styler(display = "inline-block"))
}


#' Add vertical space
#'
#' @param size Size of vertical space.
#'
#' @keywords internal
vspace <- function(size = "3px") {
  htmltools::tags$div(htmltools::tags$p("&nbsp;")) |>
    htmltools::tagAppendAttributes(
      style = css_styler(`font-size` = size, color = "transparent")
    )
}


#' @keywords internal
input_header_style <- function(...) {
  shiny::span(...) |>
    shiny::h4() |>
    htmltools::tagAppendAttributes(
      style = css_styler(
        `margin-left` = "0.5em",
        `margin-bottom` = "0em",
        `font-weight` = "bold",
        `font-size` = "20px",
        `font-variant` = "small-caps"
      )
    )
}
