#' Wrapper around `pickerInput()` to select one choice
#'
#' @description Wrapper around `pickerInput()` to select one choice but with
#'   different default settings.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param ... Additional arguments to pass to `shinyWidgets::pickerInput()`.
#'
#' @keywords internal
picker_input <- function(inputId, label, choices, selected = NULL,
                         options = NULL, ...) {
  if (is.null(options)) {
    options <- list(
      `live-search` = TRUE,
      size = 5,
      title = "Nothing selected"
    )
  }
  shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    options = options,
    ...
  )
}


#' Wrapper around `prettyRadioButtons()`
#'
#' @description Wrapper around `prettyRadioButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::prettyRadioButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::prettyRadioButtons()`.
#'
#' @keywords internal
radio_buttons <- function(inputId, label, choices, selected = NULL,
                          status = "primary", animation = "jelly",
                          icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyRadioButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  ) |>
    set_margins(bottom = "0px")
}


#' Wrapper around `radioGroupButtons()`
#'
#' @description Wrapper around `radioGroupButtons()` but with different
#'   default settings.
#'
#' @inheritParams shinyWidgets::radioGroupButtons
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::radioGroupButtons()`.
#'
#' @keywords internal
radio_group_buttons <- function(inputId, label, choices, selected = NULL,
                                status = "default", size = "sm",
                                justified = FALSE, ...) {
  shinyWidgets::radioGroupButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    size = size,
    justified = justified,
    ...
  )
}


#' Wrapper around `materialSwitch()`
#'
#' @description Wrapper around `materialSwitch()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::materialSwitch
#' @param ... Additional arguments to pass to `shinyWidgets::materialSwitch()`.
#'
#' @keywords internal
material_switch <- function(inputId, label, value = TRUE, status = "primary",
                            right = FALSE, inline = FALSE, ...) {
  shinyWidgets::materialSwitch(
    inputId = inputId,
    label = htmltools::tags$b(label),
    value = value,
    status = status,
    inline = inline,
    right = right,
    ...
  )
}


#' Wrapper around `prettyCheckbox()`
#'
#' @description Wrapper around `prettyCheckbox()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckbox
#' @param ... Additional arguments to pass to `shinyWidgets::prettyCheckbox()`.
#'
#' @keywords internal
checkbox <- function(inputId, label, value = FALSE,
                     status = "primary", animation = "jelly",
                     icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyCheckbox(
    inputId = inputId,
    label = label,
    value = value,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Wrapper around `prettyCheckboxGroup()`
#'
#' @description Wrapper around `prettyCheckboxGroup()` with different default
#'   settings.
#'
#' @inheritParams shinyWidgets::prettyCheckboxGroup
#' @param ... Additional arguments to pass to
#'   `shinyWidgets::prettyCheckboxGroup()`.
#'
#' @keywords internal
checkbox_group <- function(inputId, label, choices, selected = NULL,
                           status = "primary", animation = "jelly",
                           icon = shiny::icon("check"), bigger = TRUE, ...) {
  shinyWidgets::prettyCheckboxGroup(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    status = status,
    animation = animation,
    icon = icon,
    bigger = bigger,
    ...
  )
}


#' Reset button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
#' @keywords internal
reset_button <- function(inputId = "reset_input", label = "Reset all inputs",
                         ...) {
  htmltools::div(
    style = "display: inline-block; width: 48%; text-align: center;",
    shiny::actionButton(inputId = inputId, label = label, ...)
  )
}

#' Run button
#'
#' @description Customized action button using `actionButton()`.
#'
#' @inheritParams shiny::actionButton
#' @param ... Additional arguments to pass to `shiny::actionButton()`.
#'
#' @keywords internal
run_button <- function(inputId = "run_methods", label = "Run methods", ...) {
  htmltools::div(
    style = "display: inline-block; width: 48%; text-align: center;",
    shiny::actionButton(inputId = inputId, label = label, ...)
  )
}


#' Reset all inputs
#'
#' @description Reset all inputs to their default values. Note that
#'   `shinyjs::useShinyjs()` needs to be includes in the UI for this to work.
#'
#' @param input Shiny input.
#' @param session Shiny session.
#' @param reset_input_ids A character vector of input IDs to reset. If NULL,
#'   all inputs will be reset.
#'
#' @keywords internal
reset_inputs <- function(input, session, reset_input_ids = NULL) {
  shiny::observeEvent(input$reset_input, {
    shinyWidgets::confirmSweetAlert(
      session, inputId = "confirm_reset",
      title = "Are you sure you want to reset all inputs?",
      type = "info",
      btn_labels = c("No", "Yes"),
      btn_colors = c("#FE642E", "#04B404"),
      showCloseButton = TRUE
    )
  })
  shiny::observeEvent(input$confirm_reset, {
    if (isTRUE(input$confirm_reset)) {
      if (is.null(reset_input_ids)) {
        shinyjs::refresh()
      } else {
        for (input_id in reset_input_ids) {
          shinyjs::reset(input_id)
        }
      }
    }
  }, ignoreNULL = TRUE)
}


#' Wrapper around `dropdown()`
#'
#' @description Wrapper around `dropdown()` with different default settings
#'
#' @inheritParams shinyWidgets::dropdown
#'
#' @keywords internal
options_dropdown <- function(...,
                             status = "primary", size = "sm",
                             icon = shiny::icon("cog"), width = "300px",
                             right = TRUE, style = "material-circle",
                             tooltip = FALSE) {
  shinyWidgets::dropdown(
    ...,
    status = status, size = size, icon = icon, width = width, style = style,
    tooltip = tooltip, right = right
  ) |>
    htmltools::tagAppendAttributes(class = "options-dropdown")
}
