# Wrapper around `dropdown()`

Wrapper around `dropdown()` with different default settings

## Usage

``` r
options_dropdown(
  ...,
  status = "primary",
  size = "sm",
  icon = shiny::icon("cog"),
  width = "300px",
  right = TRUE,
  style = "material-circle",
  tooltip = FALSE
)
```

## Arguments

- ...:

  List of tag to be displayed into the dropdown menu.

- status:

  Color of the button, see
  [`actionBttn()`](https://dreamrs.github.io/shinyWidgets/reference/actionBttn.html).

- size:

  Size of the button : `xs`,`sm`, `md`, `lg`.

- icon:

  An optional icon to appear on the button.

- width:

  Width of the dropdown menu content.

- right:

  Logical. The dropdown menu starts on the right.

- style:

  Style of the button, to choose between `simple`, `bordered`,
  `minimal`, `stretch`, `jelly`, `gradient`, `fill`, `material-circle`,
  `material-flat`, `pill`, `float`, `unite`.

- tooltip:

  Put a tooltip on the button, you can customize tooltip with
  [`tooltipOptions()`](https://dreamrs.github.io/shinyWidgets/reference/tooltipOptions.html).
