# Add a tooltip to an element

Add a tooltip to an element

## Usage

``` r
add_tooltip(
  id,
  tooltip,
  placement = "right",
  allowHTML = TRUE,
  use_id_only = FALSE,
  ...
)
```

## Arguments

- id:

  The id of the element to add the tooltip to.

- tooltip:

  The tooltip to add.

- placement:

  The placement of the tooltip.

- allowHTML:

  Whether to allow HTML in the tooltip.

- use_id_only:

  Whether to use the id only. Default is `FALSE` which will add the
  namespace 'tooltip-icon' to the id. Set to `TRUE` if the tooltip icon
  was not added with `add_tooltip_icon` (e.g., if the tooltip icon was
  added manually).

- ...:

  Arguments passed to `tippy_this`.

## Value

The element with the tooltip.
