# Help icon with a Bootstrap 3 popover

Returns a question-mark icon that shows a popover on hover or click. Use
it where
[`shinyBS::bsPopover()`](https://rdrr.io/pkg/shinyBS/man/bsPopover.html)
fails (e.g.
[`shinydashboard::box()`](https://rdrr.io/pkg/shinydashboard/man/box.html)
titles).

Requires the delegated popover init script to be included once in the UI
(e.g. in `dashboardBody(tags$head(...))`). Without it, nothing is shown.

## Usage

``` r
help_popover(title, content, placement = "right", trigger = "hover")
```

## Arguments

- title:

  Popover heading.

- content:

  Popover body. HTML is rendered.

- placement:

  `"top"`, `"bottom"`, `"left"` or `"right"` (default).

- trigger:

  `"hover"` (default) or `"click"`.

## Value

An `<i>`
[htmltools::tag](https://rstudio.github.io/htmltools/reference/builder.html).
