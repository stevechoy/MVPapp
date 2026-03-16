# Function that adds on to the an existing bsPopover such that the tooltip persists after updateSelectInput etc

Function that adds on to the an existing bsPopover such that the tooltip
persists after updateSelectInput etc

## Usage

``` r
update_resistant_popover(
  id,
  title,
  content,
  placement = "bottom",
  trigger = "hover",
  options = NULL
)
```

## Arguments

- id:

  The id of the element to attach the popover to.

- title:

  The title of the popover.

- content:

  The main content of the popover.

- placement:

  Where the popover should appear relative to its target (`top`,
  `bottom`, `left`, or `right`). Defaults to `"bottom"`.

- trigger:

  What action should cause the popover to appear? (`hover`, `focus`,
  `click`, or `manual`). Defaults to `"hover"`.

- options:

  other options

## Value

A popover
