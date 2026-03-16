# Draws a spider plot

Draws a spider plot

## Usage

``` r
spider_plot(
  df,
  reference_value,
  param_name = ".paramname",
  ratio_name = ".ratio",
  metric_name = "Cmax",
  plot_title = "",
  display_as = "Value",
  filter_rows = 20,
  ylabname = "",
  normalize_x_axis = FALSE,
  bioeq_lines = FALSE
)
```

## Arguments

- df:

  Input df containing parameter name, ratio (fold-change), and metric

- reference_value:

  Untransformed reference value

- param_name:

  Column name for parameters

- ratio_name:

  Column name for ratio

- metric_name:

  Column name for metric

- plot_title:

  Plot title

- display_as:

  A choice between "Ratio", "Percentage", and "Value"

- filter_rows:

  Filter by X number of rows

- ylabname:

  Custom name for y-axis

- normalize_x_axis:

  Treat x-axis as a factor

- bioeq_lines:

  When TRUE, plots the 80% / 125% lines relative to reference value

## Value

a ggplot object
