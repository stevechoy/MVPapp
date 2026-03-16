# Draws a tornado plot

Draws a tornado plot

## Usage

``` r
tornado_plot(
  df,
  reference_value,
  param_name = ".paramname",
  lower_name = "Lower",
  upper_name = "Upper",
  lower_color = "#FC8D62",
  upper_color = "#66C2A5",
  metric_name = "",
  plot_title = "",
  display_as = "Percentage",
  filter_rows = 20,
  display_text = FALSE,
  xlabname = "",
  bioeq_lines = FALSE
)
```

## Arguments

- df:

  Input df containing parameter name, lower bound name, and upper bound
  name

- reference_value:

  Untransformed reference value

- param_name:

  Column name for parameters

- lower_name:

  Column name for lower bounds

- upper_name:

  Column name for upper bounds

- lower_color:

  Fill color for lower bounds in bars

- upper_color:

  Fill color for upper bounds in bars

- metric_name:

  Exposure metric name to be used for plot label

- plot_title:

  Plot title

- display_as:

  A choice between "Ratio", "Percentage", and "Value"

- filter_rows:

  Filter by X number of rows

- display_text:

  When TRUE, displays the size as a text labl at the end of each bar

- xlabname:

  Custom name for X-axis (technically the Y-axis due to coord_flip)

- bioeq_lines:

  When TRUE, plots the 80% / 125% lines relative to reference value

## Value

a ggplot object
