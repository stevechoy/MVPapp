# Quick Plot for Data Exploration

This is the main function for plotting uploaded datasets. It is designed
to be flexible enough to handle continuous/continuous,
discrete/continuous, and discrete/discrete type of data to cover most
use cases.

## Usage

``` r
do_data_page_plot(
  nmd,
  filter_cmt,
  x_axis,
  y_axis,
  color_by,
  med_line,
  med_line_by,
  boxplot,
  num_quantiles,
  dolm,
  smoother,
  facet_name,
  logy,
  lby = logbreaks_y,
  lbx = logbreaks_x,
  logx,
  plot_title,
  label_size = 3,
  discrete_threshold = 7,
  boxplot_x_threshold = 20,
  error_text_color = "#F8766D",
  debug = FALSE
)
```

## Arguments

- nmd:

  The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)

- filter_cmt:

  Filter by this CMT

- x_axis:

  X-axis for plot

- y_axis:

  Y-axis for plot

- color_by:

  Color by this column

- med_line:

  When TRUE, will draw median line by equidistant X-axis bins

- med_line_by:

  Column name to draw median line by

- boxplot:

  Draws a geom_boxplot instead of geom_point and geom_line

- num_quantiles:

  Converts a continuous x-axis into discrete number of quantiles

- dolm:

  Insert linear regression with formula on top of plot

- smoother:

  Insert smoother

- facet_name:

  variable(s) to facet by

- logy:

  Log Y-axis

- lby:

  Log breaks for Y-axis

- lbx:

  Log breaks for X-axis

- logx:

  Log X-axis

- plot_title:

  Optional plot title

- label_size:

  font size for geom_text labels (N=x for boxplots or linear
  regressions)

- discrete_threshold:

  Draws geom_count if there are \<= this number of unique Y-values

- boxplot_x_threshold:

  Throws an error when the unique values of x-axis exceeds this number

- error_text_color:

  error text color for element text

- debug:

  show debugging messages

## Value

a ggplot object
