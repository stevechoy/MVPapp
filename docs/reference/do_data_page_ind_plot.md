# Individual Plot for Data Exploration

This is the main function for plotting individual plots from uploaded
datasets. It is designed to be flexible enough to handle
continuous/continuous, discrete/continuous, and discrete/discrete type
of data to cover most use cases.

## Usage

``` r
do_data_page_ind_plot(
  nmd,
  rownums,
  colnums,
  pagenum = 1,
  filter_id,
  filter_cmt,
  sort_by,
  strat_by,
  highlight_range,
  x_axis,
  y_axis,
  color_by,
  med_line,
  med_line_by,
  boxplot,
  dolm,
  smoother,
  facet_name = "ID",
  logy,
  lby = logbreaks_y,
  lbx = logbreaks_x,
  logx,
  plot_title,
  label_size = 3,
  discrete_threshold = 7,
  boxplot_x_threshold = 20,
  error_text_color = "#F8766D",
  highlight_var,
  highlight_var_values,
  plot_dosing = TRUE,
  same_scale = FALSE,
  dose_col = "DOSE",
  dose_units,
  lloq_name = "",
  debug = FALSE
)
```

## Arguments

- nmd:

  The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)

- rownums:

  How many rows per page

- colnums:

  How many cols per page

- pagenum:

  Page number to filter by

- filter_id:

  Filter by these IDs

- filter_cmt:

  Filter by this CMT

- sort_by:

  Sort by one or more variables (a list), default NULL

- strat_by:

  Stratify by 1 variable and highlight outliers, default ”

- highlight_range:

  Flag potential outliers when they are higher or below this mean value
  of the group, a string

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

- dolm:

  Insert linear regression with formula on top of plot

- smoother:

  Insert smoother

- facet_name:

  variable to facet by

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

- highlight_var:

  variable name to be highlighted by a different shape

- highlight_var_values:

  variable values associated with highlight_var to be highlighted

- plot_dosing:

  Plots dosing line and dose text when TRUE

- same_scale:

  Uses fixed scales based on entire dataset when TRUE

- dose_col:

  name of dose column, usually AMT or DOSE

- dose_units:

  (Optional) name of dose units, usually mg or nmol

- lloq_name:

  (Optional) Supply name of lloq column to be plotted as hline

- debug:

  show debugging messages

## Value

a ggplot object
