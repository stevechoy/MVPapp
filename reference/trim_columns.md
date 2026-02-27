# Trim columns to only retain essential columns for plotting

Useful to cut down datasets when they are large and unwieldy

## Usage

``` r
trim_columns(
  data,
  x_axis,
  y_axis,
  color = "",
  sort_by = NULL,
  strat_by = "",
  type_of_plot,
  facet_name = NULL,
  insert_med_line = FALSE,
  med_line_by = "",
  ind_dose_colname = "",
  highlight_var = "",
  lloq_colname = ""
)
```

## Arguments

- data:

  The dataframe used for trimming

- x_axis:

  X-axis column name used for plotting

- y_axis:

  Y-axis column name used for plotting

- color:

  (optional) Color by column

- sort_by:

  (optional) A list of columns to sort by

- strat_by:

  (optional) A variable to flag outliers by

- type_of_plot:

  "general_plot", "ind_plot", "sim_plot"

- facet_name:

  (optional) column(s) to facet by

- insert_med_line:

  (optional) insert median line

- med_line_by:

  (optional) The median line to insert, insert_med_line must be TRUE

- ind_dose_colname:

  (optional) Individual plot dose column name

- highlight_var:

  (optional) highlight variable column name

- lloq_colname:

  (optional) LLOQ column name

## Value

a dataframe
