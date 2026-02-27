# Main function for the plot in Parameter Sensitivity Analysis

Main function for the plot in Parameter Sensitivity Analysis

## Usage

``` r
plot_three_data_with_nm(
  input_dataset_min,
  input_dataset_mid,
  input_dataset_max,
  param_name = "ID",
  param_min_value = -1,
  param_mid_value = -2,
  param_max_value = -3,
  x_min = NULL,
  x_max = NULL,
  nonmem_dataset = NULL,
  xvar = "TIMEADJ",
  yvar = NULL,
  log_x_axis = FALSE,
  log_x_ticks = logbreaks_x,
  log_x_labels = logbreaks_x,
  log_y_axis = FALSE,
  log_y_ticks = logbreaks_y,
  log_y_labels = logbreaks_y,
  geom_point_sim_option = FALSE,
  geom_point_data_option = FALSE,
  geom_ribbon_option = FALSE,
  geom_vline_option = FALSE,
  stat_summary_data_option = FALSE,
  nm_yvar = NULL,
  xlabel = xvar,
  ylabel = yvar,
  debug = FALSE,
  title = NULL
)
```

## Arguments

- input_dataset_min:

  simulated dataframe for Min Parameter

- input_dataset_mid:

  simulated dataframe for Mid Parameter

- input_dataset_max:

  simulated dataframe for Max Parameter

- param_name:

  Name of parameter, string

- param_min_value:

  Min value of parameter, numeric

- param_mid_value:

  Mid value of parameter, numeric

- param_max_value:

  Max value of parameter, numeric

- x_min:

  Min X-axis value for time intervals to filter datasets by

- x_max:

  Max X-axis value for time intervals to filter datasets by

- nonmem_dataset:

  uploaded NONMEM dataset

- xvar:

  Name of X-variable to be plotted, string

- yvar:

  Name of Y-variable to be plotted, string

- log_x_axis, log_x_ticks, log_x_labels, log_y_axis, log_y_ticks,
  log_y_labels:

  Log options and axis tick labels

- geom_point_sim_option, geom_point_data_option:

  Plotting sampling points

- geom_ribbon_option:

  Plots AUC when TRUE

- geom_vline_option:

  Plots time intervals as vertical lines when TRUE

- stat_summary_data_option:

  Plots median line of dataset

- nm_yvar:

  Name of Y-variable for NONMEM dataset, string

- xlabel:

  Nice label for X-axis, string

- ylabel:

  Nice label for Y-axis, string

- debug:

  When TRUE, displays debugging messages

- title:

  Title for plot

## Value

a ggplot object
