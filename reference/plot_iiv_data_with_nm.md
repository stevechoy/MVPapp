# Function to plot variability data

Function to plot variability data

## Usage

``` r
plot_iiv_data_with_nm(
  input_dataset1 = NULL,
  input_dataset2 = NULL,
  nonmem_dataset = NULL,
  xvar = NULL,
  yvar = NULL,
  yvar_2 = NULL,
  log_x_axis = FALSE,
  log_x_ticks = logbreaks_x,
  log_x_labels = logbreaks_x,
  log_y_axis = FALSE,
  log_y_ticks = logbreaks_y,
  log_y_labels = logbreaks_y,
  geom_point_sim_option = FALSE,
  geom_point_data_option = FALSE,
  show_ind_profiles = FALSE,
  y_median = NULL,
  y_mean = NULL,
  show_y_mean = FALSE,
  y_min = NULL,
  y_max = NULL,
  line_color_1 = "black",
  line_color_2 = "black",
  stat_summary_data_option = FALSE,
  nm_yvar = NULL,
  xlabel = xvar,
  ylabel = yvar,
  debug = FALSE,
  title = NULL,
  show_x_intercept = FALSE,
  x_intercept_value = NULL,
  show_y_intercept = FALSE,
  y_intercept_value = NULL
)
```

## Arguments

- input_dataset1:

  simulated dataframe for Model 1

- input_dataset2:

  simulated dataframe for Model 2

- nonmem_dataset:

  uploaded NONMEM dataset

- xvar:

  Name of X-variable to be plotted, string

- yvar:

  Name of Y-variable to be plotted, string (Model 1)

- yvar_2:

  Name of Y-variable to be plotted, string (Model 2)

- log_x_axis, log_x_ticks, log_x_labels, log_y_axis, log_y_ticks,
  log_y_labels:

  Log options and axis tick labels

- geom_point_sim_option, geom_point_data_option:

  Plotting sampling points

- show_ind_profiles:

  If TRUE, plots individual profiles instead of prediction intervals

- y_median:

  Prediction interval median (when show_ind_profiles == FALSE)

- y_mean:

  Prediction interval mean (when show_ind_profiles == FALSE)

- show_y_mean:

  Show Mean of Y-value in a dashed line

- y_min:

  Prediction interval min (when show_ind_profiles == FALSE)

- y_max:

  Prediction interval max (when show_ind_profiles == FALSE)

- line_color_1:

  Color for Model 1 prediction intervals

- line_color_2:

  Color for Model 2 prediction intervals

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

- show_x_intercept:

  Display X-intercept for threshold calculation

- x_intercept_value:

  Numeric for X-intercept

- show_y_intercept:

  Display Y-intercept for threshold calculation

- y_intercept_value:

  Numeric for Y-intercept

## Value

a ggplot object
