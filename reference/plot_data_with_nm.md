# Main plot for Simulation tab

Main plot for Simulation tab

## Usage

``` r
plot_data_with_nm(
  input_dataset1 = NULL,
  input_dataset2 = NULL,
  nonmem_dataset = NULL,
  color_data_by = NULL,
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
  stat_summary_data_option = FALSE,
  stat_summary_data_by = NULL,
  nm_yvar = NULL,
  xlabel = xvar,
  ylabel = yvar,
  debug = FALSE,
  title = NULL,
  line_color_1 = "#F8766D",
  line_color_2 = "#7570B3"
)
```

## Arguments

- input_dataset1:

  simulated dataframe for Model 1

- input_dataset2:

  simulated dataframe for Model 2

- nonmem_dataset:

  uploaded NONMEM dataset

- color_data_by:

  Color by a variable in the NONMEM dataset

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

- stat_summary_data_option:

  Plots median line of dataset

- stat_summary_data_by:

  Inserts median line by variable

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

- line_color_1:

  Color for Model 1

- line_color_2:

  Color for Model 2

## Value

a ggplot object
