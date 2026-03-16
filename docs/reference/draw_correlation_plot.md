# Draw a correlation plot from a NONMEM-formatted dataset

Draw a correlation plot from a NONMEM-formatted dataset

## Usage

``` r
draw_correlation_plot(
  input_df,
  corr_variables,
  color_sep = "",
  catcov_threshold = 10,
  debug = FALSE
)
```

## Arguments

- input_df:

  Input dataframe

- corr_variables:

  Vector (string) of names to be used in the plot

- color_sep:

  Variable (string) name used for colour separator

- catcov_threshold:

  assumes a covariate is categorical if the unique values in the
  covariate are less than this threshold (default 10), should be less
  than nsubj that is available from the dataset

- debug:

  show debugging messages

## Value

a ggplot object
