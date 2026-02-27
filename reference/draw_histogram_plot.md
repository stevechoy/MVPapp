# Draw a histogram plot from a NONMEM-formatted dataset for covariates

Draw a histogram plot from a NONMEM-formatted dataset for covariates

## Usage

``` r
draw_histogram_plot(input_df, hist_variables, bin_size = 5, debug = FALSE)
```

## Arguments

- input_df:

  Input dataframe

- hist_variables:

  Vector (string) of names to be used in the plot

- bin_size:

  Size of bin_width used for histogram

- debug:

  show debugging messages

## Value

a ggplot object
