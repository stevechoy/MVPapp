# Sanity check plots comparing newly created distributions vs NONMEM dataset (or databases)

NOT CURRENTLY USED

## Usage

``` r
compare_dist_histogram(
  df,
  variable_name,
  variable_label,
  lo_percentile = 0.025,
  hi_percentile = 0.975
)
```

## Arguments

- df:

  Dataframe of newly created distribution

- variable_name:

  Name of variable (string) to use for plotting histograms

- variable_label:

  Nice label of variable_name

- lo_percentile:

  Default 0.025, (2.5th percentile)

- hi_percentile:

  Default 0.975, (97.5th percentile)

## Value

a ggplot object
