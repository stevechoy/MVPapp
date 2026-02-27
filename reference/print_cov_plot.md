# Prints distribution of a custom covariate

Prints distribution of a custom covariate

## Usage

``` r
print_cov_plot(data, lo_percentile = 0.025, hi_percentile = 0.975)
```

## Arguments

- data:

  input dataframe, must contain SEX (Male == 0, Female == 1), AGE, WT

- lo_percentile:

  Default 0.025, (2.5th percentile)

- hi_percentile:

  Default 0.975, (97.5th percentile)

## Value

a ggplot object
