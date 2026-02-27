# Creates upper and lower percentiles by TIME for a given dataset

Creates upper and lower percentiles by TIME for a given dataset

## Usage

``` r
quantile_output(
  iiv_sim_input,
  yvar = "DV",
  lower_quartile = 0.025,
  upper_quartile = 0.975,
  dp = 5
)
```

## Arguments

- iiv_sim_input:

  a dataframe containing TIME column

- yvar:

  name of column to do the quantile calculation on

- lower_quartile:

  lower quantile for yvar, default 0.025 (2.5%)

- upper_quartile:

  lower quantile for yvar, default 0.0975 (97.5%)

- dp:

  decimal places for rounding

## Value

a dataframe
