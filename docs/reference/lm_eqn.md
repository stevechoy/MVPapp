# Linear regression text (for plotly)

Linear regression text (for plotly)

## Usage

``` r
lm_eqn(df, facet_name, x, y)
```

## Arguments

- df:

  A dataframe

- facet_name:

  Optional column name (string) to facet by

- x:

  column name (string) of x-variable

- y:

  column name (string) of y-variable

## Value

A dataframe containing the column "label" containing the string formula
of R2 coefficient for use in ggplot labels or title
