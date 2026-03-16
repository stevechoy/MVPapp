# Function that categorizes outliers relative to a supplied stratification group

Function that categorizes outliers relative to a supplied stratification
group

## Usage

``` r
categorize_outliers(df, highlight_range, y_axis, strat_by, debug = FALSE)
```

## Arguments

- df:

  Name of input dataframe

- highlight_range:

  A character string in percentage e.g. "80%" to define outlier
  threshold

- y_axis:

  Y-axis to derive the arithmetic mean for ID and group

- strat_by:

  Column name to be used as a stratification variable for group

- debug:

  Show debugging messages

## Value

a dataframe with the new column called "outlier_status", with three
categories: "Above", "Below", "Within"
