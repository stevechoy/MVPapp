# Function that combines multiple columns to create a new one that contains the facet labels

Function that combines multiple columns to create a new one that
contains the facet labels

## Usage

``` r
create_facet_label(df, sort_by)
```

## Arguments

- df:

  Name of input dataframe

- sort_by:

  A list containing columns to create label by. Uses first row only

## Value

a dataframe with the new column called "facet_label"
