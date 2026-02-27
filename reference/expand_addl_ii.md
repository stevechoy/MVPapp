# Expand ADDL and II dosing rows

Expands ADDL and II dosing rows. If there are no ADDL and II columns,
return original dataframe unchanged

## Usage

``` r
expand_addl_ii(data, x_axis, dose_col, debug = FALSE)
```

## Arguments

- data:

  The dataframe used for expansion

- x_axis:

  The x_axis variable, usually TIME or TAFD etc

- dose_col:

  The dose variable, usually AMT or DOSE

- debug:

  Show debugging messages

## Value

a dataframe
