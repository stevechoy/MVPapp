# Internal helper: renders a data.frame as readable R code

Used by build_repro_bundle() to write the pre-transformed dosing table
into simulation.R as an editable data.frame() call (one line per column)
rather than as an opaque .rds file.

## Usage

``` r
repro_df_to_code(df, name)
```

## Arguments

- df:

  A data.frame (or ev object, which is coerced)

- name:

  Name of the object to assign to in the generated code

## Value

A character vector of R code lines
