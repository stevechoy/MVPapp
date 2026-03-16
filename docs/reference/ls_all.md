# List All Objects

This function returns a character vector of all objects available.
Unlike [`ls()`](https://rdrr.io/r/base/ls.html) this function will loop
through all environments from the current environment to `.GlobalEnv`.
This will also verify that the object is indeed available from the
current environment using the
[`exists()`](https://rdrr.io/r/base/exists.html) function call.

## Usage

``` r
ls_all()
```

## Value

a character vector with the name of all objects available.

## Note

Copied from https://github.com/jbryer/ShinyDemo, R/ls_all.R
