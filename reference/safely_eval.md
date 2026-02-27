# purrr:safely wrappers for various functions

purrr:safely wrappers for various functions

## Usage

``` r
safely_eval(...)
```

## Arguments

- ...:

  args to be passed

## Value

A list with two elements:

- `result`: The result of `eval`, or `NULL` if an error occurred.

- `error`: The error that occurred, or `NULL` if no error occurred.

## See also

`eval`
