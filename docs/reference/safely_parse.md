# purrr:safely wrappers for various functions

purrr:safely wrappers for various functions

## Usage

``` r
safely_parse(...)
```

## Arguments

- ...:

  args to be passed

## Value

A list with two elements:

- `result`: The result of `parse`, or `NULL` if an error occurred.

- `error`: The error that occurred, or `NULL` if no error occurred.

## See also

`parse`
