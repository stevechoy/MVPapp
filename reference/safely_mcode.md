# purrr:safely wrappers for various functions

purrr:safely wrappers for various functions

## Usage

``` r
safely_mcode(...)
```

## Arguments

- ...:

  args to be passed

## Value

A list with two elements:

- `result`: The result of
  [`mrgsolve::mcode`](https://mrgsolve.org/docs/reference/mcode.html),
  or `NULL` if an error occurred.

- `error`: The error that occurred, or `NULL` if no error occurred.

## See also

`mrgsolve::mrgcode`
