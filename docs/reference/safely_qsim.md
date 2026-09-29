# purrr:safely wrappers for various functions

purrr:safely wrappers for various functions

## Usage

``` r
safely_qsim(...)
```

## Arguments

- ...:

  args to be passed

## Value

A list with two elements:

- `result`: The result of
  [`mrgsolve::qsim`](https://mrgsolve.org/docs/reference/qsim.html), or
  `NULL` if an error occurred.

- `error`: The error that occurred, or `NULL` if no error occurred.

## See also

[`mrgsolve::qsim`](https://mrgsolve.org/docs/reference/qsim.html)
