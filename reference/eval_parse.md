# Short-hand function to eval, and then parse a string

Short-hand function to eval, and then parse a string

## Usage

``` r
eval_parse(text)
```

## Arguments

- text:

  Some text

## Value

A list with two elements:

- `result`: The result of `eval(parse(text))`, or `NULL` if an error
  occurred.

- `error`: The error that occurred, or `NULL` if no error occurred.
