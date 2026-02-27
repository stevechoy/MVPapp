# expects numeric as input, and cleans it

1.  If value is empty (NA) and allow_zero = TRUE, will return 0

2.  If value is empty (NA) and allow_zero = FALSE, will return 1

3.  If value is \<= 0 and allow_zero = TRUE, will return 0

4.  If value is \<= 0 and allow_zero = FALSE, will return 1

5.  If a legal maximum or minimum is provided and the value falls
    outside of those ranges, the legal value will be used instead.

6.  If a return_value has been specified, any time conditions 1-5 is
    triggered, the return_value will be used instead. (therefore the
    return_value MUST make sense)

In all other cases the value is returned as-is.

## Usage

``` r
sanitize_numeric_input(
  numeric_input,
  allow_zero = TRUE,
  as_integer = FALSE,
  return_value = NULL,
  legal_maximum = NULL,
  legal_minimum = NULL,
  display_error = FALSE
)
```

## Arguments

- numeric_input:

  a numeric e.g. from numericInput

- allow_zero:

  Default TRUE, turns the numeric to 0 if input is NA or \<= 0,
  otherwise 1

- as_integer:

  Default FALSE, coerce input into integer

- return_value:

  If the input is bad, return value X. Default NULL i.e. returns 0 (if
  allow_zero = TRUE) or 1 (if allow_zero = FALSE)

- legal_maximum:

  Upper bound numeric that is accepted

- legal_minimum:

  Lower bound numeric that is accepted

- display_error:

  Displays showNotification message box

## Value

a numeric
