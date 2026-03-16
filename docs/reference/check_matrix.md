# Checks for validity of matrix

Checks for validity of matrix

## Usage

``` r
check_matrix(
  input_model_object,
  input_matrix,
  check_diagonal = TRUE,
  check_symmetry = FALSE,
  debug = FALSE,
  display_error = TRUE
)
```

## Arguments

- input_model_object:

  mrgmod object

- input_matrix:

  a list where each element is a matrix

- check_diagonal:

  Default TRUE, check that each diagonal must be \>= 0

- check_symmetry:

  Default FALSE, check for symmetry

- debug:

  Show debugging messages

- display_error:

  Displays a showNotification message pop-up

## Value

logical (TRUE/FALSE), where TRUE means pass (valid matrix)
