# Updates variability for a mrgmod object

Updates variability for a mrgmod object

## Usage

``` r
update_variability(
  input_model_object,
  input_matrix,
  name_of_matrix = "omega",
  check_validity = TRUE,
  debug = FALSE
)
```

## Arguments

- input_model_object:

  original model object to be updated

- input_matrix:

  list of matrices containing new parameter values

- name_of_matrix:

  "omega" or "sigma"

- check_validity:

  Default TRUE, where it checks that the matrix is sensible If check
  fails, the model object is NOT updated

- debug:

  When TRUE, show debugging messages

## Value

a mrgmod object
