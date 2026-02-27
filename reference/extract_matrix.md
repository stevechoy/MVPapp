# Function to output a matrix from mrgsolve model object

Function to output a matrix from mrgsolve model object

## Usage

``` r
extract_matrix(
  input_model_object,
  name_of_matrix = "omega",
  remove_upper_tri = TRUE,
  remove_zero = TRUE,
  coerce_to_character = TRUE,
  debug = FALSE
)
```

## Arguments

- input_model_object:

  mrgsolve model object to extract from

- name_of_matrix:

  either "omega" or "sigma", will use "omat" or "smat" accordingly

- remove_upper_tri:

  When TRUE, turns upper triangle of matrix to NA_real\_

- remove_zero:

  In case any of them are 0, don't replace the diagonals

- coerce_to_character:

  Default TRUE, turns all output into a character as a workaround to
  prevent rounding

- debug:

  When TRUE, show debugging messages

## Value

a matrix
