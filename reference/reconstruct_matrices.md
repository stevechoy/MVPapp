# Converts a matrix object into a list

Each element in the list corresponds to each omega block, corresponding
to provided model object

## Usage

``` r
reconstruct_matrices(
  input_model_object,
  input_matrix,
  name_of_matrix = "omega",
  coerce_to_numeric = TRUE,
  debug = FALSE
)
```

## Arguments

- input_model_object:

  original model object that contains matrix information

- input_matrix:

  single collapsed matrix (assumes NA's for upper triangle and
  non-blocks)

- name_of_matrix:

  "omega" or "sigma"

- coerce_to_numeric:

  Default TRUE, will turn all input to numeric

- debug:

  When TRUE, show debugging messages

## Value

a list with each element containing a block matrix

## Note

The original model object is needed as the configurations of matrices
are dependent on how \$OMEGA and/or \$SIGMA is defined in the model
code. It seems like for each repetition of \$OMEGA and/or \$SIGMA, a new
matrix is created.
