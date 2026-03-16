# Function to extract params from model object

Function to extract params from model object

## Usage

``` r
extract_model_params(input_model_object)
```

## Arguments

- input_model_object:

  Expects a mrgsolve model object

## Value

a dataframe of all names inside \$PARAM where each row contains a
parameter name in column 1, and parameter value in column 2

## Note

Should be a tibble of 2 columns and X rows where X = number of params
