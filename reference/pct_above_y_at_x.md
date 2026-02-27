# Function that calculates % of population \> Y-value at a given X-value

Function that calculates % of population \> Y-value at a given X-value

## Usage

``` r
pct_above_y_at_x(
  model_is_valid = FALSE,
  input_df,
  y_name = "DV",
  y_value = NA,
  x_name = "TIME",
  x_value = NA,
  return_number_ids = FALSE
)
```

## Arguments

- model_is_valid:

  checkpoint before rest of function proceeds, default FALSE

- input_df:

  Dataset containing ID column with more than 1 ID

- y_name:

  Name of column (string) for Y-value

- y_value:

  Y-value threshold (\>), e.g. plasma concentration

- x_name:

  Name of column (string) for X-value

- x_value:

  X-value threshold (==), e.g. time

- return_number_ids:

  Return number of IDs that fits the criteria instead of % Default FALSE

## Value

a numeric (either as a proportion (from 0 to 1, where 0 means 0% and 1
means 100%), or the actual number of IDs)
