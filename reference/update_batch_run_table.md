# Update batch run parameter table

Update batch run parameter table

## Usage

``` r
update_batch_run_table(
  param_df,
  lower_multiplier = 0.5,
  upper_multiplier = 1.5,
  last_change_ref_index = 0,
  length_lower_change = 0,
  length_upper_change = 0
)
```

## Arguments

- param_df:

  Input dataframe containing mrgsolve parameter names and values
  ("Name", "Reference", "Lower", "Upper")

- lower_multiplier:

  lower bound multiplier

- upper_multiplier:

  upper bound multiplier

- last_change_ref_index:

  Row index if last change through the UI was a reference value,
  otherwise 0

- length_lower_change:

  Length of lower bound changes

- length_upper_change:

  Length of upper bound changes

## Value

a df containing mrgsolve parameter names and values ("reference",
"lower", "upper") as characters
