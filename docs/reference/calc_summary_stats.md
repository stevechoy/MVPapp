# Calculate summary stats with optional rounding

Calculate summary stats with optional rounding

## Usage

``` r
calc_summary_stats(
  orig_data,
  dp = 1,
  sigdig = FALSE,
  convert_to_numeric = TRUE,
  transpose = FALSE,
  check_empty_rows = TRUE,
  id_colname = "ID",
  comma_format = TRUE,
  replace_non_numeric_to_NA = TRUE
)
```

## Arguments

- orig_data:

  input dataframe

- dp:

  round to this many decimal places

- sigdig:

  Set to TRUE to round using significant digits instead

- convert_to_numeric:

  Default TRUE, convert df to numeric and turns characters to NA's

- transpose:

  Set to TRUE to transpose the table such that each variable (column)
  becomes a row

- check_empty_rows:

  Default TRUE, will not perform summary stats if there are no rows in
  df

- id_colname:

  The ID column to distinct by and then removed before summary calcs are
  done

- comma_format:

  Set to TRUE to use big mark formatting

- replace_non_numeric_to_NA:

  Set to TRUE to replace non-numeric characters with NA

## Value

a dataframe with summary stats
