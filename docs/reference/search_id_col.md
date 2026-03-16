# Search for Likely ID Columns

Searches through several common column names that could be used to
create the "ID" column in the dataset, and push that to the first column
of the dataset

## Usage

``` r
search_id_col(
  orig_df,
  names_of_id_cols = c("SUBJIDN", "SUBJID", "USUBJID", "PTNO")
)
```

## Arguments

- orig_df:

  The dataframe used for searching

- names_of_id_cols:

  Likely column names, in order of search priority

## Value

a dataframe
