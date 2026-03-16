# Search for Likely TIME Columns

Searches through several common column names that could be used to
create the "TIME" column in the dataset

## Usage

``` r
search_time_col(
  orig_df,
  names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD")
)
```

## Arguments

- orig_df:

  The dataframe used for searching

- names_of_time_cols:

  Likely column names, in order of search priority

## Value

a dataframe
