# Return summary stats as a list

Return summary stats as a list

## Usage

``` r
calc_summary_stats_as_list(
  nca_df,
  group_by_name,
  list_of_nca_metrics = c("AUCLST", "AUCIFO", "CMAX", "CMAXD", "LAMZHL", "TMAX",
    "MRTIVLST", "MRTIVIFO", "MRTIVIFP", "MRTEVLST", "MRTEVIFO", "MRTEVIFP", "VZO", "VZP",
    "VZFO", "VZFP", "CLO", "CLP", "CLFO", "CLFP"),
  dp = 3,
  sigdig = TRUE,
  convert_to_numeric = FALSE,
  transpose = TRUE,
  id_colname = "ID"
)
```

## Arguments

- nca_df:

  input dataframe (after using NonCompart::tblNCA)

- group_by_name:

  the singular "key" argument for NonCompart::tblNCA *minus* the ID
  column

- list_of_nca_metrics:

  columns to retain in the summary table

- dp:

  round to this many decimal places

- sigdig:

  Set to TRUE to round using significant digits instead

- convert_to_numeric:

  Default FALSE, convert df to numeric and turns characters to NA's

- transpose:

  Set to TRUE to transpose the table such that each variable (column)
  becomes a row

- id_colname:

  The ID column to distinct by and then removed before summary calcs are
  done

## Value

a list containing X number of dataframes for each unique value of
group_by_name
