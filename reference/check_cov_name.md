# Function to return a DUMMY name if supplied string is reserved

Function to return a DUMMY name if supplied string is reserved

## Usage

``` r
check_cov_name(
  orig_name,
  replaced_name = "DUMMY",
  list_of_reserved_strings = c("AGE", "AGEMO", "SEX", "WT", "BMI", "BSA")
)
```

## Arguments

- orig_name:

  original name

- replaced_name:

  replacement name

- list_of_reserved_strings:

  unavailable names

## Value

a string
