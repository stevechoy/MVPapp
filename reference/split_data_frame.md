# Function to split dataframes into roughly equal portions to facilitate distribution to each core

Function to split dataframes into roughly equal portions to facilitate
distribution to each core

## Usage

``` r
split_data_frame(df, N = 4)
```

## Arguments

- df:

  data.frame to split

- N:

  number of chunks / cores

## Value

A list containing N data.frames. If N=1, then the original df is
returned unchanged
