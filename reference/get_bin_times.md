# Function that derives bin times of x_axis group

Function that derives bin times of x_axis group

## Usage

``` r
get_bin_times(dfcol, bin_num = 20, relative_threshold = 0.05)
```

## Arguments

- dfcol:

  Input dataframe column of x-axis

- bin_num:

  Maximum number of bins, integer

- relative_threshold:

  relative threshold of lumping bins that are close together

## Value

a vector of unique bin times
