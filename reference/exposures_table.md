# Function to calculate exposures from IIV output

Function to calculate exposures from IIV output

## Usage

``` r
exposures_table(
  input_simulated_table,
  output_conc,
  start_time = NULL,
  end_time = NULL,
  carry_out = "ID",
  debug = FALSE
)
```

## Arguments

- input_simulated_table:

  a dataframe (usually created from mrgsim)

- output_conc:

  y variable of interest to perform summary stats calc

- start_time:

  start time interval for metrics

- end_time:

  end time interval for metrics

- carry_out:

  Provide a vector of column names to retain in the summary table

- debug:

  show debugging messages

## Value

a dataframe with additional columns of summary stats
