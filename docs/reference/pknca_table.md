# Function to calculate summary statistics from simulated output

Function to calculate summary statistics from simulated output

## Usage

``` r
pknca_table(
  input_simulated_table,
  output_conc,
  start_time = NULL,
  end_time = NULL,
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

- debug:

  show debugging messages

## Value

a dataframe with additional columns of summary stats
