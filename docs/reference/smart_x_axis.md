# Function to improve default x-axis breaks

Function to improve default x-axis breaks

## Usage

``` r
smart_x_axis(p1, max_x = NULL, xlabel, debug = FALSE)
```

## Arguments

- p1:

  ggplot object

- max_x:

  Max x-axis value, will be derived from p1 if it is set to NULL

- xlabel:

  Will only apply when xlabel is either "Time (hours)", "Time (weeks)",
  "Time (days)", or "Time (months)

- debug:

  Shows debug messages

- xvar:

  x variable string to be used to derive max_x

## Value

a ggplot object
