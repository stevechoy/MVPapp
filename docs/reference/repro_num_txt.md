# Internal helper: numbers to text, exactly round-trippable

Formats each number with the fewest significant digits (15 to 17) that
read back as the identical double, so that values written into
simulation.R are bit-for-bit what MVP used, while staying as readable as
possible.

## Usage

``` r
repro_num_txt(x)
```

## Arguments

- x:

  A numeric vector

## Value

A character vector, one element per value
