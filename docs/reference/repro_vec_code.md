# Internal helper: renders a numeric vector as a literal in R code

Writes the vector out in full as c(...), wrapped over several lines,
using the exactly round-trippable number format of repro_num_txt().

## Usage

``` r
repro_vec_code(x, name)
```

## Arguments

- x:

  A numeric vector

- name:

  Name of the object to assign to in the generated code

## Value

A character vector of R code lines
