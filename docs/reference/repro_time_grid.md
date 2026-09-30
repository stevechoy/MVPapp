# Internal helper: finds seq() parameters that reproduce a sampling grid

Looks for (tend, tdelta) such that seq(x1, tend, by = tdelta) returns
exactly the vector x. The values supplied by the app are tried first, so
that simulation.R shows the tend / tdelta the user actually set (which
matters when tend is not a multiple of tdelta); otherwise they are
derived from x itself.

## Usage

``` r
repro_time_grid(x, tend = NULL, tdelta = NULL)
```

## Arguments

- x:

  Numeric vector of sampling times

- tend:

  Optional. Max sampling time as set in the app

- tdelta:

  Optional. Equidistant step size as set in the app

## Value

list(from, tend, tdelta), or NULL if x is not a regular grid
