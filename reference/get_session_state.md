# Get current MVP session state

Captures all current UI inputs and reactive values into a serializable
list for saving via [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).
Complementary to
[`restore_session_state`](https://stevechoy.github.io/MVPapp/reference/restore_session_state.md).

## Usage

``` r
get_session_state(input, rv, uploaded_data)
```

## Arguments

- input:

  The Shiny `input` object from the current server session.

- rv:

  A
  [`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  object containing server-side state not bound to UI inputs.

- uploaded_data:

  The `uploaded_data` reactive. Wrapped in `tryCatch` so `NULL` is
  stored gracefully if no file is loaded.

## Value

A named list containing `version`, `saved_at`, and sublists `model`,
`data`, `dosing`, `sim`, `psa`, and `variability`.

## See also

[`restore_session_state`](https://stevechoy.github.io/MVPapp/reference/restore_session_state.md)
