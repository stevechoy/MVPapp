# Restore a saved MVP session state

Restores a previously saved session state by updating all UI inputs and
reactive values to match the saved configuration. Intended to be called
inside an `observeEvent()` triggered by a session file upload, and again
after model compilation completes for inputs that require a compiled
model to exist first.

## Usage

``` r
restore_session_state(
  state,
  input,
  session,
  rv,
  uploaded_data_override,
  show_note
)
```

## Arguments

- state:

  A named list produced by
  [`get_session_state`](https://stevechoy.github.io/MVPapp/reference/get_session_state.md),
  typically loaded via
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html). Must contain a
  `version` field for compatibility checking.

- input:

  The Shiny `input` object from the current server session.

- session:

  The Shiny `session` object passed to all `updateXxx()` calls.

- rv:

  A
  [`reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  object containing server-side state not bound to UI inputs. Fields are
  overwritten directly.

- uploaded_data_override:

  A [`reactiveVal`](https://rdrr.io/pkg/shiny/man/reactiveVal.html) used
  to inject the restored dataset into `uploaded_data()` without
  requiring a file upload.

- show_note:

  Logical. Whether to display a version compatibility notification on
  restore. Default `TRUE`.

## Value

`NULL` invisibly. Called for its side effects.

## Details

Restoration covers: dataset cleaning options, NCA settings, plot
options, dosing regimens for both models, simulation settings, PSA
options, and variability settings. Inputs are restored via the
appropriate `updateXxx()` function for each widget type.

A version compatibility notification is shown if `show_note = TRUE` and
a `version` field is present in `state`, but restoration proceeds
regardless of version match.

## See also

[`get_session_state`](https://stevechoy.github.io/MVPapp/reference/get_session_state.md)
for the complementary save function.
