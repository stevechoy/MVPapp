# Helper function to handle ev_df transformations group

Helper function to handle ev_df transformations group

## Usage

``` r
transform_ev_df(
  mod,
  ev_df,
  model_dur,
  model_rate,
  pred_model,
  debug = FALSE,
  wt_based_dosing = FALSE,
  wt_name = "WT"
)
```

## Arguments

- mod:

  mrgsolve model object

- ev_df:

  Input event dataframe

- model_dur:

  When TRUE, models duration

- model_rate:

  When TRUE, models rate

- pred_model:

  When TRUE, the model is a PRED model

- debug:

  When TRUE, outputs debugging messages

- wt_based_dosing:

  When TRUE, tries to apply weight-based dosing

- wt_name:

  Name of weight parameter in the model

## Value

a cleaned event dataframe
