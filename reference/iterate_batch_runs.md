# Perform multiple simulations from a dataframe containing parameters

Perform multiple simulations from a dataframe containing parameters

## Usage

``` r
iterate_batch_runs(
  batch_run_df,
  input_model_object,
  pred_model = FALSE,
  wt_based_dosing = FALSE,
  wt_name = "WT",
  ev_df,
  model_dur = FALSE,
  model_rate = FALSE,
  sampling_times,
  divide_by = 1,
  debug = FALSE,
  append_id_text = "m1-",
  show_progress = TRUE,
  gradient = FALSE,
  parallel_sim = FALSE,
  parallel_n = 200
)
```

## Arguments

- batch_run_df:

  Input batch run dataframe containing "Name", "Reference", "Lower",
  "Upper"

- input_model_object:

  mrgmod object

- pred_model:

  Default FALSE. set to TRUE to set ev_df CMT to 0

- wt_based_dosing:

  When TRUE, will try to use weight-based dosing

- wt_name:

  Name of weight parameter in model to multiply dose amt by

- ev_df:

  ev() dataframe containing dosing info

- model_dur:

  Default FALSE, set to TRUE to model duration inside the code

- model_rate:

  Default FALSE, set to TRUE to model rate inside the code

- sampling_times:

  A vector of sampling times (note: not a tgrid object)

- divide_by:

  Divide the TIME by this value, used for scaling x-axis

- debug:

  When TRUE, outputs debugging messages

- append_id_text:

  A string prefix to be inserted for each ID

- show_progress:

  When TRUE, shows shiny progress messages

- gradient:

  When TRUE, perform gradient runs in between lower/upper

- parallel_sim:

  Default FALSE, uses the future and mrgsim.parallel packages !Not
  implemented live!

- parallel_n:

  The number of subjects required before parallelization is used !Not
  implemented live!

## Value

a df mrgsolve output totaling 2 \* params + 1 (reference) runs, or 8 \*
params + 1 if gradient option is used
