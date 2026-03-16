# Executes a mrgsim call based on user input

Executes a mrgsim call based on user input

## Usage

``` r
run_single_sim(
  input_model_object,
  pred_model = FALSE,
  ev_df,
  wt_based_dosing = FALSE,
  wt_name = "WT",
  model_dur = FALSE,
  model_rate = FALSE,
  sampling_times,
  seed = 1000,
  divide_by = 1,
  debug = FALSE,
  nsubj = 1,
  append_id_text = "m1-",
  ext_db = NULL,
  parallel_sim = FALSE,
  parallel_n = 200
)
```

## Arguments

- input_model_object:

  mrgmod object

- pred_model:

  Default FALSE. set to TRUE to set ev_df CMT to 0

- ev_df:

  ev() dataframe containing dosing info

- wt_based_dosing:

  When TRUE, will try to use weight-based dosing

- wt_name:

  Name of weight parameter in the model to multiply dose amount by

- model_dur:

  Default FALSE, set to TRUE to model duration inside the code

- model_rate:

  Default FALSE, set to TRUE to model rate inside the code

- sampling_times:

  A vector of sampling times (note: not a tgrid object)

- seed:

  Seed number for RNG for reproducibility

- divide_by:

  Divide the TIME by this value, used for scaling x-axis

- debug:

  Default FALSE, set to TRUE to show more messages in console

- nsubj:

  Default 1, in which case mrgsolve::zero_re() will be applied

- append_id_text:

  A string prefix to be inserted for each ID

- ext_db:

  Default NULL, supply R object of external database

- parallel_sim:

  Default TRUE, uses the future and mrgsim.parallel packages !Not
  implemented live!

- parallel_n:

  The number of subjects required before parallelization is used !Not
  implemented live!

## Value

a df
