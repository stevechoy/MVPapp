# Exports a standalone, reproducible mrgsolve simulation bundle

All simulation settings (seed, sampling times, zero_re toggle, the
ORIGINAL pre-transformed dosing table and the transformation switches)
are written into simulation.R as code, and it rebuilds the event data
from them using a standalone copy of transform_ev_df(). The user can
therefore edit everything directly in simulation.R, which only needs
model.cpp (and covariate_db.rds when a covariate database is used) next
to it. A copy of the original settings is also saved to
orig_settings.rds as a record; simulation.R does not need it.

## Usage

``` r
build_repro_bundle(
  zip_path,
  input_model_object,
  event_data_orig,
  event_data_post,
  sampling_times,
  custom_sampling_time = FALSE,
  tend = NULL,
  tdelta = NULL,
  mw_value = 1,
  mw_multi_factor = 1,
  mw_checkbox = FALSE,
  mw_conversion = 1,
  wt_based_dosing = FALSE,
  wt_name = "WT",
  model_dur = FALSE,
  model_rate = FALSE,
  pred_model = FALSE,
  covariate_db = NULL,
  seed = 1000
)
```

## Arguments

- zip_path:

  Path to write the output .zip archive to

- input_model_object:

  mrgmod object, the compiled model used for the simulation

- event_data_orig:

  The ev() object of dosing info as returned by
  generate_dosing_regimens(), i.e. BEFORE transform_ev_df() but WITH the
  MW conversion already applied to amt (the MW conversion is divided
  back out here so that simulation.R can expose it). Written into
  simulation.R as an editable data.frame. It must not contain an ID
  column: it is the per-subject template, which is replicated across
  covariate_db by simulation.R

- sampling_times:

  A vector of sampling times, passed to tgrid in mrgsolve::mrgsim_df()

- custom_sampling_time:

  Default FALSE. Whether sampling_times came from the user's custom time
  vector (input\$custom_sampling_time_cb) rather than from tend /
  tdelta. When TRUE, the times are written into simulation.R in full as
  a vector. When FALSE, they are written as seq(..., by = tdelta),
  provided that reproduces sampling_times exactly (otherwise in full)

- tend:

  Default NULL. Max sampling time from the app (tend()). Only used when
  custom_sampling_time is FALSE

- tdelta:

  Default NULL. Equidistant step size from the app (tdelta()). Only used
  when custom_sampling_time is FALSE

- mw_value:

  Molecular weight (g/mol). Default 1.

- mw_multi_factor:

  Multiplication factor for MW. Default 1.

- mw_checkbox:

  Default FALSE. The state of the MW checkbox (input\$mw_checkbox). When
  FALSE, no conversion is applied by simulation.R

- mw_conversion:

  Pre-calculated conversion factor, supplied by MVP

- wt_based_dosing:

  Default FALSE. Same as in run_single_sim()

- wt_name:

  Default "WT". Same as in run_single_sim()

- model_dur:

  Default FALSE. Same as in run_single_sim()

- model_rate:

  Default FALSE. Same as in run_single_sim()

- pred_model:

  Default FALSE. Same as in run_single_sim()

- covariate_db:

  Default NULL. A data.frame of covariates, keyed by ID (1:N),
  replicated against the dosing table and joined onto the event data and
  simulated output. When NULL, random effects are zeroed before
  simulating (assumes a single/typical-subject run); when supplied,
  random effects are left as specified in the model

- seed:

  Default 1000. Random seed set before simulation, for reproducibility
  of any simulated variability

- even_data_post:

  The post-transformed ev() used by MVP

## Value

Invisibly, the path to the written .zip file (zip_path)
