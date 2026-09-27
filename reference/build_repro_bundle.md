# Exports a standalone, reproducible mrgsolve simulation bundle

Exports a standalone, reproducible mrgsolve simulation bundle

## Usage

``` r
build_repro_bundle(
  zip_path,
  input_model_object,
  event_data,
  event_data_orig,
  sampling_times,
  covariate_db = NULL,
  seed = 1000
)
```

## Arguments

- zip_path:

  Path to write the output .zip archive to

- input_model_object:

  mrgmod object, the compiled model used for the simulation

- event_data:

  The exact data.frame passed to mrgsolve::data_set() for this run (i.e.
  ev_df or ext_db_ev), already fully transformed. This is what allows
  the generated script to skip transform_ev_df() entirely

- event_data_orig:

  Original ev() dosing info, pre-transformed

- sampling_times:

  A vector of sampling times, passed to tgrid in mrgsolve::mrgsim_df()

- covariate_db:

  Default NULL. A data.frame of covariates, keyed by ID, joined back
  onto the simulated output. When NULL, random effects are zeroed before
  simulating (assumes a single/typical-subject run); when supplied,
  random effects are left as specified in the model

- seed:

  Default 1000. Random seed set before simulation, for reproducibility
  of any simulated variability

## Value

Invisibly, the path to the written .zip file (zip_path)
