# Changelog

## MVPapp 1.0.0

First release prepared for CRAN.

### Breaking changes

- Only
  [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md),
  [`mvp_app()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  and the `%>%` re-export are exported; the helper functions, model
  templates and UI settings are now internal. They remain available by
  name inside the app, the in-app code editors and `pw_models_path`
  scripts.
- The inactive `use_bi_styling` argument of
  [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  was removed, together with the old logo files and color names.
- `nonmem2mrgsolve` is declared in Suggests via the
  `stevechoy.r-universe.dev` repository; `httr2`, `curl` and `mime`
  (only needed for AI translation) moved from Imports to Suggests.
- License is now declared as `GPL (>= 3)`.
- Plots and derived metrics (AUC, Cmax, percentiles, sensitivity and
  exposure tables) use observation records (EVID == 0) only, so a dose
  at a sampling time no longer shows as a second, post-dose point
  (e.g. at 72 h with 12 doses every 12 h). Cmax, AUC and similar metrics
  now only reflect sampled values. The downloadable simulated data and
  the reproducible bundle’s `solved_output` still contain all records,
  with dose records kept only at sampled times, and the bundle’s example
  plot shows observations only
  ([\#73](https://github.com/stevechoy/MVPapp/issues/73)).

### Improvements

- [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  now opens the app in your web browser by default
  (`launch.browser = TRUE`), also from RStudio, which otherwise shows it
  in the viewer pane; pass `launch.browser = FALSE` to turn this off
- Optional company branding:
  [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)/[`mvp_app()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  gain `brand_color`, `brand_text_color`, `brand_logo`, `brand_favicon`
  and `page_title` (all `NULL` by default, which keeps the standard
  look) to set the navbar/panel/table-header colour, a navbar logo, a
  favicon and the page title at launch, without editing package files
  ([\#76](https://github.com/stevechoy/MVPapp/issues/76))
- New default browser-tab icon (two concentration-time curves on the
  app’s navy); `brand_favicon` still replaces it
  ([\#76](https://github.com/stevechoy/MVPapp/issues/76))
- The browser tab title is now “Model Visualization Platform (MVP)”
  instead of showing the raw `<div>` markup
  ([\#76](https://github.com/stevechoy/MVPapp/issues/76))
- Simplified launching
  ([\#31](https://github.com/stevechoy/MVPapp/issues/31)): new exported
  [`mvp_app()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  returns the app object for deployment (an `app.R` containing
  [`MVPapp::mvp_app()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)),
  replacing `standalone_mode` in `inst/shiny/app.R`, which downloaded
  code from GitHub at every start. Launching without installation via
  [`shiny::runGitHub()`](https://rdrr.io/pkg/shiny/man/runUrl.html) now
  loads MVPapp into the app’s own environment instead of the global
  environment, checks dependencies against DESCRIPTION, and can be
  pinned to a release with `ref`. `inst/shiny/packages.R` was removed.
- Global options used by the app (`scipen`, `DT.options`,
  `shiny.maxRequestSize`) are set when the app starts and restored when
  it stops.
- [`run_mvp()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  no longer modifies the global environment or search path, and restores
  any global options it changes when the app closes. Extra arguments in
  `...` now reach the app.
- Objects previously written to the global environment for debugging
  (e.g. `llm_result`, `llm_refine`, `last_ev_df`) are now kept in
  `MVPapp:::.mvp_state`; see “Objects kept for debugging” in
  [`?run_mvp`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md).
  AI translation responses are only kept when
  `show_debugging_msg = TRUE`.
- Helper functions are split from `functions.R` into topic files.
- Fewer dependencies: ‘forcats’, ‘gridExtra’, ‘purrr’ and ‘tibble’ are
  no longer needed, and ‘GGally’ (used only for the correlation plot in
  Data Exploration) is now optional (Suggests)
  ([\#32](https://github.com/stevechoy/MVPapp/issues/32)).
- Faster app: changing the y-axis or percentiles on the Variability page
  no longer re-runs the simulation, sorting individual plots is about
  200 times faster, and tornado/spider and data plots no longer
  re-render while typing or changing options
  ([\#45](https://github.com/stevechoy/MVPapp/issues/45))
- Removed unused internal functions
  ([\#33](https://github.com/stevechoy/MVPapp/issues/33)).
- Simpler internal code with the same results
  ([\#77](https://github.com/stevechoy/MVPapp/issues/77)): exposure
  metrics share one trapezoidal AUC, the plots share helpers for
  observed data, colours, log axes and regression lines, the tornado and
  spider plots share their axis logic, and the AI-translation requests
  share one request path. Dead code and unused arguments were removed.
  Regression lines are fitted once per group instead of three times, and
  additional doses (ADDL) are written out without a row-by-row loop. A 0
  entered below a field’s legal minimum is now replaced by the minimum.

### Bugfixes

- NCA report failed with `ncar` \>= 0.7.2, which no longer exports its
  PDF helpers ([\#26](https://github.com/stevechoy/MVPapp/issues/26))
- Static “Show Individual Profiles” plot joined all subjects into one
  line; each model’s profiles now use that model’s color
  ([\#27](https://github.com/stevechoy/MVPapp/issues/27))
- Model 2 variability simulation failed when a covariate database was
  selected ([\#28](https://github.com/stevechoy/MVPapp/issues/28))
- Reproducible bundle downloads used the dosing events of whichever
  model was simulated last
  ([\#29](https://github.com/stevechoy/MVPapp/issues/29))
- Launching via
  [`shiny::runGitHub()`](https://rdrr.io/pkg/shiny/man/runUrl.html)
  failed unless shiny was attached
  ([\#30](https://github.com/stevechoy/MVPapp/issues/30))
- The LLM API key is no longer printed to the console during AI
  translation ([\#34](https://github.com/stevechoy/MVPapp/issues/34))
- AI translation: NONMEM to rxode2 (nonmem2rx) works again and accepts
  supporting files (`.ext`, `.phi`, `.cov`, …) alongside the control
  stream or listing file, uploads are checked against the selected
  provider’s file types, and translated code containing backslashes or
  quotes is kept intact
  ([\#35](https://github.com/stevechoy/MVPapp/issues/35))
- Typing in the password box no longer resets the model selection or
  erases edited model code; Model 2 now starts on its intended default
  model ([\#36](https://github.com/stevechoy/MVPapp/issues/36))
- Session loading: a second load via the other “Load Session” button is
  no longer ignored, and all settings are restored correctly
  ([\#37](https://github.com/stevechoy/MVPapp/issues/37))
- NCA: the geometric mean is no longer biased low when values are NA or
  zero, and subject IDs containing quotes no longer break NCA
  ([\#38](https://github.com/stevechoy/MVPapp/issues/38))
- The ‘% CFB’ value box read the Mean % CFB column
  ([\#39](https://github.com/stevechoy/MVPapp/issues/39))
- Variability: a clear message instead of duplicated subjects when too
  few database subjects match the filters, and clearing the Seed box no
  longer closes the session
  ([\#40](https://github.com/stevechoy/MVPapp/issues/40))
- Data Exploration: ‘Flag Outliers’ works with dosing rows present, text
  columns with NA no longer break plots, and ID/TIME are detected from
  `PTNO`/`ATSD` columns
  ([\#41](https://github.com/stevechoy/MVPapp/issues/41))
- Spider plots rank parameters by how much they change the metric
  ([\#42](https://github.com/stevechoy/MVPapp/issues/42))
- Saved sessions record the running MVPapp version (previously always
  0.4.5), the locked-models help text links to this repository, and Data
  Exploration plots no longer print ggplot2 messages to the console
  ([\#46](https://github.com/stevechoy/MVPapp/issues/46))
- Exposure metrics over a time window (Parameter Sensitivity Analysis,
  tornado/spider plots and Variability exposure tables) took the next
  dose’s peak when the window ended on an additional (ADDL) dose time.
  Additional doses are now simulated as separate records, so each dose
  time has its pre-dose and post-dose value; a window uses the pre-dose
  (trough) value at its end and the post-dose value at its start. Cavg
  is now AUC divided by the window length instead of the mean of the
  sampled values ([\#47](https://github.com/stevechoy/MVPapp/issues/47))
- Compiling Model 1 with an error after a model had compiled
  successfully closed the session
  ([\#48](https://github.com/stevechoy/MVPapp/issues/48))
- After loading a saved session, a dataset newly chosen with “Browse
  server files” was ignored in favour of the session’s dataset
  ([\#49](https://github.com/stevechoy/MVPapp/issues/49))
- The “batch results are out of date” warning for tornado/spider plots
  now only shows while the matching “Model N (All Parameters)” tab is
  open, instead of appearing whenever parameters are edited on the
  Simulation page
- Data Exploration plots with a log axis: the grey grid lines now sit at
  1, 2, … 9 times each power of 10, lining up with the log tick marks
  instead of at unrelated positions
- The “For Internal Use Only” watermark is now a plot layer that does
  not depend on the axis scales, so it stays on static (non-interactive)
  plots with log axes, in facets and in downloads; the interactive plots
  keep their own watermark
- “Generate All Individuals (.pdf)”: a last page with fewer subjects
  than rows x columns (e.g. 3 subjects on a 3 x 3 page) now keeps the
  panel size of the other pages, with the unused space left white,
  instead of stretching the panels over the whole page
- Individual plots with “Insert regression” showed every subject’s
  equation in every panel instead of one per panel
  ([\#50](https://github.com/stevechoy/MVPapp/issues/50))
- AI translation with an OpenAI-Compatible provider now sends
  `OPENAI_COMPATIBLE_API_KEY`; previously ellmer fell back to
  `OPENAI_API_KEY`, which could send an OpenAI key to the other provider
  ([\#51](https://github.com/stevechoy/MVPapp/issues/51))
- Variability helpers no longer fail when a model has no `$OMEGA` or
  `$SIGMA` block ([\#52](https://github.com/stevechoy/MVPapp/issues/52))
- Data Input no longer closes the session when a file has columns that
  only differ by case or a `#`/`@` prefix (e.g. `dv` and `DV`, `#ID` and
  `ID`): the first keeps its name and the others get a suffix (`DV_1`).
  De-selected columns that disappear after unticking a built-in option,
  and Data Editor code that doesn’t return a data frame, are also
  handled ([\#53](https://github.com/stevechoy/MVPapp/issues/53))
- Variability: pulling covariate distributions from a dataset without an
  `ID` column, or without a previously selected column, no longer closes
  the session. Switching patient database no longer errors while the age
  range updates, and the NHANES age slider is limited to the ages in the
  database (2-85 years)
  ([\#54](https://github.com/stevechoy/MVPapp/issues/54))
- Variability: an edited OMEGA/SIGMA matrix is always checked for
  positive semi-definiteness (also with `@correlation`, with a small
  tolerance for valid rank-deficient blocks), and a matrix mrgsolve
  refuses no longer closes the session
  ([\#55](https://github.com/stevechoy/MVPapp/issues/55))
- Variability: a single simulated subject now gets variability and
  sampled covariates, the sex ratio is exact (e.g. 7 of 10 subjects male
  at 70%), covariates the model also outputs (e.g. `$CAPTURE WT`) are no
  longer duplicated as `WT.x`/`WT.y`, and changing Model 1’s number of
  subjects no longer re-simulates Model 2
  ([\#56](https://github.com/stevechoy/MVPapp/issues/56))
- Mean % change from baseline (MEANCFBPCT) is now time-weighted, like
  Cavg, instead of the mean of the sampled values
  ([\#57](https://github.com/stevechoy/MVPapp/issues/57))
- Median lines no longer fail when the x-axis has a single value
  (e.g. trough-only data); tornado plots no longer fail when no
  parameter changes the metric; regression lines work with column names
  such as `Conc (ng/mL)`
  ([\#58](https://github.com/stevechoy/MVPapp/issues/58))
- Duration/rate syntax (`D_`/`R_`) is detected when indented, e.g. under
  `$MAIN` ([\#59](https://github.com/stevechoy/MVPapp/issues/59))
- NCA table labels: `AUCPEO` is the observed (not predicted) %
  extrapolated AUC, `AUCPEP` and MRTinf,pred (`MRTIVIFP`) are now
  labelled, and `AUCIFPD` shows its per-dose unit
  ([\#60](https://github.com/stevechoy/MVPapp/issues/60))
- AI translation: a control stream that nonmem2mrgsolve can’t parse no
  longer closes the session, and error messages show the provider’s
  message for that request only
  ([\#61](https://github.com/stevechoy/MVPapp/issues/61))
- Removed the bslib “Navigation containers expect a collection of
  `nav_panel()`” warnings printed by every
  [`mvp_app()`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  call ([\#62](https://github.com/stevechoy/MVPapp/issues/62))
- Data Exploration: `nmdata()` uses the built-in filtered data directly
  instead of an always-true `!is.null(a | b)` check, ‘Download’ of data
  and summaries no longer writes broken CSVs when a value contains a
  comma, the dosing column falls back to `AMTNM` correctly, individual
  plots ignore the unused median-line/smoother options and skip the
  regression label on log axes (where it was shown on the linear scale),
  the legend appears for medians by group, identical Model 1/Model 2
  names in the exposure boxplot no longer merge into one box, and an
  empty sort selection no longer leaves a trailing comma in panel labels
  ([\#63](https://github.com/stevechoy/MVPapp/issues/63))
- UI: swapped popover trigger/placement, wrong popover IDs and a
  leftover debug popover fixed, passwords are masked, the Apply/Cancel
  dialog button is labelled “Close”, dead code and typos removed,
  `selected =` used instead of partial matching, and the Model 2 tornado
  summary and covariate popovers use the right labels
  ([\#64](https://github.com/stevechoy/MVPapp/issues/64))
- Performance: the AutoComplete toggle registers one editor observer
  instead of one per click, the simulation plot container is no longer
  rebuilt (flicker, lost zoom) on every change, custom sampling times
  are parsed once, NCA subsetting splits the data once (2000 subjects:
  ~10 s to ~0.2 s), and axis breaks are derived from the data instead of
  `ggplot_build()`
  ([\#65](https://github.com/stevechoy/MVPapp/issues/65))
- AI translation: Reset updates the local-parse checkbox, an empty
  OpenAI-Compatible URL is rejected, the upload list accepts all
  nonmem2rx file types, a lone `.ext` is no longer sent as a binary
  file, Dify service names are shared, and the test compile is skipped
  for non-mrgsolve output
  ([\#66](https://github.com/stevechoy/MVPapp/issues/66))
- NCA/metrics: a cleared molecular weight no longer errors, PDF devices
  are opened before [`par()`](https://rdrr.io/r/graphics/par.html) (no
  stray `Rplots.pdf`) and closed on error, a warning is shown when a
  time window has no sample at its bounds (values are not interpolated),
  `na.rm` is used in summaries, and a failed bound run is no longer
  ranked as the most sensitive parameter
  ([\#67](https://github.com/stevechoy/MVPapp/issues/67))
- Variability/Sensitivity/Sessions: upper age, weight and BMI bounds are
  inclusive (CDC 0-20 y keeps 240-month subjects), Model 2 uses the same
  maximum subject count as the UI, the eigenvalue check has a tolerance,
  both models share one exposure-window time list, fallback multipliers
  agree, dosing events are kept per session rather than shared across
  users, and saved sessions restore the parameter selector and tornado
  time window ([\#68](https://github.com/stevechoy/MVPapp/issues/68))
- Templates/documentation: the mrgsolve version in the code preambles
  reflects the installed version at load time, the QE-TMDD template no
  longer gives `NaN` for `Lfrac` and labels `TVKD` in nM, the unexported
  `mrgsolve:::code()` is no longer used, and several help pages were
  corrected ([\#69](https://github.com/stevechoy/MVPapp/issues/69))
- The DESCRIPTION URL field no longer lists the DOI link (the DOI is
  cited in Description), launching no longer calls
  [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html),
  and the app restores the R random-number state (`.Random.seed`) when
  it stops, like the global options
  ([\#70](https://github.com/stevechoy/MVPapp/issues/70))
- Simulated output, plots and time-window choices are limited to the
  sampled range again: dosing records after the maximum sampling time
  (e.g. 12 doses every 12 h with a 72 h maximum) extended plots, and
  dosing times were offered as window start/end times
  ([\#71](https://github.com/stevechoy/MVPapp/issues/71))
- Creating a tornado plot no longer prints “Ignoring unknown aesthetics:
  text” to the console
  ([\#72](https://github.com/stevechoy/MVPapp/issues/72))
- Variability: percentiles, prediction intervals and “% above threshold”
  use observation records only, so they no longer blend pre- and
  post-dose values at dose times that are also sampling times
  ([\#73](https://github.com/stevechoy/MVPapp/issues/73))
- Time windows: the Start/End pickers of the NCA and tornado pages offer
  sampled times only after a change, too
  ([\#74](https://github.com/stevechoy/MVPapp/issues/74))
- Multi-user: compiled model names are unique per session and rewritten
  when a session file is loaded, so two sessions (or tabs) no longer
  delete each other’s compiled model
  ([\#74](https://github.com/stevechoy/MVPapp/issues/74))
- Sessions now also save and restore typed `$PARAM` values, applied
  OMEGA/SIGMA matrices, solver settings, `CFB_from_DV`,
  dataset-covariate options and custom covariate parameters
  ([\#74](https://github.com/stevechoy/MVPapp/issues/74))
- Model passwords are also checked on the server; % CFB, Mean % CFB and
  Nadir % CFB show “N/A” instead of `Inf` for a zero baseline;
  nonmem2mrgsolve output is escaped like AI output
  ([\#74](https://github.com/stevechoy/MVPapp/issues/74)).
  [`?run_mvp`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  documents that `authentication_code`, model passwords and AI
  translation are not a security boundary
- Smaller fixes: faceting by a non-syntactic column name, a warning when
  tornado/spider results are out of date, parameter inputs namespaced
  (`param_m1_`/`param_m2_`), repeated notifications replace each other,
  NCA dose from the first dosing record, temporary upload folders
  removed, `iterate_batch_runs()` runs outside Shiny, zero reference
  values handled in sensitivity plots, single weight/BMI matched within
  a tolerance, and `ID`/`USUBJID` reserved as covariate names
  ([\#75](https://github.com/stevechoy/MVPapp/issues/75))

### Documentation

- Help pages for the `cdc.expand`, `nhanes.filtered` and `who.expand`
  datasets describe each column (with units and `SEX` coding), how the
  data were derived, and their sources;
  [`?run_mvp`](https://stevechoy.github.io/MVPapp/reference/run_mvp.md)
  defaults and arguments are up to date
  ([\#46](https://github.com/stevechoy/MVPapp/issues/46))
- Scripts supplied via `pw_models_path` that were copied from the
  template before 1.0.0 should update the end of
  `update_model_choices()`; see
  [`vignette("supply-passwords")`](https://stevechoy.github.io/MVPapp/articles/supply-passwords.md)
  ([\#36](https://github.com/stevechoy/MVPapp/issues/36))

## MVPapp 0.4.7 (2026-10-03)

### Features

- Maximum upload dataset size (in MB) can now be set by the
  `max_dataset_size` argument, with a default value of 200
  ([\#25](https://github.com/stevechoy/MVPapp/issues/25))

### Bugfixes

- Correcting some tooltips not being displayed properly
  ([\#24](https://github.com/stevechoy/MVPapp/issues/24))

## MVPapp 0.4.6 (2026-10-01)

### Features

- Supports server-side dataset upload to be compatible in a validated
  environment ([\#22](https://github.com/stevechoy/MVPapp/issues/22))
- QoL additions to the standalone .zip bundle
  ([\#23](https://github.com/stevechoy/MVPapp/issues/23))

### Bugfixes

- Correcting a behavior where doses with infusion rates would fail to
  display properly in individual plots
  ([\#21](https://github.com/stevechoy/MVPapp/issues/21))

## MVPapp 0.4.5 (2026-09-26)

### Features

- Overhauled the Download Model button to have all relevant information
  in a standalone .zip bundle, together with a built-in R script to
  allow full reproducibility
  ([\#19](https://github.com/stevechoy/MVPapp/issues/19))

### Documentation

- AI Translation feature had its performance evaluated and documented in
  its vignette with results available on GitHub
  ([\#20](https://github.com/stevechoy/MVPapp/issues/20))

## MVPapp 0.4.4 (2026-09-19)

### Features

- Automatic Change From Baseline (CFB) derivations for uploaded datasets
  using the DV column, per ID and CMT (using the first instance of EVID
  == 0, if present)
  ([\#18](https://github.com/stevechoy/MVPapp/issues/18))
- Separating the original character-type ID column into USUBJID and a
  numeric ID column for simulation outputs to make it more NONMEM
  friendly ([\#16](https://github.com/stevechoy/MVPapp/issues/16))

### Bugfixes

- Dosing information now correctly included in all simulation outputs
  such that outputs are NONMEM-ready
  ([\#15](https://github.com/stevechoy/MVPapp/issues/15),
  [\#17](https://github.com/stevechoy/MVPapp/issues/17))

### QoL Changes

- Extra download button for demographics for easier-access directly on
  the Demographics tab in Variability page
  ([\#14](https://github.com/stevechoy/MVPapp/issues/14))
- Updated the model code for the “2 Compartment PK/PD (Indirect Effect)”
  model to be more general
- Updated default LLM model names

## MVPapp 0.4.3 (2026-08-13)

### Features

- Supporting the use of covariate distributions (or EBEs) from uploaded
  datasets in variability simulations
  ([\#13](https://github.com/stevechoy/MVPapp/issues/13))

### Bugfixes

- Fixing an issue where summary statistics with a non-numeric ID column
  are collapsed into the same subject
  ([\#12](https://github.com/stevechoy/MVPapp/issues/12))

## MVPapp 0.4.2 (2026-08-03)

### Bugfixes

- Fixed an issue where LLM model translation fails for thinking models
  ([\#11](https://github.com/stevechoy/MVPapp/issues/11))

## MVPapp 0.4.1 (2026-03-15)

### Features

- Deterministic translation from NONMEM files using nonmem2mrgsolve and
  nonmem2rx ([\#10](https://github.com/stevechoy/MVPapp/issues/10))

## MVPapp 0.4.0 (2026-03-02)

### Features

- Automatic model translation using LLMs
  ([\#4](https://github.com/stevechoy/MVPapp/issues/4))  
- Supports Saving / Loading sessions
  ([\#5](https://github.com/stevechoy/MVPapp/issues/5))

### Bugfixes

- Minor bugfixes and general performance improvements.
  ([\#3](https://github.com/stevechoy/MVPapp/issues/3))
- Plotly compatibility check with ggplot2 \>=4.0.0
  ([\#6](https://github.com/stevechoy/MVPapp/issues/6))

## MVPapp 0.3.5 (2026-02-04)

### Bugfixes

- Dosing info now displays correctly for all doses in Individual Plots
  ([\#2](https://github.com/stevechoy/MVPapp/issues/2))

## MVPapp 0.3.4 (2025-12-02)

### Features

- Covariate histograms feature in Data Exploration
  ([\#1](https://github.com/stevechoy/MVPapp/issues/1)).

## MVPapp 0.3.3 (2025-07-31)

### Features

- NHANES database updated to include 2021-2023, which brings it to
  approx. 100k real subjects available for sampling
  ([\#1](https://github.com/stevechoy/MVPapp/issues/1))
- BMI filtering support for external databases
- Supports running app directly without installation (moved a few
  uncommon dependencies to suggests instead)

### Bugfixes

- Correct typos for tooltips
- Corrected CV% for summary statistics to always report as absolute
  values
- Applying the distinct by ID built-in filter last for uploaded datasets
  ([\#47](https://github.com/stevechoy/MVPapp/issues/47))
- Corrected behavior of range filters for filtered data display when a
  scroll bar is used
  ([\#48](https://github.com/stevechoy/MVPapp/issues/48))

## MVPapp 0.3.2 (2025-06-13)

### Features

- Spider plot functionality added for batch runs
  ([\#46](https://github.com/stevechoy/MVPapp/issues/46))

## MVPapp 0.3.1 (2025-06-12)

### Bugfixes

- Weight-based dosing now correctly apply for batch runs and variability
  simulations ([\#6](https://github.com/stevechoy/MVPapp/issues/6))

## MVPapp 0.3.0 (2025-06-09)

### Features

- Implemented Batch runs (Parameter Sensitivity Analysis Page - All
  Parameters) to assess all parameters in the model with a defined
  upper/lower range in a single click
  ([\#44](https://github.com/stevechoy/MVPapp/issues/44))
  - QoL features includes fixing parameters, reset entire table,
    multiple display scales, and other graphical options

### Bugfixes

- General performance improvements for larger models with long sampling
  ([\#43](https://github.com/stevechoy/MVPapp/issues/43))
- Stability and minor UI fixes

## MVPapp 0.2.19 (2025-06-03)

### Features

- Re-worked median line bins to be based on quantiles
  ([\#37](https://github.com/stevechoy/MVPapp/issues/37),
  [\#38](https://github.com/stevechoy/MVPapp/issues/38))
- Built-in data filtering option to distinct by ID
  ([\#40](https://github.com/stevechoy/MVPapp/issues/40))
- More QoL options for NCA (safeguards and rounding)
  ([\#41](https://github.com/stevechoy/MVPapp/issues/41))
- Minor re-factoring and other QoL updates

### Bugfixes

- Prevent non-sensible parameter estimates from crashing the entire app
  ([\#42](https://github.com/stevechoy/MVPapp/issues/42))
- Bug fixes for General Plot’s box plot count labels
  ([\#39](https://github.com/stevechoy/MVPapp/issues/39))
- Better handling of NAs for Quantize X-axis option in General Plot

## MVPapp 0.2.18 (2025-04-21)

### Features

- New option for individual plots to identify outliers
  ([\#36](https://github.com/stevechoy/MVPapp/issues/36))
- New option for individual plots to sort by one or more variables
  ([\#35](https://github.com/stevechoy/MVPapp/issues/35))
- Additional tooltips for several options
- Separate plot titles for general and individual plots
- New template model (Parallel zero/first-order absorption model)

### Bugfixes

- Fixed a bug where NCA would fail if dosing rows are not excluded by
  default ([\#33](https://github.com/stevechoy/MVPapp/issues/33))
- Safeguard for simulating too many samples, with a default value of max
  20000 samples ([\#32](https://github.com/stevechoy/MVPapp/issues/32))
- NCA is now sorted by the grouping variable, followed by the dose
  column ([\#34](https://github.com/stevechoy/MVPapp/issues/34))

## MVPapp 0.2.17 (2025-03-13)

### Features

- New option to split X-axis into quantiles for Data Page General Plots
  ([\#30](https://github.com/stevechoy/MVPapp/issues/30))
- More template models (QS TMDD, MM TMDD).

### Bugfixes

- Allows age and/or weight range sliders in Variability page to be the
  same value ([\#29](https://github.com/stevechoy/MVPapp/issues/29))
- Re-worded some tooltips to improve clarity.

## MVPapp 0.2.16 (2025-03-04)

### Features

- Implement a short delay for some inputs to avoid frequent computations
  ([\#28](https://github.com/stevechoy/MVPapp/issues/28))

### Bugfixes

- Corrected code for Michaelis-Menten example
  ([\#27](https://github.com/stevechoy/MVPapp/issues/27))

## MVPapp 0.2.15 (2025-02-26)

### Features

- Filter per column in Data Page
  ([\#26](https://github.com/stevechoy/MVPapp/issues/26))

### Bugfixes

- Handle duplicated column names in an uploaded dataset
  ([\#23](https://github.com/stevechoy/MVPapp/issues/23))
- Properly display log y-axis with same scale when dosing info is
  displayed for Individual Plots
  ([\#25](https://github.com/stevechoy/MVPapp/issues/25))

### Other

- The built-in filter for removing EVID rows now uses `EVID >= 1`,
  instead of `EVID == 1 | EVID == 4`

## MVPapp 0.2.14 (2025-02-07)

### Features

- Additional tab for Variability plots to display box plots of exposures
  ([\#22](https://github.com/stevechoy/MVPapp/issues/22))

### Bugfixes

- Minor updates to some tooltips and button placements to increase
  clarity.

## MVPapp 0.2.13 (2025-02-04)

### Features

- Dosing information, scaling, and LLOQ plotting options for Individual
  Plots ([\#18](https://github.com/stevechoy/MVPapp/issues/18),
  [\#21](https://github.com/stevechoy/MVPapp/issues/21))
- More options for dataset built-in cleaning
  ([\#20](https://github.com/stevechoy/MVPapp/issues/20))

### Bugfixes

- Checking for column validity for “Color by” option in General Plots
  ([\#19](https://github.com/stevechoy/MVPapp/issues/19))

## MVPapp 0.2.12 (2025-01-06)

### Features

- Multiple facets for General Plots
  ([\#17](https://github.com/stevechoy/MVPapp/issues/17))
- Individual plots for exploring uploaded datasets
  ([\#15](https://github.com/stevechoy/MVPapp/issues/15))  
- Mean trend toggle for variability plots
  ([\#13](https://github.com/stevechoy/MVPapp/issues/13))  
- Progress bars for longer computations where a spinner graphic is not
  feasible ([\#16](https://github.com/stevechoy/MVPapp/issues/16))

### Bugfixes

- Allow “Total Doses” for each Dosing Regimen to be 0
  ([\#14](https://github.com/stevechoy/MVPapp/issues/14))

## MVPapp 0.2.11 (2024-12-04)

### Features

- Support for uploading tab-delimited .txt files on Data Upload, with
  the option to automatically create ID column.
  ([\#9](https://github.com/stevechoy/MVPapp/issues/9))  
- Uses dose column instead of manually entering dose amount for NCA.  
- Download Options now support non-interactive plots, with a dedicated
  “Download Non-Interactive Plot” button.
  ([\#11](https://github.com/stevechoy/MVPapp/issues/11))

### Bugfixes

- NCA Clearance unit corrected to be based on the supplied time unit.  
- Linear regression is center aligned for ggplots
  ([\#10](https://github.com/stevechoy/MVPapp/issues/10))

## MVPapp 0.2.10 (2024-11-28)

### Features

- Less strict requirements for Data Exploration plots to only require
  “ID” column to be present (previously would require “ID”, “DV”, and
  “TIME”).  
- Interactive plot toggle for Simulation and Data page plots.  
- Boxplot functionality for Data Exploration - Plot Output.
  ([\#7](https://github.com/stevechoy/MVPapp/issues/7))  
- Text size option for labels (affects linear regression formulae and
  boxplot counts (N=x) only).

### Bugfixes

- Fixed some typos.  
- Most selectizeInputs now sort alphabetically.

### Other

- Suggests mrgsolve v.1.5.2 to be installed.

## MVPapp 0.2.9 (2024-10-25)

### Features

- Upgraded Data Input - Plot Output tab, supporting facets, smoothers,
  and linear regression.
  ([\#5](https://github.com/stevechoy/MVPapp/issues/5))

### Bugfixes

- Fixed display error when model does not compile for PSA plots.
  ([\#2](https://github.com/stevechoy/MVPapp/issues/2))  
- Fixed wrong behavior for median lines binning.
  ([\#4](https://github.com/stevechoy/MVPapp/issues/4))  
- Consistently include dates for all downloadable items.

## MVPapp 0.2.8 (2024-10-14)

### Features

- More template models (1 CMT lag time).  
- Increasing default decimal places (3 -\> 5) for variability
  quantiles.  
- Supports \$PRED syntax. When model has \$PRED syntax, dosing amounts
  will be disabled.  
- Using recover = TRUE by default to output more helpful error messages
  when mrgsolve does not compile.

### Bugfixes

- Bugfix for WT-based dosing to propagate correctly to Parameter
  Sensitivity Analysis when WT is a parameter (Note: currently WT-based
  dosing remains unsupported in Variability simulations).

## MVPapp 0.2.7 (2024-09-30)

- GitHub release  
- Minor bugfixes and optimizations
