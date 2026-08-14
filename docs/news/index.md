# Changelog

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
