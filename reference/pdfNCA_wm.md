# Modified ncar::pdfNCA to support watermarks

Modified ncar::pdfNCA to support watermarks

## Usage

``` r
pdfNCA_wm(
  fileName = "Temp-NCA.pdf",
  concData,
  key = "Subject",
  colTime = "Time",
  colConc = "conc",
  dose = 0,
  adm = "Extravascular",
  dur = 0,
  doseUnit = "mg",
  timeUnit = "h",
  concUnit = "ug/L",
  down = "Linear",
  R2ADJ = 0,
  MW = 0,
  SS = FALSE,
  iAUC = "",
  excludeDelta = 1,
  watermark = TRUE,
  internal_version = TRUE,
  debug_msg = TRUE,
  show_progress = TRUE
)
```

## Arguments

- fileName:

  file name to save

- concData:

  concentration data table

- key:

  column names of concData to be shown in the output table

- colTime:

  column name for time

- colConc:

  column name for concentration

- dose:

  administered dose, a scalar or a vector

- adm:

  one of `"Bolus"` or `"Infusion"` or `"Extravascular"` to indicate drug
  administration mode

- dur:

  duration of infusion, a scalar or a vector

- doseUnit:

  unit of dose

- timeUnit:

  unit of time

- concUnit:

  unit of concentration

- down:

  either of `"Linear"` or `"Log"` to indicate the way to calculate AUC
  and AUMC

- R2ADJ:

  Minimum adjusted R-square value to determine terminal slope
  automatically

- MW:

  molecular weight of drug

- SS:

  if steady-state, this should be TRUE. AUCLST (AUClast) is used instead
  of AUCIFO (AUCinf) for the calculation of Vz (VZFO, VZO), CL (CLFO,
  CLO), and Vdss (VSSO).

- iAUC:

  interval AUC information in a dataframe with "Name", "Start", and
  "End" columns

- excludeDelta:

  Improvement of R2ADJ larger than this value could exclude the last
  point. Default value 1 is for the compatibility with other software.
  Author recommends to use `excludeDelta` option with about 0.3.

- watermark:

  Insert watermark when TRUE

- internal_version:

  changes temp dir pathing as a workaround for cloud hosting where
  access rights prevent writing

- debug_msg:

  show debug messages

- show_progress:

  display progress messages
