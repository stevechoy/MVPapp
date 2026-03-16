# Modified NonCompart::tblNCA to support progress bars

Modified NonCompart::tblNCA to support progress bars

## Usage

``` r
tblNCA_progress(
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
  show_progress = TRUE
)
```

## Arguments

- concData:

  concentration data table

- key:

  column names of concData to be shown in the output table

- colTime:

  column name for time

- colConc:

  column name for concentration

- dose:

  administered dose

- adm:

  one of `"Bolus"` or `"Infusion"` or `"Extravascular"` to indicate drug
  administration mode

- dur:

  duration of infusion

- doseUnit:

  unit of dose

- timeUnit:

  unit of time

- concUnit:

  unit of concentration

- down:

  method to calculate AUC, `"Linear"` or `"Log"`

- R2ADJ:

  Lowest threshold of adjusted R-square value to do manual slope
  determination

- MW:

  molecular weight of drug

- SS:

  if steady-state, this should be TRUE. AUCLST (AUClast) is used instead
  of AUCIFO (AUCinf) for the calculation of Vz (VZFO, VZO), CL (CLFO,
  CLO), and Vdss (VSSO).

- iAUC:

  data.frame for interval AUC

- excludeDelta:

  Improvement of R2ADJ larger than this value could exclude the last
  point. Default value 1 is for the compatibility with other software.

- show_progress:

  display progress messages
