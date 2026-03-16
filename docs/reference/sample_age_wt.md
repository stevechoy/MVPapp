# Samples from existing databases

Sampling from the loaded objects `nhanes.filtered`, `who.expand`,
`cdc.expand`, or no sampling at all ("None").

## Usage

``` r
sample_age_wt(
  df_name = "None",
  nsubj = 20,
  lower.agemo = 18 * 12,
  upper.agemo = 65 * 12,
  lower.wt = 0,
  upper.wt = 200,
  lower.bmi = 0,
  upper.bmi = 70,
  prop.male = 0.5,
  seed.number = 1234
)
```

## Arguments

- df_name:

  Name of databases, either "None", "CDC", "WHO", or "NHANES"

- nsubj:

  Number of subjects

- lower.agemo:

  Min Age (months) *not applicable for "None"*

- upper.agemo:

  Max Age (months) *not applicable for "None"*

- lower.wt:

  Min WT (kg) *not applicable for "None"*

- upper.wt:

  Max WT (kg) *not applicable for "None"*

- lower.bmi:

  Min BMI *not applicable for "None"*

- upper.bmi:

  Max BMI *not applicable for "None"*

- prop.male:

  Proportion of males (e.g. 0.5) *not applicable for "None"*

- seed.number:

  seed number

## Value

a dataframe with nsubj number of rows
