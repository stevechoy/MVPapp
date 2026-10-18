# NHANES population (1999-2023)

Sex, age, body weight and height of participants in the US National
Health and Nutrition Examination Survey (NHANES), cycles 1999-2000 to
2021-2023, aged 2 years and older. Used by the app to sample age, weight
and height when the "NHANES" database is selected for variability
simulations.

## Usage

``` r
nhanes.filtered
```

## Format

A data frame with 101,400 rows and 5 columns:

- SEX:

  Sex: 0 = male, 1 = female.

- AGEMO:

  Age (months), derived from `AGEYR`.

- AGEYR:

  Age (years).

- WT:

  Body weight (kg).

- HT:

  Standing height (cm).

## Source

Centers for Disease Control and Prevention (CDC), National Center for
Health Statistics, National Health and Nutrition Examination Survey:
<https://wwwn.cdc.gov/nchs/nhanes/Default.aspx>.

## Details

Demographics and body measures files were merged by participant and only
complete records were kept. Age is recorded in whole years, so `AGEMO`
is `AGEYR * 12`. See `data-raw/databases.R` in the [GitHub
repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
for the code.

## See also

[cdc.expand](https://stevechoy.github.io/MVPapp/reference/cdc.expand.md),
[who.expand](https://stevechoy.github.io/MVPapp/reference/who.expand.md)
