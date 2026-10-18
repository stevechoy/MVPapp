# CDC pediatric virtual population

A virtual pediatric population (birth to 20 years) simulated from the US
Centers for Disease Control and Prevention (CDC) growth charts. Used by
the app to sample age, weight and height when the "CDC" database is
selected for variability simulations.

## Usage

``` r
cdc.expand
```

## Format

A data frame with 97,177 rows and 6 columns:

- SEX:

  Sex: 0 = male, 1 = female.

- AGEMO:

  Age (months).

- AGEYR:

  Age (years).

- WT:

  Body weight (kg).

- HT:

  Length/stature (cm).

- BMI:

  Body mass index (kg/m\\^2\\).

## Source

CDC growth charts, percentile data files with LMS values:
<https://www.cdc.gov/growthcharts/percentile_data_files.htm>.

## Details

Body weight and stature were simulated from the published LMS parameters
(Box-Cox power, median and coefficient of variation) of the
weight-for-age and stature-for-age charts for each sex and month of age,
by drawing standard normal Z-scores and applying \\X = M (1 + L S
Z)^{1/L}\\. Rows with implausible BMI (outside 10 to 50 kg/m\\^2\\) were
removed. See `data-raw/databases.R` in the [GitHub
repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
for the code.

## See also

[nhanes.filtered](https://stevechoy.github.io/MVPapp/reference/nhanes.filtered.md),
[who.expand](https://stevechoy.github.io/MVPapp/reference/who.expand.md)
