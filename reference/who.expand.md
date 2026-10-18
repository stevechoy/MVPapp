# WHO pediatric virtual population

A virtual pediatric population (birth to 10 years) simulated from the
World Health Organization (WHO) Child Growth Standards and Growth
Reference. Used by the app to sample age, weight and height when the
"WHO" database is selected for variability simulations.

## Usage

``` r
who.expand
```

## Format

A data frame with 77,744 rows and 6 columns:

- SEX:

  Sex: 0 = male, 1 = female.

- AGEMO:

  Age (months).

- AGEYR:

  Age (years).

- WT:

  Body weight (kg).

- HT:

  Length/height (cm).

- BMI:

  Body mass index (kg/m\\^2\\).

## Source

WHO Child Growth Standards:
<https://www.who.int/toolkits/child-growth-standards/standards/weight-for-age>,
<https://www.who.int/toolkits/child-growth-standards/standards/length-height-for-age>.
WHO Growth Reference 5-19 years:
<https://www.who.int/toolkits/growth-reference-data-for-5to19-years/indicators/height-for-age>.

## Details

Body weight and height were simulated from the published LMS parameters
(Box-Cox power, median and coefficient of variation) for each sex and
month of age, by drawing standard normal Z-scores and applying \\X = M
(1 + L S Z)^{1/L}\\. Sources:

- Weight-for-age, 0 to 10 years.

- Length/height-for-age, 0 to 5 years (Child Growth Standards) and 5 to
  10 years (Growth Reference 5-19 years).

Rows with implausible BMI (outside 10 to 50 kg/m\\^2\\) were removed.
See `data-raw/databases.R` in the [GitHub
repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
for the code.

## See also

[cdc.expand](https://stevechoy.github.io/MVPapp/reference/cdc.expand.md),
[nhanes.filtered](https://stevechoy.github.io/MVPapp/reference/nhanes.filtered.md)
