# See data-raw/databases.R for more details and code.
# 'cdc.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 100k virtual subjects)
# 'who.expand'      - SEX, AGEMO, AGEYR, WT, HT (approx. 80k virtual subjects)
# 'nhanes.filtered' - SEX, AGEMO, AGEYR, WT, HT (approx. 100k real subjects)


#' WHO pediatric virtual population
#'
#' A virtual pediatric population (birth to 10 years) simulated from the World
#' Health Organization (WHO) Child Growth Standards and Growth Reference. Used
#' by the app to sample age, weight and height when the "WHO" database is
#' selected for variability simulations.
#'
#' Body weight and height were simulated from the published LMS parameters
#' (Box-Cox power, median and coefficient of variation) for each sex and month
#' of age, by drawing standard normal Z-scores and applying
#' \eqn{X = M (1 + L S Z)^{1/L}}. Sources:
#' * Weight-for-age, 0 to 10 years.
#' * Length/height-for-age, 0 to 5 years (Child Growth Standards) and 5 to
#'   10 years (Growth Reference 5-19 years).
#'
#' Rows with implausible BMI (outside 10 to 50 kg/m\eqn{^2}) were removed. See
#' `data-raw/databases.R` in the
#' [GitHub repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
#' for the code.
#'
#' @format A data frame with 77,744 rows and 6 columns:
#' \describe{
#'   \item{SEX}{Sex: 0 = male, 1 = female.}
#'   \item{AGEMO}{Age (months).}
#'   \item{AGEYR}{Age (years).}
#'   \item{WT}{Body weight (kg).}
#'   \item{HT}{Length/height (cm).}
#'   \item{BMI}{Body mass index (kg/m\eqn{^2}).}
#' }
#' @source WHO Child Growth Standards:
#'   <https://www.who.int/toolkits/child-growth-standards/standards/weight-for-age>,
#'   <https://www.who.int/toolkits/child-growth-standards/standards/length-height-for-age>.
#'   WHO Growth Reference 5-19 years:
#'   <https://www.who.int/toolkits/growth-reference-data-for-5to19-years/indicators/height-for-age>.
#' @seealso [cdc.expand], [nhanes.filtered]
"who.expand"


#' NHANES population (1999-2023)
#'
#' Sex, age, body weight and height of participants in the US National Health
#' and Nutrition Examination Survey (NHANES), cycles 1999-2000 to 2021-2023,
#' aged 2 years and older. Used by the app to sample age, weight and height
#' when the "NHANES" database is selected for variability simulations.
#'
#' Demographics and body measures files were merged by participant
#' and only complete records were kept. Age is recorded in whole years, so
#' `AGEMO` is `AGEYR * 12`. See
#' `data-raw/databases.R` in the
#' [GitHub repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
#' for the code.
#'
#' @format A data frame with 101,400 rows and 5 columns:
#' \describe{
#'   \item{SEX}{Sex: 0 = male, 1 = female.}
#'   \item{AGEMO}{Age (months), derived from `AGEYR`.}
#'   \item{AGEYR}{Age (years).}
#'   \item{WT}{Body weight (kg).}
#'   \item{HT}{Standing height (cm).}
#' }
#' @source Centers for Disease Control and Prevention (CDC), National Center for
#'   Health Statistics, National Health and Nutrition Examination Survey:
#'   <https://wwwn.cdc.gov/nchs/nhanes/Default.aspx>.
#' @seealso [cdc.expand], [who.expand]
"nhanes.filtered"


#' CDC pediatric virtual population
#'
#' A virtual pediatric population (birth to 20 years) simulated from the US
#' Centers for Disease Control and Prevention (CDC) growth charts. Used by the
#' app to sample age, weight and height when the "CDC" database is selected for
#' variability simulations.
#'
#' Body weight and stature were simulated from the published LMS parameters
#' (Box-Cox power, median and coefficient of variation) of the weight-for-age
#' and stature-for-age charts for each sex and month of age, by drawing
#' standard normal Z-scores and applying \eqn{X = M (1 + L S Z)^{1/L}}. Rows
#' with implausible BMI (outside 10 to 50 kg/m\eqn{^2}) were removed. See
#' `data-raw/databases.R` in the
#' [GitHub repository](https://github.com/stevechoy/MVPapp/tree/master/data-raw)
#' for the code.
#'
#' @format A data frame with 97,177 rows and 6 columns:
#' \describe{
#'   \item{SEX}{Sex: 0 = male, 1 = female.}
#'   \item{AGEMO}{Age (months).}
#'   \item{AGEYR}{Age (years).}
#'   \item{WT}{Body weight (kg).}
#'   \item{HT}{Length/stature (cm).}
#'   \item{BMI}{Body mass index (kg/m\eqn{^2}).}
#' }
#' @source CDC growth charts, percentile data files with LMS values:
#'   <https://www.cdc.gov/growthcharts/percentile_data_files.htm>.
#' @seealso [nhanes.filtered], [who.expand]
"cdc.expand"
