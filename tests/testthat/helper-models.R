# Small one-compartment model with a weight effect, shared by the tests.
# Compiling mrgsolve models is slow, so tests using it are skipped on CRAN.
mvp_test_model <- function() {
  code <- '
$PARAM CL = 1, V = 10, WT = 70
$CMT CENT
$ODE dxdt_CENT = -CL * pow(WT / 70, 0.75) / V * CENT;
$TABLE double CP = CENT / V;
$CAPTURE CP
'
  mrgsolve::mcode("mvp_test_model", code, quiet = TRUE)
}
