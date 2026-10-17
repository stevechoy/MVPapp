## Submission

This is a new release (first submission of MVPapp to CRAN).

MVPapp bundles an interactive 'shiny' application for exploring
pharmacometrics models with 'mrgsolve'. The exported API is intentionally
small: `run_mvp()` launches the app and `mvp_app()` returns the app object
(e.g. for deployment on Posit Connect or shinyapps.io).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* Possibly misspelled words in DESCRIPTION (Pharmacometrics, pharmacometrics,
  NCA, Choy, et, al): these are correct. "Pharmacometrics" is the standard
  name of the field, NCA (non-compartmental analysis) is defined in the
  Description, and "Choy et al. (2025)" is the reference to the paper
  describing the app.
* Suggests or Enhances not in mainstream repositories: nonmem2mrgsolve
  (see below; available via `Additional_repositories`).
* Possibly invalid URL https://lagom.shinyapps.io/MVPapp/ (status 500):
  this is a live demo of the app hosted on shinyapps.io. It works in a
  browser, but the server returns an error to automated checks while the
  app is waking up from sleep.

### Notes on dependencies

* Suggests includes 'nonmem2mrgsolve', which is not on CRAN. It is available
  from the repository declared in `Additional_repositories`
  (https://stevechoy.r-universe.dev). It is only used conditionally, via
  `requireNamespace()`, to offer an optional NONMEM-to-mrgsolve translation
  feature in the app.
* The package imports a relatively large number of packages (26 non-base
  Imports). MVPapp is a full 'shiny' application (data exploration,
  simulation, sensitivity analysis, NCA, interactive tables and plots), and
  each Import is used by the app. Features that are optional (NCA reports,
  AI-assisted model translation, PDF handling) use packages in Suggests and
  are checked for with `rlang::check_installed()` / `requireNamespace()`.

## Test environments

* local: Windows 11, R 4.6.1 (2026-06-24 ucrt): 0 errors | 0 warnings | 0 notes
* GitHub Actions: windows-latest (release), macos-latest (release),
  ubuntu-latest (devel, release, oldrel-1)
* win-builder: R-devel (2026-10-16 r90686 ucrt): 0 errors | 0 warnings | 1 note

## Other notes

* Examples that launch the app are wrapped in `if (interactive())`.
  The `mvp_app()` example builds the app object without launching it.
* Tests that compile an 'mrgsolve' model are skipped on CRAN to keep check
  time short.
