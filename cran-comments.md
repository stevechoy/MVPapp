## Submission

This is a new release (first submission of MVPapp to CRAN).

MVPapp bundles an interactive 'shiny' application for exploring
pharmacometric models with 'mrgsolve'. The exported API is intentionally
small: `run_mvp()` launches the app and `mvp_app()` returns the app object
(e.g. for deployment on Posit Connect or shinyapps.io).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

### Notes on dependencies

* Suggests includes 'nonmem2mrgsolve', which is not on CRAN. It is available
  from the repository declared in `Additional_repositories`
  (https://stevechoy.r-universe.dev). It is only used conditionally, via
  `requireNamespace()`, to offer an optional NONMEM-to-mrgsolve translation
  feature in the app.
* The package imports a relatively large number of packages (31 non-base
  Imports). MVPapp is a full 'shiny' application (data exploration,
  simulation, sensitivity analysis, NCA, interactive tables and plots), and
  each Import is used by the app. Features that are optional (NCA reports,
  AI-assisted model translation, PDF handling) use packages in Suggests and
  are checked for with `rlang::check_installed()` / `requireNamespace()`.

## Test environments

* local: Windows 11, R x.y.z (TO FILL IN)
* GitHub Actions: windows-latest (release), macos-latest (release),
  ubuntu-latest (devel, release, oldrel-1)
* win-builder: R-devel (TO FILL IN)

## Other notes

* Examples that launch the app are wrapped in `if (interactive())`.
  The `mvp_app()` example builds the app object without launching it.
* Tests that compile an 'mrgsolve' model are skipped on CRAN to keep check
  time short.
