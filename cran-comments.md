## Submission

This is a new release (first submission of MVPapp to CRAN).

MVPapp bundles an interactive 'shiny' application for exploring
pharmacometrics models with 'mrgsolve'. The exported API is intentionally
small: `run_mvp()` launches the app and `mvp_app()` returns the app object
(e.g. for deployment on Posit Connect or shinyapps.io).

## R CMD check results

0 errors | 0 warnings | 1 note (on a new submission, the single 'CRAN incoming
feasibility' note, which contains the points below)

* This is a new submission.
* Possibly misspelled words in DESCRIPTION (Pharmacometrics, pharmacometrics,
  NCA, Choy, et, al): these are correct. "Pharmacometrics" is the standard
  name of the field, NCA (non-compartmental analysis) is defined in the
  Description, and "Choy et al. (2025)" is the reference to the paper
  describing the app.
* Suggests or Enhances not in mainstream repositories: nonmem2mrgsolve
  (see below; available via `Additional_repositories`).
* Imports includes 26 non-default packages: MVPapp is a full 'shiny'
  application and every Import is used (see "Notes on dependencies" below).
* Possibly invalid URL https://lagom.shinyapps.io/MVPapp/ (status 500):
  this is a live demo of the app hosted on shinyapps.io. It works in a
  browser, but the server returns an error to automated checks while the
  app is waking up from sleep. TODO: delete this bullet if the latest
  win-builder / `urlchecker::url_check()` run no longer reports it.

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

TODO: re-run all of these on the final commit and update the results and dates.

* local: Windows 11, R 4.6.1 (2026-06-24 ucrt), with all Suggests installed:
  TODO: result of the final `devtools::check()`
* GitHub Actions: windows-latest (release), macos-latest (release),
  ubuntu-latest (devel, release, oldrel-1): TODO: confirm all five are green
* win-builder: R-devel (TODO: version and date) and R-release:
  TODO: result (expected: 0 errors | 0 warnings | 1 note, the new-submission note above)
* macOS builder (`check_mac_release()`): TODO result

## Other notes

* Examples that launch the app are wrapped in `if (interactive())`.
  The `mvp_app()` example builds the app object without launching it.
* Tests that compile an 'mrgsolve' model are skipped on CRAN to keep check
  time short.
* The optional AI-assisted translation needs a provider account or API key
  that the user supplies; no examples or tests make network calls, and the
  feature is only available when the Suggests packages are installed.
* The app compiles and runs model code written by the user, as any 'mrgsolve'
  session does. `?run_mvp` ("Hosting and security") documents that the
  `authentication_code` and model-password options are not a security boundary,
  so the app should not be exposed to untrusted users.
* The default favicon (`inst/favicon/`) was created for this package.
* Code in `run_mvp()` is adapted from the ShinyDemo package by Jason Bryer
  (https://github.com/jbryer/ShinyDemo). TODO: confirm how that code is
  licensed and, if it was copied rather than only inspired by it, list its
  author as `ctb`/`cph` in `Authors@R`.
