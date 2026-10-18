## Submission

First submission of MVPapp to CRAN: an interactive 'shiny' app for exploring
pharmacometrics models with 'mrgsolve'. Only `run_mvp()` and `mvp_app()` are exported.

## R CMD check results

0 errors | 0 warnings | 1 note (CRAN incoming feasibility, new submission)

* Possibly misspelled words in DESCRIPTION (Pharmacometrics, pharmacometrics,
  NCA, Choy, et, al) are correct: the name of the field, a defined acronym,
  and the reference to the paper describing the app.
* Suggests not in mainstream repositories: 'nonmem2mrgsolve', declared in
  `Additional_repositories` (https://stevechoy.r-universe.dev). It is used only
  conditionally, via `requireNamespace()`, for an optional NONMEM translation feature.

## Dependencies

The package has 26 non-base Imports because it is a full 'shiny' application;
each is used by the app. Optional features (NCA reports, AI-assisted model
translation, PDF handling) use Suggests packages, checked at run time.

## Test environments

TODO (re-run on the final commit): update the results below.

* local Windows 11, R 4.6.1: 0 errors | 0 warnings | 0 notes
* GitHub Actions: windows, macOS, ubuntu (devel, release, oldrel-1): TODO confirm green
* win-builder R-devel and R-release: 0 errors | 0 warnings | 1 note (above)
* macOS builder (R 4.6.1, arm64): TODO re-run (first attempt failed at "checking
  package dependencies" because the builder lacked common CRAN packages such as
  ggplot2 and shiny)

## Other notes

* Examples that launch the app are wrapped in `if (interactive())`.
* Tests that compile an 'mrgsolve' model are skipped on CRAN. No tests or examples
  make network calls; AI-assisted translation needs a user-supplied API key.
* The app runs model code written by the user. `?run_mvp` ("Hosting and security")
  states that the authentication options are not a security boundary.
* Code in `run_mvp()` is adapted from the ShinyDemo package (GPL) by Jason Bryer,
  who is listed in `Authors@R` as `ctb` and `cph`.
