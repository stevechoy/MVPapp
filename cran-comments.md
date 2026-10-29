## Resubmission

This is a resubmission. In response to the review:

* Writing to the user's home filespace: the internal NCA PDF report function
  (`pdfNCA_wm()`) no longer has a default file name; the app always passes the
  path of the Shiny download (a temporary file). A test that built a ggplot grob
  without an open device wrote `Rplots.pdf` into the tests folder; it now uses
  `pdf(NULL)`. All other files are written to `tempdir()`.
* Changing options, par or the working directory: `pdfNCA_wm()` no longer calls
  `setwd()`, and resets `options(width)` and the graphical parameters with an
  immediate `on.exit()`; the page helper no longer sets options. A test that
  changed the working directory now uses `source(chdir = TRUE)`. New tests check
  that the working directory, options, graphical parameters and active device
  are unchanged after the report is written.
* The app itself (`inst/shiny/app.R`) sets `scipen`, `DT.options` and
  `shiny.maxRequestSize` and the random seed when it starts; `on.exit()` cannot
  be used for a running 'shiny' app, so they are restored with
  `shiny::onStop()` when the app stops, including when it stops with an error.

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

* local Windows 11, R 4.6.1: 0 errors | 0 warnings | 0 notes
* GitHub Actions: windows, macOS, ubuntu (devel, release, oldrel-1): all passing
* win-builder R-devel and R-release: 0 errors | 0 warnings | 1 note (above)

## Other notes

* Examples that launch the app are wrapped in `if (interactive())`.
* Tests that compile an 'mrgsolve' model are skipped on CRAN. No tests or examples
  make network calls; AI-assisted translation needs a user-supplied API key.
* The app runs model code written by the user. `?run_mvp` ("Hosting and security")
  states that the authentication options are not a security boundary.
* Code in `run_mvp()` is adapted from the ShinyDemo package (GPL) by Jason Bryer,
  who is listed in `Authors@R` as `ctb` and `cph`.
