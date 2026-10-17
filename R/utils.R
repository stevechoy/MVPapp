## Most of the following names are created as column names inside functions

utils::globalVariables(c("Upper", "Lower", "delta_upper", "delta_lower", "del", "Parameter", "Level", "output_pretransform", "label_text", "output", "ymax", "ymin", "xmax", "xmin", "tooltip", "amt", "tinf", "RATE", "Name", "Reference", "tags",
                         "binned_xvar", "ADDL", "AGE", "AGEMO", "AGEYR", "AUC_ranged", "CAVG", "CFBPCT", "CMAX", "CMIN", "CMT", "CV%", "ColumnName", "DOSETIME", "DVBL", "EVID", "HT", "ID", "II", "INFDUR", "MEANCFBPCT", "MODEL", "Max", "Mean",
                         "Median", "Min", "N", "NADIRPCT", "NAMT", "Quantile", "SAMT", "SEX", "Statistic", "TIME", "TIMEADJ", "cdc.expand", "dYVAR", "dosename", "maximum", "mean_yvar", "facet_label", "gMean", "input", "label", "label_text",
                         "lower05", "lower_yvar", "median_yvar", "min_yvar", "minimum", "nhanes.filtered", "who.expand", "session", "slope", "tad", "upper95", "upper_yvar", ".data", ".SD", ":=", "..count..", "XVAR", "YVAR", "YVARNAME", "YLAG",
                         "XLAG", ".", "TMAX", "TMIN", "WT", "gMean CV%", "name", "value", "splits", "LowerQ1", "LowerQ2", "LowerQ3", "UpperQ1", "UpperQ2", "UpperQ3", "Metric", "desc", "RatioN", "Ratio",
                         "BMI", "BSA", "USUBJID", "Value", "addl", "cmt", "evid", "ii", "max_dose", "max_yvar", "rate", "scaling_factor"))

#-------------------------------------------------------------------------------
# Package-level state
#
# Used instead of assigning into the global environment (not allowed by CRAN
# policy). Objects such as the last simulated event data, and debugging output
# from the LLM helpers, are stored here. They can be inspected with e.g.
# `MVPapp:::.mvp_state$llm_result`.
#-------------------------------------------------------------------------------
.mvp_state <- new.env(parent = emptyenv())

#' Packages whose exported functions are made visible to the app and to code
#' typed into the in-app editors (later entries take precedence, mirroring the
#' order they used to be attached with library()).
#' @noRd
mvp_visible_pkgs <- c("magrittr", "rlang", "ggplot2", "shiny", "shinyBS", "dplyr", "mrgsolve")

#' Top-level environment of MVPapp (the package namespace when installed, or
#' the global environment when the R files are sourced directly).
#' @noRd
mvp_home_env <- function() {
  topenv(environment(mvp_home_env))
}

#' Build (once) an environment exposing the exports of `mvp_visible_pkgs` and
#' the package datasets, without attaching anything to the search path.
#' Its parent is the MVPapp namespace so internal helpers are also visible.
#' Objects defined by MVPapp itself take precedence over dependency exports.
#' @noRd
mvp_deps_env <- function() {
  if (!is.null(.mvp_state$deps_env)) {
    return(.mvp_state$deps_env)
  }
  home <- mvp_home_env()
  e    <- new.env(parent = home)
  for (pkg in mvp_visible_pkgs) {
    for (nm in getNamespaceExports(pkg)) {
      if (exists(nm, envir = home, inherits = FALSE)) next
      if (exists(nm, envir = e, inherits = FALSE)) rm(list = nm, envir = e)
      local({
        p <- pkg; n <- nm
        delayedAssign(n, getExportedValue(p, n), assign.env = e)
      })
    }
  }
  if (isNamespace(home)) { # lazy-loaded datasets (cdc.expand, who.expand, nhanes.filtered)
    lazydata <- getNamespaceInfo(home, "lazydata")
    for (nm in ls(lazydata, all.names = TRUE)) {
      local({
        n <- nm
        delayedAssign(n, get(n, envir = lazydata), assign.env = e)
      })
    }
  }
  .mvp_state$deps_env <- e
  e
}

#' Retrieve one of the package datasets (cdc.expand, who.expand, nhanes.filtered)
#'
#' Package functions cannot see lazy-loaded data unless the package is
#' attached, so look it up explicitly. When the R files are sourced directly
#' (runGitHub / standalone mode) the datasets are loaded into the global
#' environment instead.
#' @noRd
mvp_dataset <- function(name) {
  home <- mvp_home_env()
  if (isNamespace(home)) {
    lazydata <- getNamespaceInfo(home, "lazydata")
    if (exists(name, envir = lazydata, inherits = FALSE)) {
      return(get(name, envir = lazydata, inherits = FALSE))
    }
  }
  get(name, envir = globalenv())
}

#' Fresh environment for evaluating code typed into the app (data filtering
#' code, mrgsolve model code). Replaces relying on attached packages and on
#' global assignment.
#' @noRd
mvp_user_env <- function() {
  new.env(parent = mvp_deps_env())
}

#' Packages used by the Shiny app in inst/shiny/app.R
#'
#' These packages are only called from the app script, so they are imported
#' here to declare the dependency in the package namespace.
#' @importFrom DT DTOutput
#' @importFrom bslib bs_theme
#' @importFrom flextable flextable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shinyBS bsTooltip
#' @importFrom shinyFiles shinyFilesButton
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#' @noRd
NULL
