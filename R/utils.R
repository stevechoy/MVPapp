## Most of the following names are created as column names inside functions

utils::globalVariables(c("Upper", "Lower", "delta_upper", "delta_lower", "del", "Parameter", "Level", "output_pretransform", "label_text", "output", "ymax", "ymin", "xmax", "xmin", "tooltip", "amt", "tinf", "RATE", "Name", "Reference", "tags",
                         "binned_xvar", "ADDL", "AGE", "AGEMO", "AGEYR", "AUC", "AUC_ranged", "AUC_tlast", "CAVG", "CFBPCT", "CMAX", "CMIN", "CMT", "CV%", "ColumnName", "DOSETIME", "DVBL", "EVID", "HT", "ID", "II", "INFDUR", "MEANCFBPCT", "MODEL", "Max", "Mean",
                         "Median", "Min", "N", "NADIRPCT", "NAMT", "Quantile", "SAMT", "SEX", "Statistic", "TIME", "TIMEADJ", "cdc.expand", "dYVAR", "dCFB", "dosename", "maximum", "mean_yvar", "facet_label", "gMean", "input", "label", "label_text",
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

#' Home environment of MVPapp: the package namespace when installed, or the
#' environment the R files were sourced into (app launched from the source,
#' e.g. via shiny::runGitHub()).
#' @noRd
mvp_home_env <- function() {
  environment(mvp_home_env)
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
#' (app launched from the source) the datasets are loaded next to them.
#' @noRd
mvp_dataset <- function(name) {
  home <- mvp_home_env()
  if (isNamespace(home)) {
    lazydata <- getNamespaceInfo(home, "lazydata")
    if (exists(name, envir = lazydata, inherits = FALSE)) {
      return(get(name, envir = lazydata, inherits = FALSE))
    }
  }
  get(name, envir = home)
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
#' @importFrom stats as.formula coef lm
#' @noRd
NULL

#-------------------------------------------------------------------------------
#' @name mvp_safely
#'
#' @title Wrap a function so that it captures errors instead of throwing them
#'
#' @description Minimal replacement for `purrr::safely()`.
#'
#' @param .f A function.
#'
#' @returns A function returning a list with `result` (the value of `.f`, or
#'   `NULL` on error) and `error` (the error condition, or `NULL`).
#' @noRd
#-------------------------------------------------------------------------------
mvp_safely <- function(.f) {
  force(.f)
  function(...) {
    tryCatch(list(result = .f(...), error = NULL),
             error = function(e) list(result = NULL, error = e))
  }
}

#-------------------------------------------------------------------------------
#' @name fct_inorder
#'
#' @title Factor with levels in order of first appearance
#'
#' @description Minimal replacement for `forcats::fct_inorder()`.
#'
#' @param x A vector or factor.
#'
#' @returns A factor.
#' @noRd
#-------------------------------------------------------------------------------
fct_inorder <- function(x) {
  factor(x, levels = unique(as.character(x[!is.na(x)])))
}

#-------------------------------------------------------------------------------
#' @name fct_relevel
#'
#' @title Move factor levels to the front
#'
#' @description Minimal replacement for `forcats::fct_relevel()`: converts `x`
#'   to a factor and moves the levels given in `...` (those present) to the
#'   front, in that order.
#'
#' @param x A vector or factor.
#' @param ... Character vectors of levels to move to the front.
#'
#' @returns A factor.
#' @noRd
#-------------------------------------------------------------------------------
fct_relevel <- function(x, ...) {
  x     <- as.factor(x)
  front <- intersect(unique(c(...)), levels(x))
  factor(x, levels = c(front, setdiff(levels(x), front)))
}

#-------------------------------------------------------------------------------
#' @name mvp_version
#'
#' @title Version of the running MVPapp
#'
#' @description Returns the installed package version, or, when the app is
#'   launched from source without installing MVPapp (e.g. via
#'   `shiny::runGitHub()`), the version in that source's DESCRIPTION, which
#'   `inst/shiny/app.R` stores as `.mvp_source_version`.
#'
#' @returns A character string, e.g. "1.0.0".
#' @noRd
#-------------------------------------------------------------------------------
mvp_version <- function() {
  v <- get0(".mvp_source_version", envir = environment(mvp_version), inherits = FALSE)
  if (!is.null(v)) return(v)
  tryCatch(as.character(utils::packageVersion("MVPapp")), error = function(e) "unknown")
}
