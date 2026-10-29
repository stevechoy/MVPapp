## Most of the following names are created as column names inside functions

utils::globalVariables(c("Upper", "Lower", "delta_upper", "delta_lower", "del", "Parameter", "Level", "output_pretransform", "label_text", "output", "ymax", "ymin", "xmax", "xmin", "tooltip", "amt", "tinf", "RATE", "Name", "Reference", "tags",
                         "binned_xvar", "ADDL", "AGE", "AGEMO", "AGEYR", "AUC", "AUC_ranged", "AUC_tlast", "CAVG", "CFBPCT", "CMAX", "CMIN", "CMT", "CV%", "ColumnName", "DOSETIME", "DVBL", "EVID", "HT", "ID", "II", "INFDUR", "MEANCFBPCT", "MODEL", "Max", "Mean",
                         "Median", "Min", "N", "NADIRPCT", "NAMT", "Quantile", "SAMT", "SEX", "Statistic", "TIME", "TIMEADJ", "cdc.expand", "dYVAR", "dCFB", "dosename", "maximum", "mean_yvar", "facet_label", "gMean", "input", "label", "label_text",
                         "lower05", "lower_yvar", "median_yvar", "min_yvar", "minimum", "nhanes.filtered", "who.expand", "session", "slope", "tad", "upper95", "upper_yvar", ".data", ".SD", ":=", "..count..", "XVAR", "YVAR", "YVARNAME", "YLAG",
                         "XLAG", ".", "TMAX", "TMIN", "WT", "gMean CV%", "name", "value", "splits", "LowerQ1", "LowerQ2", "LowerQ3", "UpperQ1", "UpperQ2", "UpperQ3", "Metric", "desc", "RatioN", "Ratio",
                         "BMI", "BSA", "USUBJID", "Value", "addl", "cmt", "evid", "ii", "max_dose", "max_yvar", "rate", "scaling_factor", ".label_x", ".label_y"))

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

#-------------------------------------------------------------------------------
#' @name message_object
#'
#' @title Sends the printed form of an object to the console as a message
#'
#' @description
#' For debugging output: `print()` and `dplyr::glimpse()` write to stdout, and
#' `message(glimpse(x))` prints twice. This keeps all debugging output on the
#' message stream.
#'
#' @param x any object
#'
#' @returns `NULL`, invisibly
#' @noRd
#-------------------------------------------------------------------------------

message_object <- function(x) {
  message(paste(utils::capture.output(print(x)), collapse = "\n"))
  invisible(NULL)
}

#-------------------------------------------------------------------------------
#' Sampling times of a simulation
#'
#' Sorted unique times of the observation records, so that dosing records
#' (which can fall between or after the sampling times) are not offered as
#' choices for metric time windows.
#'
#' @param sim_out simulated output with TIME (and optionally evid)
#' @noRd
#-------------------------------------------------------------------------------
sampled_times <- function(sim_out) {
  if (is.null(sim_out) || !"TIME" %in% names(sim_out)) return(numeric(0))
  if ("evid" %in% names(sim_out)) sim_out <- sim_out[sim_out$evid == 0, , drop = FALSE]
  sort(unique(sim_out$TIME))
}

#-------------------------------------------------------------------------------
#' Observation records of a simulation output
#'
#' Plots and derived metrics use observation records (evid == 0) only; the
#' downloadable simulated data keeps every record (see `keep_dose_records` in
#' `run_single_sim()`).
#'
#' @param sim_out simulated output (data frame, or NULL)
#' @noRd
#-------------------------------------------------------------------------------
obs_only <- function(sim_out) {
  if (!is.data.frame(sim_out) || !"evid" %in% names(sim_out)) return(sim_out)
  sim_out[!is.na(sim_out$evid) & sim_out$evid == 0, , drop = FALSE]
}

#-------------------------------------------------------------------------------
#' Unique suffix for compiled mrgsolve model names
#'
#' Compiling a model with the name of an existing one deletes the earlier
#' shared library, so names must not repeat across sessions (or browser tabs)
#' of one R process. Built from the Shiny session token and a process-wide
#' counter instead of the (re-seeded) random number stream.
#'
#' @param session Shiny session (or NULL)
#' @noRd
#-------------------------------------------------------------------------------
.mvp_model_counter <- new.env(parent = emptyenv())
.mvp_model_counter$n <- 0L

new_model_suffix <- function(session = NULL) {
  .mvp_model_counter$n <- .mvp_model_counter$n + 1L
  token <- tryCatch(session$token, error = function(e) NULL)
  token <- if (is.null(token)) "local" else substr(gsub("[^A-Za-z0-9]", "", token), 1, 8)
  paste0(token, "x", Sys.getpid(), "x", .mvp_model_counter$n)
}

#-------------------------------------------------------------------------------
#' Give the mcode() model name inside stored editor code a fresh unique suffix
#' (used when a session file is restored).
#'
#' @param code character, editor contents
#' @param session Shiny session
#' @noRd
#-------------------------------------------------------------------------------
rename_stored_models <- function(code, session = NULL) {
  if (is.null(code) || length(code) != 1 || is.na(code)) return(code)
  m <- regmatches(code, regexec("(mrgsolve::)?mcode\\('(Model-[12]-)[^']*'", code))[[1]]
  if (length(m) == 0) return(code)
  sub("(mrgsolve::)?mcode\\('Model-([12])-[^']*'",
      paste0("\\1mcode('Model-\\2-", new_model_suffix(session), "'"),
      code)
}

#-------------------------------------------------------------------------------
#' Dose per NCA group (subject and additional keys), taken from the first
#' positive dosing record (EVID >= 1) when the dataset has an EVID column, so
#' that an AMT column (0 on observation rows) or a dose column that varies
#' within a group does not give a wrong or mismatched dose. Falls back to the
#' first positive value on the observation rows (e.g. a DOSE column).
#'
#' A group whose dose is not found with all keys is looked up again without
#' `CMT` (doses go into a dosing compartment, observations are in another one),
#' then by subject alone (M30). A group without any positive dose gets 0, with a
#' warning, as dose-normalised NCA metrics are then not meaningful.
#'
#' @param obs_df observation records used for the NCA, in NCA order
#' @param all_df the dataset before observation filtering (may contain dosing rows)
#' @param subject_col,key_cols,dose_col column names
#' @returns numeric vector, one value per distinct (subject, keys) in `obs_df`
#' @importFrom dplyr distinct left_join filter group_by summarise across all_of first
#' @noRd
#-------------------------------------------------------------------------------
nca_dose_vector <- function(obs_df, all_df, subject_col, key_cols, dose_col) {
  # plain data frames: the dataset can be a data.table, where df[, keys] means something else
  obs_df <- as.data.frame(obs_df)
  if (!is.null(all_df)) all_df <- as.data.frame(all_df)
  keys <- c(subject_col, key_cols)
  keys <- keys[!is.na(keys) & nzchar(keys)]
  res  <- dplyr::distinct(obs_df[, keys, drop = FALSE])
  out  <- rep(NA_real_, nrow(res))

  # first positive dose per group of `by`, matched onto the NCA groups
  dose_by <- function(df, by) {
    df <- df[!is.na(df[[dose_col]]) & suppressWarnings(as.numeric(df[[dose_col]])) > 0, , drop = FALSE]
    d  <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::summarise(.dose = suppressWarnings(as.numeric(dplyr::first(.data[[dose_col]]))), .groups = "drop")
    dplyr::left_join(res[, by, drop = FALSE], d, by = by)$.dose
  }

  sources <- list()
  if (!is.null(all_df) && "EVID" %in% names(all_df) && dose_col %in% names(all_df)) {
    sources$dosing <- all_df[!is.na(all_df$EVID) & all_df$EVID >= 1, , drop = FALSE]
  }
  if (dose_col %in% names(obs_df)) sources$observations <- obs_df
  key_sets <- unique(list(keys, setdiff(keys, "CMT"), subject_col))

  for (src in sources) {
    for (by in key_sets) {
      if (!all(by %in% names(src)) || !anyNA(out)) next
      found <- dose_by(src, by)
      fill  <- is.na(out) & !is.na(found)
      out[fill] <- found[fill]
    }
  }

  if (anyNA(out)) {
    safely_showNotification(
      paste0("WARNING: No positive dose found in '", dose_col, "' for ", sum(is.na(out)), " of ", length(out),
             " NCA profiles; their dose is set to 0, so dose-normalised metrics (e.g. CL, V, CMAXD) are not meaningful. ",
             "Please check the dose column."),
      type = "warning", duration = 15, id = "nca_dose_missing")
    out[is.na(out)] <- 0
  }
  out
}
