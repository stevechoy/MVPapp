# Reproducible simulation bundle (standalone R script export)

#-------------------------------------------------------------------------------
#' @name build_repro_bundle
#'
#' @title Exports a standalone, reproducible mrgsolve simulation bundle
#'
#' @description
#' All simulation settings (seed, sampling times, zero_re toggle, the ORIGINAL
#' pre-transformed dosing table and the transformation switches) are written into
#' simulation.R as code, and it rebuilds the event data from them using a
#' standalone copy of transform_ev_df(). The user can therefore edit everything
#' directly in simulation.R, which only needs model.cpp (and covariate_db.rds when
#' a covariate database is used) next to it. A copy of the original settings is
#' also saved to orig_settings.rds as a record; simulation.R does not need it.
#'
#' @param zip_path            Path to write the output .zip archive to
#' @param input_model_object  mrgmod object, the compiled model used for the simulation
#' @param event_data_orig     The ev() object of dosing info as returned by
#'                            generate_dosing_regimens(), i.e. BEFORE
#'                            transform_ev_df() but WITH the MW conversion already
#'                            applied to amt (the MW conversion is divided back out
#'                            here so that simulation.R can expose it). Written into
#'                            simulation.R as an editable data.frame. It must not
#'                            contain an ID column: it is the per-subject template,
#'                            which is replicated across covariate_db by simulation.R
#' @param event_data_post     The post-transformed ev() used by MVP                           
#' @param sampling_times      A vector of sampling times, passed to tgrid in
#'                            mrgsolve::mrgsim_df()
#' @param custom_sampling_time Default FALSE. Whether sampling_times came from the
#'                            user's custom time vector (input$custom_sampling_time_cb)
#'                            rather than from tend / tdelta. When TRUE, the times are
#'                            written into simulation.R in full as a vector. When FALSE,
#'                            they are written as seq(..., by = tdelta), provided that
#'                            reproduces sampling_times exactly (otherwise in full)
#' @param tend                Default NULL. Max sampling time from the app (tend()).
#'                            Only used when custom_sampling_time is FALSE
#' @param tdelta              Default NULL. Equidistant step size from the app (tdelta()).
#'                            Only used when custom_sampling_time is FALSE
#' @param mw_value            Molecular weight (g/mol). Default 1.
#' @param mw_multi_factor     Multiplication factor for MW. Default 1.
#' @param mw_checkbox         Default FALSE. The state of the MW checkbox
#'                            (input$mw_checkbox). When FALSE, no conversion is
#'                            applied by simulation.R
#' @param mw_conversion       Pre-calculated conversion factor, supplied by MVP
#' @param wt_based_dosing     Default FALSE. Same as in run_single_sim()
#' @param wt_name             Default "WT". Same as in run_single_sim()
#' @param model_dur           Default FALSE. Same as in run_single_sim()
#' @param model_rate          Default FALSE. Same as in run_single_sim()
#' @param pred_model          Default FALSE. Same as in run_single_sim()
#' @param covariate_db        Default NULL. A data.frame of covariates, keyed by ID
#'                            (1:N), replicated against the dosing table and joined
#'                            onto the event data and simulated output. When NULL,
#'                            random effects are zeroed before simulating (assumes a
#'                            single/typical-subject run); when supplied, random
#'                            effects are left as specified in the model
#' @param seed                Default 1000. Random seed set before simulation, for
#'                            reproducibility of any simulated variability
#'
#' @returns Invisibly, the path to the written .zip file (zip_path)
#'
#' @importFrom mrgsolve mwrite_cpp mread zero_re data_set carry_out mrgsim_df ev_rep as.ev
#' @importFrom dplyr left_join arrange
#' @importFrom zip zip
#' @noRd
#-------------------------------------------------------------------------------

build_repro_bundle <- function(zip_path,
                               input_model_object,
                               event_data_orig,
                               event_data_post,
                               sampling_times,
                               custom_sampling_time = FALSE,
                               tend            = NULL,
                               tdelta          = NULL,
                               mw_value        = 1,
                               mw_multi_factor = 1,
                               mw_checkbox     = FALSE,
                               mw_conversion   = 1,
                               wt_based_dosing = FALSE,
                               wt_name         = "WT",
                               model_dur       = FALSE,
                               model_rate      = FALSE,
                               pred_model      = FALSE,
                               covariate_db    = NULL,
                               seed            = 1000) {
  
  build_dir <- file.path(tempdir(), paste0("repro_", as.integer(Sys.time())))
  dir.create(build_dir)
  on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
  
  ## Project root marker -------------------------------------------------------
  #file.create(file.path(build_dir, ".here"))
  
  ## Model source --------------------------------------------------------------
  mrgsolve::mwrite_cpp(input_model_object, file = file.path(build_dir, "model.cpp"), update = FALSE)
  
  if (!is.null(covariate_db)) {
    saveRDS(covariate_db, file.path(build_dir, "covariate_db.rds"))
    zero_re <- FALSE
  } else {
    zero_re <- TRUE
  }
  
  ## Dosing + transformation settings, written into simulation.R as code -------
  # generate_dosing_regimens() already multiplied amt by the MW conversion. Divide it
  # back out so that simulation.R holds the raw dose plus an editable conversion
  mw_checkbox   <- isTRUE(mw_checkbox)
  mw_conversion <- as.numeric(mw_conversion)[1] #as.numeric(1/mw_value * mw_multi_factor)[1]
  mw_applied    <- if (mw_checkbox) mw_conversion else 1
  if (!is.finite(mw_applied) || mw_applied <= 0) {
    stop("build_repro_bundle(): mw_conversion must be a finite, positive number")
  }
  
  dosing_raw     <- as.data.frame(event_data_orig)
  dosing_raw$amt <- dosing_raw$amt / mw_applied
  if ("rate" %in% names(dosing_raw)) { # ev()'s rate is amt/tinf, so scale it in step
    has_rate <- dosing_raw$rate > 0
    dosing_raw$rate[has_rate] <- dosing_raw$rate[has_rate] / mw_applied
  }
  
  dosing_code <- repro_df_to_code(dosing_raw, "dosing")
  
  wt_based_dosing <- isTRUE(wt_based_dosing)
  wt_name         <- as.character(wt_name)[1]
  model_dur       <- isTRUE(model_dur)
  model_rate      <- isTRUE(model_rate)
  pred_model      <- isTRUE(pred_model)
  
  sampling_times <- unname(sampling_times)
  if (!is.numeric(sampling_times) || length(sampling_times) == 0L || anyNA(sampling_times)) {
    stop("build_repro_bundle(): sampling_times must be a non-empty numeric vector (not a tgrid object)")
  }
  seed <- as.numeric(seed)[1]
  
  # Numbers are written at the shortest precision that round-trips exactly, so that
  # simulation.R reproduces the app's doses and sample times bit-for-bit
  seed_code    <- paste("seed           <-", repro_num_txt(seed))
  
  custom_sampling_time <- isTRUE(custom_sampling_time)
  time_grid <- if (custom_sampling_time) NULL else repro_time_grid(sampling_times, tend, tdelta)
  
  times_code <- if (!is.null(time_grid)) {
    c("# Equidistant sampling times from the first time up to tend, in steps of tdelta",
      paste("tend           <-", repro_num_txt(time_grid$tend)),
      paste("tdelta         <-", repro_num_txt(time_grid$tdelta)),
      paste0("sampling_times <- seq(", repro_num_txt(time_grid$from), ", tend, by = tdelta)"))
  } else if (custom_sampling_time) {
    c("# Custom sampling times provided in MVP",
      repro_vec_code(sampling_times, "sampling_times"))
  } else {
    c("# Sampling times (not a regular grid, so written out in full)",
      repro_vec_code(sampling_times, "sampling_times"))
  }
  zero_re_code <- paste("zero_re        <-", deparse(zero_re))
  
  setting_code <- c(
    paste("mw_checkbox     <-", deparse(mw_checkbox)),
    paste("mw_value        <-", deparse(mw_value)),
    paste("mw_multi_factor <-", deparse(mw_multi_factor)), #repro_num_txt(mw_conversion)),
    paste("mw_conversion   <- 1/mw_value * mw_multi_factor # Only relevant when mw_checkbox = TRUE"),
    paste("wt_based_dosing <-", deparse(wt_based_dosing)),
    paste("wt_name         <-", deparse(wt_name), " # Only relevant when wt_based_dosing = TRUE and wt_name exists as a parameter"),
    paste("model_dur       <-", deparse(model_dur), " # Detected automatically - manual override possible"),
    paste("model_rate      <-", deparse(model_rate), " # Detected automatically - manual override possible"),
    paste("pred_model      <-", deparse(pred_model), " # Detected automatically - manual override possible")
  )
  
  ## Record of the ORIGINAL settings -------------------------------------------
  # simulation.R holds the live copy of every setting as code. orig_settings.rds keeps
  # what MVP originally used, so the user can always see (or restore) the starting
  # point after editing simulation.R. It is not needed by simulation.R.
  dosing_env <- new.env()
  eval(parse(text = dosing_code), envir = dosing_env)
  
  orig_settings <- list(
    seed            = seed,
    sampling_times  = sampling_times,
    custom_sampling_time = custom_sampling_time,   # TRUE: user's custom time vector; FALSE: tend / tdelta grid
    tend            = time_grid$tend,              # NULL when custom (or not a regular grid)
    tdelta          = time_grid$tdelta,            # NULL when custom (or not a regular grid)
    zero_re         = zero_re,
    dosing          = dosing_env$dosing,                # the table written into simulation.R (amt BEFORE MW conversion)
    mw_checkbox     = mw_checkbox,
    mw_value        = mw_value,
    mw_multi_factor = mw_multi_factor,
    mw_conversion   = mw_conversion,
    wt_based_dosing = wt_based_dosing,
    wt_name         = wt_name,
    model_dur       = model_dur,
    model_rate      = model_rate,
    pred_model      = pred_model,
    event_data_post = as.data.frame(event_data_post)     # Actual ev_df() used in run_single_sim
  )
  saveRDS(orig_settings, file.path(build_dir, "orig_settings.rds"))
  
  ## Environment fingerprint / file guide ---------------------------------------
  readme <- c(
    "MVPapp reproducible simulation bundle",
    "======================================",
    "",
    paste("Generated:        ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste("R version:        ", getRversion()),
    paste("mrgsolve version: ", as.character(utils::packageVersion("mrgsolve"))),
    "",
    "Description of files",
    "---------------------",
    "model.cpp          mrgsolve model source, written with mrgsolve::mwrite_cpp().",
    "                   Rebuilt with mrgsolve::mread('model.cpp') in simulation.R.",
    "",
    "orig_settings.rds  A record of the ORIGINAL settings MVP used for this run, plus:",
    "                     'dosing',          the pre-transformed dosing table from MVP",
    "                     'event_data_post'  the actual events table used by MVP",
    "",
    "                   Note: This file is *NOT* required. Every setting is written",
    "                   into simulation.R itself, so edit the values there; this file",
    "                   is only there so you can always compare with, or restore",
    "                   the original values, e.g. readRDS('orig_settings.rds')$dosing",
    "",
    if (!is.null(covariate_db)) {
      c("covariate_db.rds   Covariate table for this run. Its rows define the subjects:",
        "                   simulation.R replicates the dosing table once per ID, attaches",
        "                   these covariates, and joins them onto the simulated output.")
    } else {
      "covariate_db.rds   Not included -- no covariate database was used for this run."
    },
    "",
    "simulation.R       Standalone script that reproduces the simulation. Depends only on",
    "                   the mrgsolve package. Sources in model.cpp (and covariate_db.rds,",
    "                   if included) in the working directory. All relevant settings are ",
    "                   written into it, and easily editable.",
    "",
    "                   The 'Dosing' section of simulation.R contains the original dosing",
    "                   table (as generated by MVP before any transformation, with amt",
    "                   BEFORE MW / WT conversion). The switches MVP applies to it (MW",
    "                   conversion, weight-based dosing, modelled duration/rate, PRED",
    "                   model) will rebuild the event data from those. Edit this",
    "                   section to change the dosing.",
    "",
    "                   The 'Covariates data' section is commented out by default - ",
    "                   as this information ('covariate_db') is supplied from MVP directly.",
    "                   However, example code is provided if the user wishes to manually create ",
    "                   their own virtual patient population.",
    "",
    "                   The 'Event data' section will merge in 'covariate_db' (e.g. from ",
    "                   external databases, such as NHANES) and perform weight-based ",
    "                   dose transformations (if required). This creates an 'event_data'",
    "                   object which supplies the dataset used for simulations.",
    "",
    "                   Note: The final output object is called 'solved_output'.",
    "                   Example code is included for convenience so the user can",
    "                   export the object for use in their own workflows, if desired.",
    ""
  )
  writeLines(readme, file.path(build_dir, "README.txt"))
  
  ## The runnable script -------------------------------------------------------
  script <- c(
    "## =============================================================================",
    "## Auto-generated reproducible mrgsolve simulation from MVP.",
    "## Please see README.txt for explanations for each file.",
    "## Requires model.cpp (and covariate_db.rds, if present) in the working directory.",
    "## All settings below are the values MVP used; edit them freely. The original sims",
    "## settings are also kept for reference in orig_settings.rds (not required by this script).",
    "##",
    "## Quick start: Check that the working directory is in the same location as the script,",
    "##              and then just run the entire script (Ctrl + A, Ctrl + Enter).",
    "## =============================================================================",
    "",
    "library(mrgsolve)",
    "",
    "## Setup -----------------------------------------------------------------------",
    "",
    "mod            <- mrgsolve::mread('model.cpp')",
    "covariate_db   <- if (file.exists('covariate_db.rds')) readRDS('covariate_db.rds') else NULL",
    "orig_settings  <- if (file.exists('orig_settings.rds')) readRDS('orig_settings.rds') else NULL # Reference only - not required",
    "",
    "## Simulation settings ---------------------------------------------------------",
    "",
    seed_code,
    times_code,
    "",
    "# TRUE sets all omegas and sigmas to 0 (single/typical-subject run).",
    "# Set to FALSE to simulate with the variability specified in the model.",
    zero_re_code,
    "if (zero_re) mod <- mrgsolve::zero_re(mod)",
    "",
    "## Dosing ----------------------------------------------------------------------",
    "",
    "# Original dosing table from MVP, BEFORE any transformation. Edit it freely",
    "# (e.g. change amt, ii, addl, time, cmt, tinf, or add/remove rows).",
    "#   amt   dose amount BEFORE MW conversion (see mw_checkbox / mw_conversion below)",
    "#   addl  number of ADDITIONAL doses after the first (total doses - 1)",
    "#   ii    interdose interval",
    "#   tinf  infusion duration",
    "",
    dosing_code,
    "",
    "# Uncomment below to restore MVP's original pre-transformed dosing table:",
    "#dosing <- orig_settings$dosing",
    "",
    "# Transformations to be applied to the 'dosing' table above, if applicable, i.e.",
    "# 'amt' will be modified if mw_checkbox and/or wt_based_dosing is set to TRUE",
    "",
    setting_code,
    "",
    "# Helper function from MVPapp to transform the dose",
    "transform_ev <- function(mod, ev_df, model_dur, model_rate, pred_model,",
    "                         wt_based_dosing = FALSE, wt_name = 'WT') {",
    "  ev_df <- as.data.frame(ev_df)",
    "  ",
    "  # Weight-based dosing: per-subject weight if it is a column of the event data",
    "  # (i.e. supplied by covariate_db), otherwise the model's parameter value",
    "  if (wt_based_dosing && wt_name %in% names(mrgsolve::param(mod))) {",
    "    wt <- if (wt_name %in% names(ev_df)) ev_df[[wt_name]] else mrgsolve::param(mod)[[wt_name]]",
    "    ev_df$amt <- ev_df$amt * wt",
    "  }",
    "  ",
    "  # as.ev() re-derives 'rate' from amt/tinf, keeping it consistent after weight",
    "  # scaling or after you edit amt/tinf in the dosing table above",
    "  ev_df <- as.data.frame(mrgsolve::as.ev(ev_df))",
    "  ",
    "  if (model_dur) {  # duration modelled in the model code (D_<cmt>), cannot coexist with tinf",
    "    ev_df$rate <- -2",
    "    ev_df$tinf <- NULL",
    "  }",
    "  if (model_rate) { # rate modelled in the model code (R_<cmt>), cannot coexist with tinf",
    "    ev_df$rate <- -1",
    "    ev_df$tinf <- NULL",
    "  }",
    "  if (pred_model) { # $PRED models require CMT = 0",
    "    ev_df$cmt  <- 0",
    "    ev_df$tinf <- NULL",
    "    ev_df$rate <- NULL",
    "  }",
    "  ev_df",
    "}",
    "",
    "## Covariates data (virtual population) ----------------------------------------",
    "",
    "# Uncomment below to create your own virtual population from scratch (this replaces any",
    "# 'covariate_db' loaded above). The dosing table is replicated for every ID, the covariates",
    "# will be merged with dosing to create event_data in the next section.",
    "#",
    "# Requirements: one row per subject, an 'ID' column running 1..N,",
    "# and columns named like model parameters (e.g. WT, AGE) to override those parameters.",
    "# Note: if zero_re is TRUE (e.g. if this bundle came from a single-subject run),",
    "# you may want to set it to FALSE in the 'Simulation settings' section above ",
    "# to simulate with variability.",
    "",
    "#set.seed(seed + 10000) # Nudging the seed to have a different pattern",
    "#n_subj       <- 100 # Number of subjects to be simulated - change as you see fit",
    "#covariate_db <- data.frame(",
    "#  ID  = seq_len(n_subj),",
    "#  WT  = round(runif(n_subj, min = 40, max = 120)), # Example - change as you see fit",
    "#  AGE = round(runif(n_subj, min = 18, max = 65))   # Example - change as you see fit",
    "#)",
    "",
    "## Event data ------------------------------------------------------------------",
    "",
    "# MW conversion, applied to amt first (if applicable)",
    "dosing_transformed      <- dosing",
    "dosing_transformed$amt  <- dosing$amt * if (mw_checkbox) mw_conversion else 1",
    "",
    "if (is.null(covariate_db)) {",
    "  # Single (typical) subject",
    "  event_data <- transform_ev(mod, dosing_transformed, model_dur, model_rate, pred_model,",
    "                             wt_based_dosing, wt_name)",
    "  event_data$ID <- 1",
    "} else {",
    "  # One copy of the dosing per subject in covariate_db (IDs 1..N), with the",
    "  # covariates attached. Covariate columns named like model parameters override",
    "  # those parameters in mrgsolve, and a WT column drives weight-based dosing.",
    "",
    "  ## Helper: base-R replacement for dplyr::left_join that preserves row order of x",
    "  left_join_base <- function(x, y, by) {",
    "    x$.row_order_ <- seq_len(nrow(x))",
    "    out <- merge(x, y, by = by, all.x = TRUE, sort = FALSE)",
    "    out <- out[order(out$.row_order_), , drop = FALSE]",
    "    out$.row_order_ <- NULL",
    "    rownames(out) <- NULL",
    "    out",
    "  }",
    "",
    "  event_data <- mrgsolve::ev_rep(mrgsolve::as.ev(dosing_transformed), seq_len(nrow(covariate_db)))",
    "  event_data <- left_join_base(event_data, covariate_db, by = 'ID')",
    "  event_data <- event_data[order(event_data$ID), ]",
    "  rownames(event_data) <- NULL",
    "  event_data <- transform_ev(mod, event_data, model_dur, model_rate, pred_model,",
    "                             wt_based_dosing, wt_name)",
    "}",
    "# Uncomment below to retore MVP's original post-transformed event data:",
    "#event_data <- orig_settings$event_data_post",
    "#if (!'ID' %in% names(event_data)) event_data$ID <- 1",
    "",
    "## Model info ------------------------------------------------------------------",
    "",
    "# Please check that the parameter values are correct.",
    "# Note: If zero_re is set, all omegas and sigmas will be set to 0.",
    "mod@param",
    "mod@omega",
    "mod@sigma",
    "",
    "# Please check that the dosing info and covariates are correct.",
    "head(event_data)",
    "",
    "## Simulation ------------------------------------------------------------------",
    "",
    "# Note: seed is only relevant when simulating with variability",
    "set.seed(seed)",
    "",
    "# Additional doses (addl) are written out as separate records, as in MVP, so that the",
    "# output at each dose time has the pre-dose value (observation) and the post-dose value (dose record).",
    "event_data_sim <- event_data",
    "if ('addl' %in% names(event_data) && any(event_data$addl > 0, na.rm = TRUE)) {",
    "  event_data_sim <- as.data.frame(mrgsolve::realize_addl(event_data))",
    "}",
    "",
    "solved_output <- mod %>%",
    "  mrgsolve::data_set(event_data_sim) %>%",
    "  mrgsolve::carry_out(amt, rate, addl, ii, cmt, evid, tinf) %>%",
    "  mrgsolve::mrgsim_df(tgrid = sampling_times, tad = TRUE)",
    "",
    "# Joining covariates (i.e. external databases) if present",
    "if (!is.null(covariate_db)) {",
    "  # Skip covariates the model already outputs (e.g. via $CAPTURE)",
    "  new_cols <- c('ID', setdiff(names(covariate_db), names(solved_output)))",
    "  solved_output <- dplyr::left_join(solved_output, as.data.frame(covariate_db)[, new_cols, drop = FALSE], by = 'ID')",
    "}",
    "",
    "## Output ----------------------------------------------------------------------",
    "",
    "# time vs. the first variable captured by the model.",
    "# Set yvar yourself (e.g. yvar <- 'CP') to plot something else, and customize freely.",
    "",
    "out_vars <- mrgsolve::outvars(mod)",
    "yvar     <- intersect(c(out_vars$capture, out_vars$cmt), names(solved_output))[1]",
    "",
    "# Creating unique dose times to be visualized in the plot - each dosing record plus its",
    "# additional doses (addl, one every ii) within the simulated time range.",
    "",
    "get_col    <- function(df, nm, default) if (nm %in% names(df)) replace(df[[nm]], is.na(df[[nm]]), default) else rep(default, nrow(df))",
    "is_dose    <- get_col(event_data, 'evid', 1) %in% c(1, 4) & get_col(event_data, 'amt', 0) > 0",
    "dose_time  <- get_col(event_data, 'time', 0)[is_dose]",
    "dose_ii    <- get_col(event_data, 'ii', 0)[is_dose]",
    "dose_addl  <- get_col(event_data, 'addl', 0)[is_dose]",
    "dose_times <- unique(unlist(Map(function(t, ii, n) t + ii * (seq_len(n) - 1), dose_time, dose_ii, dose_addl + 1)))",
    "",
    "# Example plot - note that only simulated observations (EVID == 0) are shown.",
    "if (requireNamespace('ggplot2', quietly = TRUE) && !is.na(yvar)) {",
    "  plot_output <- ggplot2::ggplot(solved_output, ggplot2::aes(x = time, y = .data[[yvar]], group = ID)) +",
    "    ggplot2::geom_vline(xintercept = dose_times, linetype = 'dashed', colour = 'red') + # remove if there are too many doses",
    "    ggplot2::geom_line(alpha = 0.3) +",
    "    ggplot2::geom_point(alpha = 0.5) +",
    "    #ggplot2::scale_y_log10() +",
    "    ggplot2::labs(x = 'Time', y = yvar, caption = if (length(dose_times) > 0) 'Dashed vertical lines: dosing times') +",
    "    ggplot2::theme_bw()",
    "  print(plot_output)",
    "} else {",
    "  message('Plot skipped: needs the ggplot2 package and a captured variable or compartment to plot.')",
    "}",
    "",
    "#head(solved_output) # Data review of first few rows of the output",
    "",
    "## Save results ----------------------------------------------------------------",
    "",
    "# Uncomment to store solved_output for use in your own workflows.",
    "# Files are saved to the working directory (see getwd()).",
    "",
    "# .csv: (numbers are written to ~15 significant digits)",
    "#write.csv(solved_output, 'solved_output.csv', row.names = FALSE)",
    "#solved_output <- read.csv('solved_output.csv')   # to read it back in R",
    "",
    "# .rds: R-only, but keeps everything exactly as it is in memory (full precision, column types)",
    "#saveRDS(solved_output, 'solved_output.rds')",
    "#solved_output <- readRDS('solved_output.rds')    # to read it back in R"
  )
  writeLines(unlist(script), file.path(build_dir, "simulation.R"))
  
  ## Zip it up -----------------------------------------------------------------
  zip::zip(zipfile = zip_path, files = list.files(build_dir), root = build_dir)
  
  invisible(zip_path)
}

#-------------------------------------------------------------------------------
#' @name repro_df_to_code
#'
#' @title Internal helper: renders a data.frame as readable R code
#'
#' @description
#' Used by build_repro_bundle() to write the pre-transformed dosing table into
#' simulation.R as an editable data.frame() call (one line per column) rather
#' than as an opaque .rds file.
#'
#' @param df    A data.frame (or ev object, which is coerced)
#' @param name  Name of the object to assign to in the generated code
#'
#' @returns A character vector of R code lines
#' @noRd
#-------------------------------------------------------------------------------

repro_df_to_code <- function(df, name) {
  
  df       <- as.data.frame(df)
  col_nms  <- vapply(names(df), function(nm) deparse(as.name(nm), backtick = TRUE), character(1))
  col_vals <- vapply(df, function(x) {
    if (is.factor(x)) x <- as.character(x)
    paste(deparse(unname(x), width.cutoff = 500L), collapse = " ")
  }, character(1))
  
  cols <- paste0("  ", formatC(col_nms, width = -max(nchar(col_nms))), " = ", col_vals)
  cols <- paste0(cols, c(rep(",", length(cols) - 1L), ""))
  
  c(paste0(name, " <- data.frame("), cols, ")")
}

#-------------------------------------------------------------------------------
#' @name repro_num_txt
#'
#' @title Internal helper: numbers to text, exactly round-trippable
#'
#' @description
#' Formats each number with the fewest significant digits (15 to 17) that read
#' back as the identical double, so that values written into simulation.R are
#' bit-for-bit what MVP used, while staying as readable as possible.
#'
#' @param x A numeric vector
#'
#' @returns A character vector, one element per value
#' @noRd
#-------------------------------------------------------------------------------

repro_num_txt <- function(x) {
  vapply(as.numeric(x), function(v) {
    txt <- format(v, digits = 15)
    for (dg in 16:17) {
      if (isTRUE(as.numeric(txt) == v)) break
      txt <- format(v, digits = dg)
    }
    txt
  }, character(1), USE.NAMES = FALSE)
}

#-------------------------------------------------------------------------------
#' @name repro_vec_code
#'
#' @title Internal helper: renders a numeric vector as a literal in R code
#'
#' @description
#' Writes the vector out in full as c(...), wrapped over several lines, using the
#' exactly round-trippable number format of repro_num_txt().
#'
#' @param x     A numeric vector
#' @param name  Name of the object to assign to in the generated code
#'
#' @returns A character vector of R code lines
#' @noRd
#-------------------------------------------------------------------------------

repro_vec_code <- function(x, name) {
  
  vals   <- repro_num_txt(unname(as.numeric(x)))
  chunks <- split(vals, ceiling(seq_along(vals) / 10L))
  pad    <- strrep(" ", nchar(name) + 6L)
  
  vapply(seq_along(chunks), function(i) {
    paste0(if (i == 1L) paste0(name, " <- c(") else pad,
           paste(chunks[[i]], collapse = ", "),
           if (i == length(chunks)) ")" else ",")
  }, character(1))
}

#-------------------------------------------------------------------------------
#' @name repro_time_grid
#'
#' @title Internal helper: finds seq() parameters that reproduce a sampling grid
#'
#' @description
#' Looks for (tend, tdelta) such that seq(x1, tend, by = tdelta) returns exactly
#' the vector x. The values supplied by the app are tried first, so that simulation.R
#' shows the tend / tdelta the user actually set (which matters when tend is not a
#' multiple of tdelta); otherwise they are derived from x itself.
#'
#' @param x       Numeric vector of sampling times
#' @param tend    Optional. Max sampling time as set in the app
#' @param tdelta  Optional. Equidistant step size as set in the app
#'
#' @returns list(from, tend, tdelta), or NULL if x is not a regular grid
#' @noRd
#-------------------------------------------------------------------------------

repro_time_grid <- function(x, tend = NULL, tdelta = NULL) {
  
  x <- unname(as.numeric(x))
  n <- length(x)
  if (n < 3L) return(NULL)
  
  cands <- list()
  if (is.numeric(tend) && is.numeric(tdelta) && length(tend) == 1L && length(tdelta) == 1L &&
      is.finite(tend) && is.finite(tdelta)) {
    cands <- list(c(tend, tdelta))
  }
  derived <- unique(c(x[2] - x[1], (x[n] - x[1]) / (n - 1L)))
  cands   <- c(cands, lapply(derived, function(st) c(x[n], st)))
  
  for (cd in cands) {
    te   <- as.numeric(repro_num_txt(cd[1])) # exactly what will be read back from simulation.R
    td   <- as.numeric(repro_num_txt(cd[2]))
    cand <- tryCatch(seq(x[1], te, by = td), error = function(e) NULL)
    if (identical(cand, x)) return(list(from = x[1], tend = te, tdelta = td))
  }
  NULL
}
