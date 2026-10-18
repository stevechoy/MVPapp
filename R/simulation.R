# Model compilation, dosing regimens and simulation

#-------------------------------------------------------------------------------
#' @name realize_dose_records
#'
#' @title Write additional doses (ADDL) out as separate dosing records
#'
#' @description
#' With every dose as its own record, the simulated output has two rows at each
#' dose time: the observation (pre-dose value) followed by the dose record
#' (post-dose value). Without this, an observation at an additional dose time
#' holds the post-dose value and the pre-dose (trough) value is lost.
#'
#' @param ev_data A dosing data frame (or mrgsolve event object)
#'
#' @returns A data frame with addl = 0 for every dosing record
#' @noRd
#-------------------------------------------------------------------------------
realize_dose_records <- function(ev_data) {
  ev_data  <- as.data.frame(ev_data)
  addl_col <- intersect(c("addl", "ADDL"), names(ev_data))
  if (length(addl_col) == 0 || !any(ev_data[[addl_col[1]]] > 0, na.rm = TRUE)) {
    return(ev_data)
  }
  as.data.frame(mrgsolve::realize_addl(ev_data))
}

#-------------------------------------------------------------------------------
#' @name safely_mrgsim_df
#'
#' @title Error-capturing (safely) wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::mrgsim_df`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::mrgsim_df`
#' @noRd
#-------------------------------------------------------------------------------

safely_mrgsim_df <- mvp_safely(mrgsolve::mrgsim_df)

#-------------------------------------------------------------------------------
#' @name safely_mcode
#'
#' @title Error-capturing (safely) wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::mcode`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::mrgcode`
#' @noRd
#-------------------------------------------------------------------------------

safely_mcode <- mvp_safely(mrgsolve::mcode)

#-------------------------------------------------------------------------------
#' @name run_single_sim
#'
#' @title Executes a mrgsim call based on user input
#'
#' @param input_model_object    mrgmod object
#' @param pred_model            Default FALSE. set to TRUE to set ev_df CMT to 0
#' @param ev_df                 ev() dataframe containing dosing info
#' @param wt_based_dosing       When TRUE, will try to use weight-based dosing
#' @param wt_name               Name of weight parameter in the model to multiply dose amount by
#' @param model_dur             Default FALSE, set to TRUE to model duration inside the code
#' @param model_rate            Default FALSE, set to TRUE to model rate inside the code
#' @param sampling_times        A vector of sampling times (note: not a tgrid object)
#' @param seed                  Seed number for RNG for reproducibility
#' @param divide_by             Divide the TIME by this value, used for scaling x-axis
#' @param debug                 Default FALSE, set to TRUE to show more messages in console
#' @param nsubj_offset          Default 0, uses this value +1 to start ID count
#' @param append_id_text        A string prefix to be inserted for each ID to become their USUBJID
#' @param ext_db                Default NULL, supply R object of external database
#' @param show_matches          Default FALSE, shows a notification when external databases found a matching name with model parameters
#' @param event_store           Optional environment. With `save_events_as`, the event table is also stored here
#'                              (as `event_store[[save_events_as]]`), so each app session keeps its own copy
#' @param save_events_as        Optional label (e.g. "model_1"). When supplied, the event table used
#'                              for this simulation is kept for that model's reproducible bundle download
#' @param keep_dose_records     Default FALSE: only observation records (evid == 0) are returned, which is
#'                              what plots and metrics use. TRUE also keeps the dose records at sampled
#'                              times (for the downloadable simulated data); filter with `obs_only()` before
#'                              plotting or summarising
#'
#' @returns a df
#'
#' @importFrom dplyr mutate select rename relocate
#' @importFrom data.table merge.data.table fwrite fread
#' @importFrom shiny showNotification
#' @noRd
#-------------------------------------------------------------------------------

run_single_sim <- function(input_model_object,
                           pred_model      = FALSE,
                           ev_df,
                           wt_based_dosing = FALSE,
                           wt_name         = "WT",
                           model_dur       = FALSE,
                           model_rate      = FALSE,
                           sampling_times,
                           seed            = 1000,
                           divide_by       = 1,
                           debug           = FALSE,
                           nsubj_offset    = 0,
                           append_id_text  = "m1-",
                           ext_db          = NULL,
                           show_matches    = FALSE,
                           save_events_as  = NULL,
                           event_store     = NULL,
                           keep_dose_records = FALSE) {
  
  if (is.null(input_model_object)) {
    if (debug) message("input_model_object is NULL")
    return(NULL)
  }
  
  # Population run whenever a covariate database is given (even for a single
  # subject, which keeps variability and the sampled covariates)
  use_db <- !is.null(ext_db) && nrow(ext_db) > 0
  
  if (use_db) {
    # One copy of the dosing per subject, with that subject's covariates
    ev_df <- data.table::merge.data.table(mrgsolve::ev_rep(ev_df, seq_len(nrow(ext_db))), ext_db, by = "ID", all.x = TRUE)
  }
  ev_df <- transform_ev_df(input_model_object, ev_df, model_dur, model_rate, pred_model, debug,
                           wt_based_dosing = wt_based_dosing, wt_name = wt_name)
  
  if (use_db) {
    ext_db <- dplyr::mutate(ext_db, ID = ID + nsubj_offset)
    ev_df  <- dplyr::mutate(ev_df,  ID = ID + nsubj_offset)
    
    if (show_matches) {
      matching_names <- intersect(names(mrgsolve::param(input_model_object)), names(ext_db))
      if (length(matching_names) > 0) {
        shiny::showNotification(paste0(paste(matching_names, collapse = ", "),
                                       " from the model are replaced by the custom distributions."),
                                type = "message", duration = 10)
      }
    }
    
    input_model_object <- mrgsolve::update(input_model_object, digits = 5)
    sim_data <- realize_dose_records(ev_df)
    set.seed(seed)
  } else {
    # Single typical subject (no variability). Model 2 uses ID 2, and offsets
    # (batch runs) keep the IDs of several runs apart
    input_model_object <- mrgsolve::zero_re(input_model_object)
    id <- if (append_id_text == "m2-") 2 else 1
    sim_data <- dplyr::mutate(realize_dose_records(ev_df), ID = id + nsubj_offset)
  }
  
  solved_output <- input_model_object %>%
    mrgsolve::data_set(sim_data) %>%
    mrgsolve::carry_out(amt, rate, addl, ii, cmt, evid, tinf) %>%
    safely_mrgsim_df(tgrid = sampling_times, tad = TRUE)
  
  ## Keep the events used, per model, for that model's reproducible bundle download
  if (!is.null(save_events_as)) {
    .mvp_state$last_ev_df[[save_events_as]] <- ev_df # last one in the process, for debugging
    if (is.environment(event_store)) {
      event_store[[save_events_as]] <- ev_df           # this session's own copy
    }
  }
  
  if (!is.null(solved_output$error)) {
    shiny::showNotification(
      paste0(solved_output$error, " Potentially due to non-sensible parameter values."),
      type = "error", duration = 10)
    return(NULL)
  }
  out <- solved_output$result
  
  if (use_db) {
    # Add the covariates the model doesn't already output (e.g. via $CAPTURE WT;
    # merging both would give WT.x / WT.y)
    new_cols <- c("ID", setdiff(names(ext_db), names(out)))
    if (length(new_cols) > 1) {
      out <- data.table::merge.data.table(data.table::as.data.table(out), dplyr::select(ext_db, dplyr::all_of(new_cols)),
                                          by = "ID", all.x = TRUE)
    }
  }
  
  out <- out %>%
    dplyr::rename(TIME = time) %>%
    dplyr::mutate(TIMEADJ = TIME / divide_by,
                  USUBJID = as.factor(paste0(append_id_text, ID))) %>%
    dplyr::relocate(USUBJID, .before = ID)
  
  # Plots and metrics use observation records only. Dose records are kept
  # (keep_dose_records = TRUE) just for the downloadable simulated data, and
  # only at sampled times: records written out by realize_dose_records() can
  # fall after the last sampling time (e.g. 12 doses q12h with a 72 h
  # maximum) and would extend the time range with values never sampled
  if (keep_dose_records) {
    t_samp <- round(mrgsolve::stime(sampling_times), 6)
    dplyr::filter(out, .data$evid == 0 | round(.data$TIME, 6) %in% t_samp)
  } else {
    dplyr::filter(out, .data$evid == 0)
  }
}

#-------------------------------------------------------------------------------
#' @name sample_age_wt
#' @title Samples from existing databases
#' @description
#' Sampling from the loaded objects `nhanes.filtered`, `who.expand`, `cdc.expand`,
#' or no sampling at all ("None").
#' @param df_name        Name of databases, either "None", "CDC", "WHO", or "NHANES"
#' @param nsubj          Number of subjects
#' @param lower.agemo    Min Age (months) *not applicable for "None"*
#' @param upper.agemo    Max Age (months) *not applicable for "None"*
#' @param lower.wt       Min WT (kg)      *not applicable for "None"*
#' @param upper.wt       Max WT (kg)      *not applicable for "None"*
#' @param lower.bmi      Min BMI          *not applicable for "None"*
#' @param upper.bmi      Max BMI          *not applicable for "None"*
#' @param prop.male      Proportion of males (e.g. 0.5)  *not applicable for "None"*
#' @param seed.number    seed number
#' @returns a dataframe with nsubj number of rows
#' @importFrom dplyr slice_sample arrange filter
#' @noRd
#-------------------------------------------------------------------------------

sample_age_wt <- function(df_name     = "None",
                          nsubj       = 20,
                          lower.agemo = 18 * 12,
                          upper.agemo = 65 * 12,
                          lower.wt    = 0,
                          upper.wt    = 200,
                          lower.bmi   = 0,
                          upper.bmi   = 70,
                          prop.male   = 0.5,
                          seed.number = 1234) {

  set.seed(seed.number)

  if(df_name == "None") {
    df.combined <- dplyr::tibble(ID = 1:nsubj)
    return(df.combined)
  }

  df <- switch(EXPR = df_name,
               "CDC"    = mvp_dataset("cdc.expand"),
               "WHO"    = mvp_dataset("who.expand"),
               "NHANES" = mvp_dataset("nhanes.filtered")
  )

  if(upper.agemo > max(df$AGEMO)) {
    stop("Requested upper bound of age exceeds what's available in the database.")
  }

  if(lower.agemo < min(df$AGEMO)) {
    stop("Requested lower bound of age exceeds what's available in the database.")
  }
  
  if(lower.agemo == upper.agemo) { # Allows singular age
    df.sexes <- df %>%
      dplyr::filter(AGEMO == lower.agemo)

    if(nrow(df.sexes) == 0) { # CDC does not have exact whole months available
      df.sexes <- df %>% # Note we're using inclusive both ends to be more accurate of what the user wants
        dplyr::filter(AGEMO >= (lower.agemo - 0.5), AGEMO <= (upper.agemo + 0.5))
    }
  } else {
    df.sexes <- df %>%
      dplyr::filter(AGEMO >= lower.agemo, AGEMO <= upper.agemo)
  }
  
  df.sexes <- df.sexes %>%
    dplyr::mutate(BMI = round(WT / (HT/100)^2,2)) # Check for non-sensible values
  

  if(lower.wt == upper.wt) { # Allows singular weight
    df.sexes <- df.sexes %>%
      dplyr::filter(abs(WT - lower.wt) <= 0.5) # +/- 0.5 kg, as for single ages (L55)
  } else {
    df.sexes <- df.sexes %>%
      dplyr::filter(WT >= lower.wt, WT <= upper.wt)
  }
  
  if(lower.bmi == upper.bmi) { # Allows singular BMI
    df.sexes <- df.sexes %>%
      dplyr::filter(abs(BMI - lower.bmi) <= 0.5) # +/- 0.5 kg/m2 (L55)
  } else {
    df.sexes <- df.sexes %>%
      dplyr::filter(BMI >= lower.bmi, BMI <= upper.bmi)
  }

  # Without enough matching subjects, slice_sample() silently returns fewer rows,
  # which previously led to duplicated subjects (or a cryptic error) below
  # The two counts add up to nsubj exactly (ceiling() on both could draw one
  # extra subject, which was then dropped at random, shifting the sex ratio)
  n_male_needed   <- round(nsubj * prop.male)
  n_female_needed <- nsubj - n_male_needed
  n_male_avail    <- sum(df.sexes$SEX == 0)
  n_female_avail  <- sum(df.sexes$SEX == 1)
  if (n_male_avail < n_male_needed || n_female_avail < n_female_needed) {
    stop("Only ", n_male_avail, " males and ", n_female_avail, " females in the ", df_name,
         " database match the selected age, weight and BMI ranges, but ", nsubj,
         " subjects (", n_male_needed, " males, ", n_female_needed, " females) were requested. ",
         "Please reduce the number of subjects or widen the ranges.", call. = FALSE)
  }

  df.boys <- df.sexes %>%
    dplyr::filter(SEX == 0) %>%
    dplyr::slice_sample(n = n_male_needed, replace = FALSE)

  df.girls <- df.sexes %>%
    dplyr::filter(SEX == 1) %>%
    dplyr::slice_sample(n = n_female_needed, replace = FALSE)

  df.combined <- rbind(df.boys, df.girls) %>%
    dplyr::slice_sample(n = nsubj, replace = FALSE) %>%
    dplyr::arrange(AGEMO) %>%
    dplyr::rename(AGE = AGEYR) %>%
    dplyr::mutate(BSA = round(0.20247 * WT^0.425 * (HT/100)^0.725,2)) %>% # Du Bois formula for BSA, height in m
    dplyr::select(SEX, AGEMO, AGE, WT, HT, BMI, BSA)

  return(cbind(dplyr::tibble(ID = 1:nsubj), df.combined))
}

#-------------------------------------------------------------------------------
#' @name extract_model_params
#'
#' @title Function to extract params from model object
#'
#' @param input_model_object Expects a mrgsolve model object
#'
#' @returns a dataframe of all names inside $PARAM where each row contains
#' a parameter name in column 1, and parameter value in column 2
#'
#' @note
#' Should be a tibble of 2 columns and X rows where X = number of params
#' @importFrom tidyr pivot_longer everything
#' @rawNamespace import(mrgsolve, except = c(req))
#' @noRd
#-------------------------------------------------------------------------------

extract_model_params <- function(input_model_object) {
  tmp_df <- as.data.frame(mrgsolve::param(input_model_object)) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = tidyr::everything())
  return(tmp_df)
}

#-------------------------------------------------------------------------------
#' @name update_model_object
#' @title Function to update model object
#'
#' @param input_model_object  original model object
#' @param input_new_df        dataframe containing new name/value combinations
#' @param convert_colnames    When TRUE, rename input_new_df column names
#'
#' @returns a mrgsolve model object
#' @noRd
#-------------------------------------------------------------------------------

update_model_object <- function(input_model_object, input_new_df, convert_colnames = FALSE) {
  tmp1 <- input_new_df

  if(convert_colnames) names(tmp1) <- c("name", "value")

  param_list <- as.list(setNames(tmp1[["value"]], tmp1[["name"]]))

  mrgsolve::param(input_model_object, param_list)
  #return(new_mod)
}

#-------------------------------------------------------------------------------
#' @name generate_dosing_regimens
#' @title Generate Dosing Regimens
#'
#' @description
#' This is the main function to supply dosing regimens to simulations.
#' If there are no valid dose amounts from any dosing regimens, a dummy mrgsolve::ev()
#' object is created to supply a dose of 0 to be used for simulations. Note that
#' weight-based dosing is handled later inside transform_ev_df()
#'
#' @param regimens         list of dosing regimens, each a list with amt, delay_time, cmt,
#'                        tinf, total and ii (see dosing_regimens_from_input())
#' @param mw_conversion MW conversion value
#' @param create_dummy_ev Default TRUE, to create dummy ev if there are no valid
#' dose amounts, set to FALSE to not create one
#' @param debug set to TRUE to show debug messages
#'
#' @importFrom mrgsolve ev as.ev
#' @importFrom dplyr arrange filter if_else
#' @returns A mrgsolve::ev event object
#' @noRd

#-------------------------------------------------------------------------------

generate_dosing_regimens <- function(regimens,
                                     mw_conversion   = 1,
                                     create_dummy_ev = TRUE,
                                     debug           = FALSE) {

  one_regimen <- function(r) {
    total <- sanitize_numeric_input(r$total, allow_zero = TRUE, as_integer = TRUE)
    # mrgsolve::ev() does not allow 0 doses: these become one dose of amount 0,
    # which is dropped below
    mrgsolve::ev(amt   = if (total <= 0) 0 else sanitize_numeric_input(r$amt) * mw_conversion,
                 time  = sanitize_numeric_input(r$delay_time),
                 cmt   = r$cmt,
                 tinf  = sanitize_numeric_input(r$tinf),
                 total = sanitize_numeric_input(r$total, allow_zero = FALSE, as_integer = TRUE),
                 ii    = sanitize_numeric_input(r$ii, allow_zero = FALSE))
  }

  total_doses <- do.call(c, lapply(regimens, one_regimen)) %>%
    as.data.frame() %>%
    dplyr::arrange(time) %>%
    dplyr::filter(amt > 0) %>%
    mrgsolve::as.ev()

  # No dose at all: a dose of 0 so that the model can still be simulated
  if (create_dummy_ev && nrow(total_doses) == 0) {
    total_doses <- mrgsolve::ev(amt = 0, time = 0, cmt = regimens[[1]]$cmt, tinf = 0, total = 1, ii = 0)
  }

  if (debug) {
    message("Dosing regimen generated")
    message_object(total_doses)
  }

  total_doses
}

#-------------------------------------------------------------------------------
#' @name dosing_regimens_from_input
#'
#' @title The five dosing regimen rows of a model, read from the app inputs
#'
#' @param input  Shiny input
#' @param model  1 or 2. Model 2's input ids end in "_2" (e.g. amt1_2)
#'
#' @returns a list of 5 lists (amt, delay_time, cmt, tinf, total, ii), for
#'   generate_dosing_regimens()
#' @noRd
#-------------------------------------------------------------------------------
dosing_regimens_from_input <- function(input, model = 1) {
  sfx <- if (model == 2) "_2" else ""
  lapply(1:5, function(i) {
    list(amt        = input[[paste0("amt", i, sfx)]],
         delay_time = input[[paste0("delay_time", i, sfx)]],
         cmt        = input[[paste0("cmt", i, "_model_", model)]],
         tinf       = input[[paste0("tinf", i, sfx)]],
         total      = input[[paste0("total", i, sfx)]],
         ii         = input[[paste0("ii", i, sfx)]])
  })
}

#-------------------------------------------------------------------------------
#' @name expand_addl_ii
#' @title Expand ADDL and II dosing rows
#'
#' @description
#' Expands ADDL and II dosing rows. If there are no ADDL and II columns, return
#' original dataframe unchanged. Missing ADDL values are treated as 0 (single dose).
#'
#' @param data The dataframe used for expansion
#' @param x_axis The x_axis variable, usually TIME or TAFD etc
#' @param dose_col The dose variable, usually AMT or DOSE
#' @param debug Show debugging messages
#'
#' @returns a dataframe
#' @importFrom dplyr arrange select all_of
#' @noRd
#-------------------------------------------------------------------------------

expand_addl_ii <- function(data, x_axis, dose_col, debug = FALSE) {

  data$DOSETIME <- as.numeric(as.character(data[[x_axis]])) # also created when there is no ADDL / II

  if (all(c("ADDL", "II", "EVID", x_axis, dose_col) %in% colnames(data))) {
    addl <- as.integer(data$ADDL)
    ii   <- as.numeric(data$II)

    # Each dosing row is repeated 1 + ADDL times (missing ADDL or II: a single
    # dose), the k-th copy given k intervals after the first
    n_copies <- 1L + ifelse(!is.na(addl) & addl > 0 & !is.na(ii), addl, 0L)
    rows     <- rep(seq_len(nrow(data)), n_copies)
    k        <- sequence(n_copies) - 1L

    data <- data[rows, , drop = FALSE]
    data$DOSETIME <- data$DOSETIME + k * ifelse(is.na(ii[rows]), 0, ii[rows])
    data$ADDL <- NA
    data$II   <- NA
    data <- dplyr::arrange(data, ID, DOSETIME)
    if (debug) message("expand_addl_ii: ", nrow(data), " dosing rows after expansion")
  }

  # NAMT, SAMT, min_yvar are previously calculated
  keep_cols <- c("ID", "facet_label", "DOSETIME", x_axis, dose_col, "NAMT", "SAMT", "min_yvar")
  if ("RATE" %in% colnames(data)) {
    data$RATE <- as.numeric(as.character(data$RATE))
    keep_cols <- c(keep_cols, "RATE")
  }

  dplyr::select(data, dplyr::all_of(unique(keep_cols)))
}

#-------------------------------------------------------------------------------
#' @name trim_columns
#' @title Trim columns to only retain essential columns for plotting
#'
#' @description
#' Useful to cut down datasets when they are large and unwieldy
#'
#' @param data The dataframe used for trimming
#' @param x_axis X-axis column name used for plotting
#' @param y_axis Y-axis column name used for plotting
#' @param color (optional) Color by column
#' @param sort_by (optional) A list of columns to sort by
#' @param strat_by (optional) A variable to flag outliers by
#' @param type_of_plot "general_plot", "ind_plot", "sim_plot"
#' @param facet_name (optional) column(s) to facet by
#' @param insert_med_line (optional) insert median line
#' @param med_line_by (optional) The median line to insert, insert_med_line must be TRUE
#' @param ind_dose_colname (optional) Individual plot dose column name
#' @param highlight_var (optional) highlight variable column name
#' @param lloq_colname (optional) LLOQ column name
#'
#' @returns a dataframe
#' @importFrom dplyr select all_of filter mutate arrange rename rowwise ungroup
#' @noRd
#-------------------------------------------------------------------------------

trim_columns <- function(data,
                         x_axis,
                         y_axis,
                         color = "",
                         sort_by = NULL,
                         strat_by = "",
                         type_of_plot,
                         facet_name = NULL,
                         insert_med_line = FALSE,
                         med_line_by = "",
                         ind_dose_colname = "",
                         highlight_var = "",
                         lloq_colname = "") {

  # Required columns that are needed to make the plots. Optional columns are added afterwards
  essential_columns <- c("ID",
                         x_axis,
                         y_axis
  )

  if(!is.na(color) && color != "") {
    essential_columns <- c(essential_columns, color)
  }

  if("CMT" %in% colnames(data)) {
    essential_columns <- c(essential_columns, "CMT")
  }

  if(type_of_plot == "general_plot" | type_of_plot == "sim_plot") {

    if(type_of_plot == "general_plot") {
      if(!is.null(facet_name[1]) && facet_name[1] != "") {
        essential_columns <- c(essential_columns, facet_name)
      }
    }

    if(insert_med_line && med_line_by != '') {
      essential_columns <- c(essential_columns, med_line_by)
    }

  } # end of "general_plot" or "sim_plot"

  if(type_of_plot == "ind_plot") {
    if("EVID" %in% colnames(data)) {
      essential_columns <- c(essential_columns, "EVID")
    }

    if(ind_dose_colname != "") {
      essential_columns <- c(essential_columns, ind_dose_colname)
    }

    if(length(sort_by) > 0) {
      essential_columns <- c(essential_columns, unlist(sort_by))
    }

    if(!is.na(strat_by) && strat_by != '') {
      essential_columns <- c(essential_columns, strat_by)
    }

    if(all(c("ADDL", "II") %in% colnames(data))) {
      if(any(!is.na(data$ADDL))) { # Checks if there are any populated ADDL values despite the column is present
        essential_columns <- c(essential_columns, "ADDL", "II")
      }
    }

    if("RATE" %in% colnames(data)) {
      if(any(subset(data, !is.na(RATE))$RATE > 0)) {
        essential_columns <- c(essential_columns, "RATE")
      }
    }

    if(highlight_var != "") {
      essential_columns <- c(essential_columns, highlight_var)
    }

    if(lloq_colname != "") {
      essential_columns <- c(essential_columns, lloq_colname)
    }
  } # End of "ind_plot" check

  # Creates dataset used to plot and dropping any non-unique columns
  data_to_plot <- data %>%
    dplyr::select(dplyr::all_of(unique(essential_columns)))

  return(data_to_plot)

}

#-------------------------------------------------------------------------------
#' @name handle_blanks
#'
#' @title Function that turns blank values into ".(blanks)"
#' group
#'
#' @param df              Input dataframe
#' @param column_name     Name of column (must be character type) to check for blanks
#'
#' @importFrom dplyr mutate sym
#' @returns a dataframe with the blank values converted into ".(blanks)
#' @noRd
#-------------------------------------------------------------------------------

handle_blanks <- function(df, column_name) {
  if (is.character(df[[column_name]]) && any(df[[column_name]] == "", na.rm = TRUE)) {
    shiny::showNotification(paste0("WARNING: Some ", column_name, " values are blank. Renamed to '.(blank)'."), type = "warning", duration = 10)
    df <- df %>% dplyr::mutate(!!column_name := ifelse(!is.na(!!dplyr::sym(column_name)) & !!dplyr::sym(column_name) == "", ".(blank)", !!dplyr::sym(column_name)))
  }
  return(df)
}

#-------------------------------------------------------------------------------
#' @name transform_ev_df
#'
#' @title Helper function to handle ev_df transformations
#' group
#'
#' @param mod             mrgsolve model object
#' @param ev_df           Input event dataframe
#' @param model_dur       When TRUE, models duration
#' @param model_rate      When TRUE, models rate
#' @param pred_model      When TRUE, the model is a PRED model
#' @param debug           When TRUE, outputs debugging messages
#' @param wt_based_dosing When TRUE, tries to apply weight-based dosing
#' @param wt_name         Name of weight parameter in the model
#'
#' @importFrom dplyr mutate select any_of sym
#' @importFrom mrgsolve as.ev
#' @returns a cleaned event dataframe
#' @noRd
#-------------------------------------------------------------------------------

# Helper function to handle ev_df transformations
transform_ev_df <- function(mod, ev_df, model_dur, model_rate, pred_model, debug = FALSE,
                            wt_based_dosing = FALSE, wt_name = "WT") {

  if (wt_based_dosing && wt_name %in% names(mrgsolve::param(mod))) {
    ev_df <- as.data.frame(ev_df)
    # Each subject's own weight when the events carry it (covariate database),
    # otherwise the model's parameter value
    wt <- if (wt_name %in% names(ev_df)) ev_df[[wt_name]] else mrgsolve::param(mod)[[wt_name]]
    ev_df$amt <- ev_df$amt * wt
    ev_df <- mrgsolve::as.ev(ev_df)
  }

  # Infusion duration (rate = -2) or rate (rate = -1) set in the model code;
  # either replaces tinf
  if (model_dur || model_rate) {
    if (debug) message("Applying ", if (model_dur) "model_dur" else "model_rate", " transformation")
    ev_df <- as.data.frame(ev_df)
    ev_df$rate <- if (model_dur) -2 else -1
    ev_df$tinf <- NULL
    ev_df <- mrgsolve::as.ev(ev_df)
  }

  if (pred_model) { # $PRED models need all CMTs set to 0
    if (debug) message("Applying pred_model transformation")
    ev_df <- ev_df %>%
      dplyr::mutate(cmt = 0) %>%
      dplyr::select(-dplyr::any_of(c("tinf", "rate")))
  }

  ev_df
}

#-------------------------------------------------------------------------------
#' @name iterate_batch_runs
#'
#' @title Perform multiple simulations from a dataframe containing parameters
#'
#' @param batch_run_df          Input batch run dataframe containing "Name", "Reference", "Lower", "Upper"
#' @param input_model_object    mrgmod object
#' @param pred_model            Default FALSE. set to TRUE to set ev_df CMT to 0
#' @param wt_based_dosing       When TRUE, will try to use weight-based dosing
#' @param wt_name               Name of weight parameter in model to multiply dose amt by
#' @param ev_df                 ev() dataframe containing dosing info
#' @param model_dur             Default FALSE, set to TRUE to model duration inside the code
#' @param model_rate            Default FALSE, set to TRUE to model rate inside the code
#' @param sampling_times        A vector of sampling times (note: not a tgrid object)
#' @param divide_by             Divide the TIME by this value, used for scaling x-axis
#' @param debug                 Default FALSE, set to TRUE to show more messages in console
#' @param show_progress         When TRUE, shows shiny progress messages
#' @param gradient              When TRUE, perform gradient runs in between lower/upper
#'
#' @importFrom dplyr mutate rename select bind_rows across rowwise ungroup 
#' @importFrom shiny withProgress setProgress
#' @returns a df mrgsolve output totaling 2 * params + 1 (reference) runs, or 8 * params + 1 if gradient option is used
#' @noRd
#-------------------------------------------------------------------------------

iterate_batch_runs <- function(batch_run_df,
                               input_model_object,
                               pred_model         = FALSE,
                               wt_based_dosing    = FALSE,
                               wt_name            = "WT",
                               ev_df,
                               model_dur          = FALSE,
                               model_rate         = FALSE,
                               sampling_times,
                               divide_by          = 1,
                               debug              = FALSE,
                               show_progress      = TRUE,
                               gradient           = FALSE) {
  
  if (nrow(batch_run_df) == 0) return(NULL)
  
  batch_run_df <- dplyr::mutate(batch_run_df, dplyr::across(-Name, as.numeric))
  
  # With gradient = TRUE, three values evenly spaced between each bound and the
  # reference are also run (columns LowerQ1-3 and UpperQ1-3)
  if (gradient) {
    splits <- Map(split_reference_value, batch_run_df$Reference, batch_run_df$Lower, batch_run_df$Upper)
    for (q in 1:3) {
      batch_run_df[[paste0("LowerQ", q)]] <- vapply(splits, function(s) s$lower_splits[q], numeric(1))
      batch_run_df[[paste0("UpperQ", q)]] <- vapply(splits, function(s) s$upper_splits[q], numeric(1))
    }
  }
  bound_cols <- if (gradient) {
    c("Lower", "LowerQ1", "LowerQ2", "LowerQ3", "UpperQ1", "UpperQ2", "UpperQ3", "Upper")
  } else {
    c("Lower", "Upper")
  }
  
  # One run per parameter and bound, after the reference run. Each run changes
  # a single parameter and has its own ID (offset), so that the runs can be
  # combined into one table
  base_params <- dplyr::select(batch_run_df, Name, Reference)
  runs <- list(list(desc = "Reference", cat = "Reference", param = "Reference", value = -99, ratio = 1,
                    id_text = "ref", params = base_params))
  for (col in bound_cols) {
    direction <- if (startsWith(col, "Lower")) "lower" else "upper"
    suffix    <- sub("^Lower|^Upper", "", col) # "" or "Q1" - "Q3"
    shown     <- if (suffix == "") "" else paste0(" (", suffix, ")")
    for (i in seq_len(nrow(batch_run_df))) {
      name   <- batch_run_df$Name[i]
      value  <- batch_run_df[[col]][i]
      ref    <- batch_run_df$Reference[i]
      params <- base_params
      params$Reference[i] <- value
      runs[[length(runs) + 1]] <- list(
        desc    = paste0(direction, " ", name, shown),
        cat     = paste0(tools::toTitleCase(direction), shown),
        param   = name,
        value   = value,
        ratio   = if (isTRUE(ref != 0)) value / ref else NA_real_, # undefined for a reference of 0 (L54)
        id_text = paste0(direction, name, suffix),
        params  = params)
    }
  }
  
  run_all <- function() {
    lapply(seq_along(runs), function(k) {
      r <- runs[[k]]
      if (show_progress) shiny::setProgress(value = k / length(runs), detail = paste0(k, "/", length(runs)))
      if (debug) message("Trying Batch run: ", r$desc)
      
      out <- run_single_sim(
        input_model_object = update_model_object(input_model_object, r$params, convert_colnames = TRUE),
        wt_based_dosing    = wt_based_dosing,
        wt_name            = wt_name,
        pred_model         = pred_model,
        ev_df              = ev_df,
        model_dur          = model_dur,
        model_rate         = model_rate,
        sampling_times     = sampling_times,
        divide_by          = divide_by,
        debug              = debug,
        append_id_text     = r$id_text,
        nsubj_offset       = k - 1
      )
      if (!is.data.frame(out)) return(NULL)
      
      out$.desc      <- r$desc
      out$.cat       <- r$cat
      out$.paramname <- r$param
      out$.value     <- r$value
      out$.ratio     <- r$ratio
      out
    })
  }
  
  # withProgress() needs a running Shiny session, so only use it when asked to
  results <- if (show_progress) shiny::withProgress(message = "Batch Run", value = 0, run_all()) else run_all()
  dplyr::bind_rows(results)
}

#-------------------------------------------------------------------------------
#' @name update_batch_run_table
#'
#' @title Update batch run parameter table
#'
#' @param param_df              Input dataframe containing mrgsolve parameter names and values ("Name", "Reference", "Lower", "Upper")
#' @param lower_multiplier      lower bound multiplier
#' @param upper_multiplier      upper bound multiplier
#' @param last_change_ref_index Row index if last change through the UI was a reference value, otherwise 0
#' @param length_lower_change   Length of lower bound changes
#' @param length_upper_change   Length of upper bound changes
#'
#' @importFrom dplyr mutate across everything
#' @returns a df containing mrgsolve parameter names and values ("reference", "lower", "upper") as characters
#' @noRd
#-------------------------------------------------------------------------------

update_batch_run_table <- function(param_df,
                                   lower_multiplier      = 0.5,
                                   upper_multiplier      = 1.5,
                                   last_change_ref_index = 0,
                                   length_lower_change   = 0,
                                   length_upper_change   = 0
) {

  table <- dplyr::mutate(param_df, dplyr::across(-Name, as.numeric))
  if (anyNA(table)) {
    return(table)
  }

  # A bound edited by hand is kept as it is. Otherwise the bounds are
  # Reference x multiplier: for the edited row only after a Reference edit, and
  # for every row after a multiplier change. (A multiplier change therefore
  # also resets bounds edited earlier, so bounds are best edited last.)
  bound_edited <- (length_lower_change == 1 && length_upper_change == 0) ||
                  (length_lower_change == 0 && length_upper_change == 1)
  if (!bound_edited) {
    lower <- sanitize_numeric_input(lower_multiplier, allow_zero = FALSE, return_value = 0.5, display_error = TRUE)
    upper <- sanitize_numeric_input(upper_multiplier, allow_zero = FALSE, return_value = 1.5, display_error = TRUE)
    # (switching models can give more than one index; then all rows are updated)
    rows  <- if (length(last_change_ref_index) == 1 && last_change_ref_index > 0) last_change_ref_index else seq_len(nrow(table))
    table$Lower[rows] <- table$Reference[rows] * lower
    table$Upper[rows] <- table$Reference[rows] * upper
  }

  # Character columns, so that every update is a type change that re-triggers
  # the table's reactive in the app
  dplyr::mutate(table, dplyr::across(dplyr::everything(), as.character))
}
