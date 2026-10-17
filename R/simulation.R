# Model compilation, dosing regimens and simulation

#-------------------------------------------------------------------------------
#' @name safely_qsim
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::qsim`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::qsim`
#' @noRd
#-------------------------------------------------------------------------------

safely_qsim <- purrr::safely(mrgsolve::qsim)

#-------------------------------------------------------------------------------
#' @name safely_mrgsim_df
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::mrgsim_df`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::mrgsim_df`
#' @noRd
#-------------------------------------------------------------------------------

safely_mrgsim_df <- purrr::safely(mrgsolve::mrgsim_df)

#-------------------------------------------------------------------------------
#' @name safely_mcode
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `mrgsolve::mcode`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `mrgsolve::mrgcode`
#' @noRd
#-------------------------------------------------------------------------------

safely_mcode <- purrr::safely(mrgsolve::mcode)

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
#' @param nsubj                 Default 1, in which case mrgsolve::zero_re() will be applied
#' @param nsubj_offset          Default 0, uses this value +1 to start ID count
#' @param append_id_text        A string prefix to be inserted for each ID to become their USUBJID
#' @param ext_db                Default NULL, supply R object of external database
#' @param show_matches          Default FALSE, shows a notification when external databases found a matching name with model parameters
#' @param parallel_sim          Default TRUE, uses the future and mrgsim.parallel packages !Not implemented live!
#' @param parallel_n            The number of subjects required before parallelization is used !Not implemented live!
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
                           nsubj           = 1,
                           nsubj_offset    = 0,
                           append_id_text  = "m1-",
                           ext_db          = NULL,
                           show_matches    = FALSE,
                           parallel_sim    = FALSE,
                           parallel_n      = 200) {
  
  if (is.null(input_model_object)) {
    if (debug) message("input_model_object is NULL")
    return(NULL)
  }
  
  ev_df_orig <- ev_df # Pre-transformed ev() dataframe
  
  if (nsubj > 1 && !is.null(ext_db)) {
    
    ext_db_ev_prewt <- data.table::merge.data.table(
      mrgsolve::ev_rep(ev_df, 1:nrow(ext_db)),
      ext_db, by = "ID", all.x = TRUE
    )
    
    ext_db_ev <- transform_ev_df(
      input_model_object, ext_db_ev_prewt,
      model_dur, model_rate, pred_model, debug,
      wt_based_dosing = wt_based_dosing,
      wt_name         = wt_name
    )
    
    if(nsubj_offset > 0) {
      ext_db <- ext_db %>%
        mutate(ID = ID + nsubj_offset)
      ext_db_ev <- ext_db_ev %>%
        mutate(ID = ID + nsubj_offset)
    }   
    
    input_model_object <- mrgsolve::update(input_model_object, digits = 5)
    
    if(show_matches) {
      
      mod_param_names <- names(mrgsolve::param(input_model_object))
      ext_db_names    <- names(ext_db)
      matching_names  <- intersect(mod_param_names, ext_db_names)
      
      if (length(matching_names) > 0) {
        
        shiny::showNotification(
          paste0(
            paste(matching_names, collapse = ", "),
            " from the model are replaced by the custom distributions."
          ),
          type = "message",
          duration = 10
        )
      }
    }
    
    set.seed(seed)
    
    # solved_output <- safely_qsim(
    #   input_model_object,
    #   data    = ext_db_ev,
    #   obsonly = FALSE,
    #   tgrid   = sampling_times,
    #   tad     = TRUE,
    #   output  = "df"
    # )
    
    solved_output <- input_model_object %>%
      mrgsolve::data_set(as.data.frame(ext_db_ev)) %>%
      mrgsolve::carry_out(amt, rate, addl, ii, cmt, evid, tinf) %>%
      safely_mrgsim_df(#events = ev_df,
        tgrid  = sampling_times,
        tad    = TRUE)
    
    if (is.null(solved_output$error))
      solved_output$result <- data.table::merge.data.table(
        solved_output$result, ext_db, by = c("ID"), all.x = TRUE
      )
    
    ev_df <- ext_db_ev
    
  } else {
    # Covers nsubj <= 1 AND the nsubj > 1 / ext_db NULL edge case ──
    if (nsubj > 1 && is.null(ext_db)) {
      if (debug) message("nsubj > 1 but ext_db is NULL - falling back to single-subject sim")
    }
    
    ev_df <- transform_ev_df(
      input_model_object, ev_df,
      model_dur, model_rate, pred_model, debug,
      wt_based_dosing = wt_based_dosing,
      wt_name         = wt_name
    )
    
    id_num <- dplyr::case_when(append_id_text == "m1-"  ~ 1 + nsubj_offset,
                               append_id_text == "m2-"  ~ 2 + nsubj_offset,
                               TRUE                     ~ 1 + nsubj_offset) # handles iterate_batch_runs()
    
    solved_output <- input_model_object %>%
      mrgsolve::data_set(as.data.frame(ev_df) %>% dplyr::mutate(ID = id_num)) %>%
      mrgsolve::carry_out(amt, rate, addl, ii, cmt, evid, tinf) %>%
      #mrgsolve::obsonly() %>%
      mrgsolve::zero_re() %>%
      safely_mrgsim_df(#events = ev_df,
                       tgrid  = sampling_times,
                       tad    = TRUE)
  }
  
  # ── Shared post-processing ────────────────────────────────────────────────
  if (is.null(solved_output$error)) {
    solved_output <- solved_output$result %>%
      dplyr::rename(TIME    = time) %>%
      dplyr::mutate(TIMEADJ = TIME / divide_by,
                    USUBJID = as.factor(paste0(append_id_text, ID))) %>%
      dplyr::relocate(USUBJID, .before = ID)
  } else {
    shiny::showNotification(
      paste0(solved_output$error, " Potentially due to non-sensible parameter values."),
      type = "error", duration = 10)
    solved_output <- NULL
  }
  
  ## Globally save simulation data for easy export
  #last_input_model_object <<- input_model_object
  .mvp_state$last_ev_df      <- ev_df
  .mvp_state$last_ev_df_orig <- ev_df_orig
  #last_sampling_times     <<- sampling_times
  #last_ext_db             <<- ext_db
  #last_seed               <<- seed
  
  return(solved_output)
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
      dplyr::filter(AGEMO >= lower.agemo, AGEMO < upper.agemo)
  }
  
  df.sexes <- df.sexes %>%
    dplyr::mutate(BMI = round(WT / (HT/100)^2,2)) # Check for non-sensible values
  
  message(names(df.sexes))

  if(lower.wt == upper.wt) { # Allows singular weight
    df.sexes <- df.sexes %>%
      dplyr::filter(WT == lower.wt)
  } else {
    df.sexes <- df.sexes %>%
      dplyr::filter(WT >= lower.wt, WT < upper.wt)
  }
  
  if(lower.bmi == upper.bmi) { # Allows singular BMI
    df.sexes <- df.sexes %>%
      dplyr::filter(BMI == lower.bmi)
  } else {
    df.sexes <- df.sexes %>%
      dplyr::filter(BMI >= lower.bmi, BMI < upper.bmi)
  }

  df.boys <- df.sexes %>%
    dplyr::filter(SEX == 0) %>%
    dplyr::slice_sample(n = ceiling(nsubj * prop.male), replace = FALSE)

  df.girls <- df.sexes %>%
    dplyr::filter(SEX == 1) %>%
    dplyr::slice_sample(n = ceiling(nsubj * (1 - prop.male)), replace = FALSE)

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
#' @param amt1 Dose Amount Regimen 1
#' @param delay_time1 Delay Time Regimen 1
#' @param cmt1 Input CMT Regimen 1
#' @param tinf1 Infusion Time Regimen 1
#' @param total1 Total Doses Regimen 1
#' @param ii1 Interdose Interval Regimen 1
#' @param amt2 Dose Amount Regimen 2
#' @param delay_time2 Delay Time Regimen 2
#' @param cmt2 Input CMT Regimen 2
#' @param tinf2 Infusion Time Regimen 2
#' @param total2 Total Doses Regimen 2
#' @param ii2 Interdose Interval Regimen 2
#' @param amt3 Dose Amount Regimen 3
#' @param delay_time3 Delay Time Regimen 3
#' @param cmt3 Input CMT Regimen 3
#' @param tinf3 Infusion Time Regimen 3
#' @param total3 Total Doses Regimen 3
#' @param ii3 Interdose Interval Regimen 3
#' @param amt4 Dose Amount Regimen 4
#' @param delay_time4 Delay Time Regimen 4
#' @param cmt4 Input CMT Regimen 4
#' @param tinf4 Infusion Time Regimen 4
#' @param total4 Total Doses Regimen 4
#' @param ii4 Interdose Interval Regimen 4
#' @param amt5 Dose Amount Regimen 5
#' @param delay_time5 Delay Time Regimen 5
#' @param cmt5 Input CMT Regimen 5
#' @param tinf5 Infusion Time Regimen 5
#' @param total5 Total Doses Regimen 5
#' @param ii5 Interdose Interval Regimen 5
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

generate_dosing_regimens <- function(amt1, delay_time1, cmt1, tinf1, total1, ii1,
                                     amt2, delay_time2, cmt2, tinf2, total2, ii2,
                                     amt3, delay_time3, cmt3, tinf3, total3, ii3,
                                     amt4, delay_time4, cmt4, tinf4, total4, ii4,
                                     amt5, delay_time5, cmt5, tinf5, total5, ii5,
                                     mw_conversion = 1,
                                     create_dummy_ev = TRUE,
                                     debug = FALSE
) {

  # Special handling of amt to treat a user-input of dose number 0 to insert a dummy amount of 0,
  # As mrgsolve::ev does not allow zero doses

  dosing_scheme_1 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total1, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt1) * mw_conversion),
                                  time    =  sanitize_numeric_input(delay_time1),
                                  cmt     =  cmt1,
                                  tinf    =  sanitize_numeric_input(tinf1),
                                  total   =  sanitize_numeric_input(total1, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii1, allow_zero = FALSE)
  )
  dosing_scheme_2 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total2, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt2) * mw_conversion),
                                  time    =  sanitize_numeric_input(delay_time2),
                                  cmt     =  cmt2,
                                  tinf    =  sanitize_numeric_input(tinf2),
                                  total   =  sanitize_numeric_input(total2, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii2, allow_zero = FALSE)
  )
  dosing_scheme_3 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total3, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt3) * mw_conversion),
                                  time    =  sanitize_numeric_input(delay_time3),
                                  cmt     =  cmt3,
                                  tinf    =  sanitize_numeric_input(tinf3),
                                  total   =  sanitize_numeric_input(total3, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii3, allow_zero = FALSE)
  )
  dosing_scheme_4 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total4, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt4) * mw_conversion),
                                  time    =  sanitize_numeric_input(delay_time4),
                                  cmt     =  cmt4,
                                  tinf    =  sanitize_numeric_input(tinf4),
                                  total   =  sanitize_numeric_input(total4, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii4, allow_zero = FALSE)
  )
  dosing_scheme_5 <- mrgsolve::ev(amt     =  dplyr::if_else(sanitize_numeric_input(total5, allow_zero = TRUE, as_integer = TRUE) <= 0,
                                                            0,
                                                            sanitize_numeric_input(amt5) * mw_conversion),
                                  time    =  sanitize_numeric_input(delay_time5),
                                  cmt     =  cmt5,
                                  tinf    =  sanitize_numeric_input(tinf5),
                                  total   =  sanitize_numeric_input(total5, allow_zero = FALSE, as_integer = TRUE),
                                  ii      =  sanitize_numeric_input(ii5, allow_zero = FALSE)
  )

  total_doses <- c(dosing_scheme_1, dosing_scheme_2, dosing_scheme_3,
                   dosing_scheme_4, dosing_scheme_5) %>%
    as.data.frame() %>%
    dplyr::arrange(time) %>%
    dplyr::filter(amt > 0) %>%
    mrgsolve::as.ev()

  if(create_dummy_ev) {
    if(nrow(total_doses) == 0) {
      total_doses <- mrgsolve::ev(amt   = 0,
                                  time  = 0,
                                  cmt   = cmt1,
                                  tinf  = 0,
                                  total = 1,
                                  ii    = 0)

    }
  }

  if(debug) {
    message("Dosing regimen generated")
    message(paste(utils::capture.output(print(total_doses)), collapse = "\n"))
  }

  return(total_doses)
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
#' @importFrom dplyr filter mutate arrange rename rowwise ungroup select bind_rows sym
#' @importFrom tidyr unnest
#' @noRd
#-------------------------------------------------------------------------------

expand_addl_ii <- function(data, x_axis, dose_col, debug = FALSE) {
  
  data$DOSETIME <- as.numeric(as.character(data[[x_axis]])) # Still create DOSETIME if no ADDL II is found
  
  # Check if necessary columns are present
  if (all(c("ADDL", "II", "EVID", x_axis, dose_col) %in% colnames(data))) {
    
    data$ADDL <- as.integer(data$ADDL) # number of additional doses must be whole numbers
    data$II   <- as.numeric(data$II)
    
    # Treat missing ADDL as a single dose so these rows are never dropped by the split below
    data$ADDL[is.na(data$ADDL)] <- 0L
    
    # Split data into rows with ADDL > 0 and others
    # (ADDL has no NAs at this point, so the two filters are exact complements)
    addl_rows     <- dplyr::filter(data, ADDL > 0 & !is.na(II))
    non_addl_rows <- dplyr::filter(data, !(ADDL > 0 & !is.na(II)))
    
    if (debug) {
      message("nrow(addl_rows): ", nrow(addl_rows))
      message("nrow(non_addl_rows): ", nrow(non_addl_rows))
    }
    
    # Process addl_rows to expand
    if (nrow(addl_rows) > 0) { # Edge case where entire dataset has ADDL == 0
      expanded_addl <- addl_rows %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          # Generate a sequence of times including original and additional doses
          DOSETIME = list(DOSETIME + II * 0:ADDL)
        ) %>%
        tidyr::unnest(cols = c(DOSETIME)) %>%
        dplyr::ungroup()
    } else {
      expanded_addl <- NULL
    }
    
    # Combine with non_addl_rows and arrange by ID and TIME
    df <- dplyr::bind_rows(expanded_addl, non_addl_rows) %>%
      dplyr::mutate(ADDL = NA, II = NA) %>% # Clean up for clarity
      dplyr::arrange(ID, DOSETIME)
  } else {
    df <- data
  }
  
  # NAMT, SAMT, min_yvar are previously calculated
  keep_cols <- c("ID", "facet_label", "DOSETIME", x_axis, dose_col, "NAMT", "SAMT", "min_yvar")
  
  if ("RATE" %in% colnames(df)) {
    # Use df (the expanded data), not data (the original), so lengths and row order match
    df$RATE   <- as.numeric(as.character(df$RATE))
    keep_cols <- c(keep_cols, "RATE")
  }
  
  df <- dplyr::select(df, dplyr::all_of(unique(keep_cols)))
  
  return(df)
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
  if (is.character(df[[column_name]]) && any(df[[column_name]] == "")) {
    shiny::showNotification(paste0("WARNING: Some ", column_name, " values are blank. Renamed to '.(blank)'."), type = "warning", duration = 10)
    df <- df %>% dplyr::mutate(!!column_name := ifelse(!!dplyr::sym(column_name) == "", ".(blank)", !!dplyr::sym(column_name)))
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
    if(wt_name %in% names(ev_df)) { # Use external database weights when available
      ev_df <- ev_df %>%
        as.data.frame() %>%
        dplyr::mutate(amt = amt * !!dplyr::sym(wt_name)) %>%
        mrgsolve::as.ev()
    } else {
      wt_multiplication_value <- mrgsolve::param(mod)[[wt_name]]
      ev_df <- ev_df %>%
        as.data.frame() %>%
        dplyr::mutate(amt = amt * wt_multiplication_value) %>%
        mrgsolve::as.ev()
    }
  }

  if (model_dur) { # modeling duration, it cannot coexist with tinf
    if(debug) {message("Applying model_dur transformation")}
    ev_df <- ev_df %>%
      as.data.frame() %>%
      dplyr::mutate(rate = -2) %>%
      dplyr::select(-tinf) %>%
      mrgsolve::as.ev()
  }

  if (model_rate) {
    if(debug) {message("Applying model_rate transformation")}
    ev_df <- ev_df %>%
      as.data.frame() %>%
      dplyr::mutate(rate = -1) %>%
      dplyr::select(-tinf) %>%
      mrgsolve::as.ev()
  }

  if (pred_model) { # set all CMTs to zero as that is required for PRED models
    if(debug) {message("Applying pred_model transformation")}
    ev_df <- ev_df %>%
      dplyr::mutate(cmt = 0) %>%
      dplyr::select(-dplyr::any_of(c("tinf", "rate")))
  }

  return(ev_df)
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
#' @param append_id_text        A string prefix to be inserted for each ID
#' @param show_progress         When TRUE, shows shiny progress messages
#' @param debug                 When TRUE, outputs debugging messages
#' @param gradient              When TRUE, perform gradient runs in between lower/upper
#' @param parallel_sim          Default FALSE, uses the future and mrgsim.parallel packages !Not implemented live!
#' @param parallel_n            The number of subjects required before parallelization is used !Not implemented live!
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
                               append_id_text     = "m1-",
                               show_progress      = TRUE,
                               gradient           = FALSE,
                               nsubj_offset       = 0,
                               parallel_sim       = FALSE,
                               parallel_n         = 200#,
) {
  
  if (nrow(batch_run_df) == 0) return(NULL)
  
  # ── Coerce all non-name columns to numeric ────────────────────────────────────
  batch_run_df <- batch_run_df %>%
    dplyr::mutate(dplyr::across(-Name, as.numeric))
  
  # ── Optionally expand with gradient quantile columns ─────────────────────────
  if (gradient) {
    splits       <- lapply(seq_len(nrow(batch_run_df)), function(i) {
      split_reference_value(
        batch_run_df$Reference[i],
        batch_run_df$Lower[i],
        batch_run_df$Upper[i]
      )
    })
    batch_run_df <- batch_run_df %>%
      dplyr::mutate(
        LowerQ1 = sapply(splits, function(s) s$lower_splits[1]),
        LowerQ2 = sapply(splits, function(s) s$lower_splits[2]),
        LowerQ3 = sapply(splits, function(s) s$lower_splits[3]),
        UpperQ1 = sapply(splits, function(s) s$upper_splits[1]),
        UpperQ2 = sapply(splits, function(s) s$upper_splits[2]),
        UpperQ3 = sapply(splits, function(s) s$upper_splits[3])
      ) %>%
      dplyr::select(Name, Reference, Lower,
                    LowerQ1, LowerQ2, LowerQ3,
                    UpperQ1, UpperQ2, UpperQ3,
                    Upper)
  }
  
  # ── Pre-compute loop invariants ───────────────────────────────────────────────
  n_params      <- nrow(batch_run_df)
  ref_values    <- stats::setNames(batch_run_df$Reference, batch_run_df$Name)
  
  # Determine which bound columns to iterate over, in order
  lower_cols <- if (gradient) c("Lower", "LowerQ1", "LowerQ2", "LowerQ3") else "Lower"
  upper_cols <- if (gradient) c("UpperQ1", "UpperQ2", "UpperQ3", "Upper") else "Upper"
  bound_cols <- c(lower_cols, upper_cols)
  
  # ── Build run schedule upfront ────────────────────────────────────────────────
  # Each row describes one simulation: which param, which bound value, display names
  make_suffix_good <- function(suffix) {
    if (suffix == "") "" else paste0(" (", suffix, ")")
  }
  
  schedule_rows <- vector("list", length(bound_cols) * n_params)
  k <- 0L
  for (col in bound_cols) {
    # Derive display suffix from column name (e.g. "LowerQ1" -> "Q1", "Lower" -> "")
    direction  <- ifelse(grepl("^Lower", col) | col == "Lower", "lower", "upper")
    raw_suffix <- sub("^Lower|^Upper", "", col)   # "" | "Q1" | "Q2" | "Q3"
    suffix_good <- make_suffix_good(raw_suffix)
    cat_label   <- paste0(
      ifelse(direction == "lower", "Lower", "Upper"),
      suffix_good
    )
    
    for (i in seq_len(n_params)) {
      k <- k + 1L
      schedule_rows[[k]] <- list(
        row_idx         = i,
        param_name      = batch_run_df$Name[i],
        bound_value     = batch_run_df[[col]][i],
        reference_value = ref_values[batch_run_df$Name[i]],
        run_name        = paste0(direction, batch_run_df$Name[i], raw_suffix),
        run_name_good   = paste0(direction, " ", batch_run_df$Name[i], suffix_good),
        cat_name        = cat_label
      )
    }
  }
  
  # Pre-select columns needed for update_model_object to avoid repeated select()
  base_param_df <- batch_run_df %>% dplyr::select(Name, Reference)
  
  n_runs      <- length(schedule_rows) + 1L   # +1 for reference
  list_of_runs <- vector("list", n_runs)
  
  # ── Simulation loop ───────────────────────────────────────────────────────────
  shiny::withProgress(message = "Batch Run", value = 0, {
    
    # -- Reference run (i = 1) --
    if (show_progress) shiny::setProgress(value = 1 / n_runs, detail = paste0("1/", n_runs))
    if (debug) message("Trying Batch run: Reference")
    
    tmp <- run_single_sim(
      input_model_object = update_model_object(input_model_object, base_param_df,
                                               convert_colnames = TRUE),
      wt_based_dosing    = wt_based_dosing,
      wt_name            = wt_name,
      pred_model         = pred_model,
      ev_df              = ev_df,
      model_dur          = model_dur,
      model_rate         = model_rate,
      sampling_times     = sampling_times,
      divide_by          = divide_by,
      debug              = debug,
      append_id_text     = "ref",
      #nsubj_offset       = 0,
      parallel_sim       = parallel_sim,
      parallel_n         = parallel_n
    )
    
    if (!is.null(tmp) && is.data.frame(tmp)) {
      tmp$.desc      <- "Reference"
      tmp$.cat       <- "Reference"
      tmp$.paramname <- "Reference"
      tmp$.value     <- -99
      tmp$.ratio     <- 1
      list_of_runs[[1]] <- tmp
    }
    
    # -- Bound runs (i = 2 .. n_runs) --
    for (i in seq_along(schedule_rows)) {
      s      <- schedule_rows[[i]]
      run_i  <- i + 1L
      
      if (show_progress) shiny::setProgress(value = run_i / n_runs,
                                            detail = paste0(run_i, "/", n_runs))
      if (debug) message("Trying Batch run: ", s$run_name_good)
      
      # Modify only the single reference value that changes — no full df copy
      param_df_i <- base_param_df
      param_df_i$Reference[s$row_idx] <- s$bound_value
      
      tmp <- run_single_sim(
        input_model_object = update_model_object(input_model_object, param_df_i,
                                                 convert_colnames = TRUE),
        wt_based_dosing    = wt_based_dosing,
        wt_name            = wt_name,
        pred_model         = pred_model,
        ev_df              = ev_df,
        model_dur          = model_dur,
        model_rate         = model_rate,
        sampling_times     = sampling_times,
        divide_by          = divide_by,
        debug              = debug,
        append_id_text     = s$run_name,
        nsubj_offset       = run_i - 1L,
        parallel_sim       = parallel_sim,
        parallel_n         = parallel_n
      )
      
      if (!is.null(tmp) && is.data.frame(tmp)) {
        tmp$.desc         <- s$run_name_good
        tmp$.cat          <- s$cat_name
        tmp$.paramname    <- s$param_name
        tmp$.value        <- s$bound_value
        tmp$.ratio        <- s$bound_value / s$reference_value
        list_of_runs[[run_i]] <- tmp
      }
    }
    
  }) # end withProgress
  
  #tmp_batch_run_bind <<- dplyr::bind_rows(list_of_runs)
  
  dplyr::bind_rows(list_of_runs)
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

  # # Coerce all columns except "Name" to numeric
  starting_table <- param_df %>%
    dplyr::mutate(dplyr::across(-Name, as.numeric))

  if(any(is.na(starting_table))) {
    return(starting_table)
  }

  # Default case is changing entire table
  # A limitation is that changing lower bound and then modifying upper multiplier will update entire table (same goes for changing upper -> modifying lower multiplier)
  # A workaround is not found, perhaps due to circular logic?
  all_params_table <- starting_table %>%
    dplyr::mutate(
      Lower = Reference * sanitize_numeric_input(lower_multiplier, allow_zero = FALSE, return_value = 0.1, display_error = TRUE),
      Upper = Reference * sanitize_numeric_input(upper_multiplier, allow_zero = FALSE, return_value = 1.1, display_error = TRUE)
    )

  # If last change was on a reference value. Only the corresponding upper/lower bounds should be updated
  # Switching models could introduce more than one last_change_ref_model_1() so we're also checking against that
  if(length(last_change_ref_index) == 1 && last_change_ref_index > 0) {
    all_params_table <- starting_table
    all_params_table$Lower[last_change_ref_index] <- all_params_table$Reference[last_change_ref_index] * sanitize_numeric_input(lower_multiplier, allow_zero = FALSE, return_value = 0.1, display_error = TRUE)
    all_params_table$Upper[last_change_ref_index] <- all_params_table$Reference[last_change_ref_index] * sanitize_numeric_input(upper_multiplier, allow_zero = FALSE, return_value = 1.1, display_error = TRUE)
  }

  # If last change was on a bound, don't update the entire table, however updating the table has some weird circular logic interaction
  # The compromise is either 1) Multipliers stop working after changing reference value, or
  # 2) When multipliers are changed, entire table gets updated. I think having 2) is more user-friendly if there is a reminder to ask users to edit bounds last
  # 3) - directly updating the tor_tab_new_model_1() reactive seems to have worked well such that changing bounds and then going back and changing reference does not update entire table

  if((length_lower_change == 1 & length_upper_change == 0) |
     (length_lower_change == 0 & length_upper_change == 1)){
    #message("no change")
    all_params_table <- starting_table
  }

  # Coerce entire df to character before displaying, this is needed to always trigger the type change in tor_tab_model_1()
  all_params_table <- all_params_table %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  return(all_params_table)
}
