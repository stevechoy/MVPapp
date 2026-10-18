# NCA, exposure metrics and summary statistics

#=============================================================================
#' @name calc_summary_stats
#'
#' @title Calculate summary stats with optional rounding
#'
#' @param orig_data           input dataframe
#' @param dp                  round to this many decimal places
#' @param sigdig              Set to TRUE to round using significant digits instead
#' @param convert_to_numeric  Default TRUE, convert df to numeric and turns characters to NA's
#' @param transpose           Set to TRUE to transpose the table such that each variable
#'                      (column) becomes a row
#' @param check_empty_rows    Default TRUE, will not perform summary stats if there are no rows in df
#' @param id_colname          The ID column to distinct by and then removed before summary calcs are done
#' @param comma_format        Set to TRUE to use big mark formatting
#' @param replace_non_numeric_to_NA Set to TRUE to replace non-numeric characters with NA
#'
#' @returns a dataframe with summary stats
#' @importFrom dplyr mutate mutate_all distinct select sym summarise across
#' @importFrom tidyr everything pivot_longer pivot_wider
#' @importFrom stats median quantile sd
#' @importFrom data.table as.data.table setDT setcolorder
#' @noRd
#=============================================================================

calc_summary_stats <- function(orig_data,
                               dp = 1,
                               sigdig = FALSE,
                               convert_to_numeric = TRUE,
                               transpose = FALSE,
                               check_empty_rows = TRUE,
                               id_colname = "ID",
                               comma_format = TRUE,
                               replace_non_numeric_to_NA = TRUE) {

  data <- orig_data
  
  if (convert_to_numeric) {
    cols_to_convert <- setdiff(names(orig_data), id_colname)
    data <- data.table::as.data.table(data)
    data[, (cols_to_convert) := lapply(.SD, function(x) suppressWarnings(as.numeric(as.character(x)))),
         .SDcols = cols_to_convert]
  } else {
    data <- data.table::as.data.table(data)  # ensure data.table before any := usage
  }
  
  if (id_colname %in% names(data)) {
    data <- unique(data, by = id_colname)          # data.table native distinct
    data[, (id_colname) := NULL]                   # in-place column removal
  }
  
  if (check_empty_rows && nrow(data) == 0L) {
    return(as.data.frame(data))
  }
  
  data <- data.table::setDT(data)
  
  # Single-pass summary — all stats computed in one lapply over columns
  stats_fns <- list(
    "Min"       = function(x) min(x,                                  na.rm = TRUE),
    "5%"        = function(x) quantile(x, probs = 0.05,              na.rm = TRUE),
    "1st Qu."   = function(x) quantile(x, probs = 0.25,              na.rm = TRUE),
    "Median"    = function(x) median(x,                               na.rm = TRUE),
    "Mean"      = function(x) mean(x,                                 na.rm = TRUE),
    "3rd Qu."   = function(x) quantile(x, probs = 0.75,              na.rm = TRUE),
    "95%"       = function(x) quantile(x, probs = 0.95,              na.rm = TRUE),
    "Max"       = function(x) max(x,                                  na.rm = TRUE),
    "CV%"       = function(x) abs(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100),
    "gMean"     = function(x) gm_mean(x),
    "gMean CV%" = function(x) gm_mean_cv(x)
  )
  
  col_names <- names(data)
  
  tmp <- data.table::rbindlist(lapply(names(stats_fns), function(stat_name) {
    row <- data[, lapply(.SD, stats_fns[[stat_name]]), .SDcols = col_names]
    row[, Statistic := stat_name]
  }))
  
  data.table::setcolorder(tmp, c("Statistic", col_names))
  
  # Drop known non-metric columns in one pass
  drop_cols <- intersect(c("AGEMO", "SEX"), names(tmp))
  if (length(drop_cols)) tmp[, (drop_cols) := NULL]
  
  # Derive numeric cols from tmp's CURRENT columns — not the stale pre-drop col_names
  num_cols <- names(tmp)[vapply(tmp, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "Statistic")  # exclude the Statistic label column
  
  # Round in-place on numeric columns — no data.table -> tibble -> data.table round-trip
  if (length(num_cols)) {
    round_fn <- if (sigdig) function(x) signif(x, dp) else function(x) round(x, dp)
    tmp[, (num_cols) := lapply(.SD, round_fn), .SDcols = num_cols]
  }
  
  # Comma formatting via lapply — avoids for loop
  if (comma_format) {
    fmt_cols <- names(tmp)[vapply(tmp, is.numeric, logical(1))]
    tmp[, (fmt_cols) := lapply(.SD, function(x) format(x, big.mark = ",")), .SDcols = fmt_cols]
  }
  
  # Convert to character for display
  tmp <- tmp[, lapply(.SD, as.character)]
  tmp <- as.data.frame(tmp)
  
  # Transpose: t() is faster than double pivot for a pure reshape
  if (transpose) {
    stat_names <- tmp$Statistic
    row_names  <- setdiff(names(tmp), "Statistic")
    mat        <- t(as.matrix(tmp[, row_names]))
    tmp        <- as.data.frame(mat, stringsAsFactors = FALSE)
    colnames(tmp) <- stat_names
    tmp        <- cbind(ColumnName = row_names, tmp)
    rownames(tmp) <- NULL
  }
  
  return(tmp)
}

#=============================================================================
#' @name calc_summary_stats_as_list
#'
#' @title Return summary stats as a list
#'
#' @param nca_df              input dataframe (after using NonCompart::tblNCA)
#' @param group_by_name       the singular "key" argument for NonCompart::tblNCA *minus* the ID column
#' @param list_of_nca_metrics columns to retain in the summary table
#' @param dp                  round to this many decimal places
#' @param sigdig              Set to TRUE to round using significant digits instead
#' @param convert_to_numeric  Default FALSE, convert df to numeric and turns characters to NA's
#' @param transpose           Set to TRUE to transpose the table such that each variable
#'                      (column) becomes a row
#' @param id_colname          The ID column to distinct by and then removed before summary calcs are done
#'
#' @returns a list containing X number of dataframes for each unique value of group_by_name
#' @importFrom dplyr mutate mutate_all distinct select sym rename one_of
#' @importFrom tidyr everything pivot_longer pivot_wider
#' @noRd
#=============================================================================

calc_summary_stats_as_list <- function(nca_df,
                                       group_by_name,
                                       list_of_nca_metrics = c("AUCLST", "AUCIFO", "CMAX", "CMAXD", "LAMZHL", "TMAX",
                                                               "MRTIVLST", "MRTIVIFO", "MRTIVIFP", "MRTEVLST", "MRTEVIFO", "MRTEVIFP",
                                                               "VZO", "VZP", "VZFO", "VZFP",
                                                               "CLO", "CLP", "CLFO", "CLFP"),
                                       dp                 = 3,
                                       sigdig             = TRUE,
                                       convert_to_numeric = FALSE,
                                       transpose          = TRUE,
                                       id_colname         = "ID") {
  
  # Defined once — used in both transpose branches to avoid duplication
  keep_stats <- c("Min", "Mean", "Median", "Max", "CV%", "gMean", "gMean CV%")
  
  # Helper that processes a single already-filtered subset
  process_subset <- function(subset_df, n_val) {
    result <- calc_summary_stats(
      subset_df,
      dp                = dp,
      sigdig            = sigdig,
      convert_to_numeric = convert_to_numeric,
      transpose         = transpose,
      id_colname        = id_colname
    )
    
    if (transpose) {
      result %>%
        dplyr::filter(ColumnName %in% list_of_nca_metrics) %>%
        dplyr::select(ColumnName, dplyr::any_of(keep_stats)) %>%
        dplyr::mutate(N = n_val) %>%
        dplyr::select(ColumnName, N, dplyr::everything()) %>%
        dplyr::rename(Metric = ColumnName)
    } else {
      result %>%
        dplyr::filter(Statistic %in% keep_stats) %>%
        dplyr::select(Statistic, dplyr::any_of(list_of_nca_metrics)) %>%
        dplyr::mutate(N = n_val) %>%
        dplyr::select(Statistic, N, dplyr::everything())
    }
  }
  
  if (group_by_name %in% names(nca_df)) {
    # Pre-compute unique group values once — not repeated per iteration
    group_vals <- unique(nca_df[[group_by_name]])
    
    lapply(group_vals, function(grp_val) {
      # Filter once — reused for both calc_summary_stats and nrow
      subset_df <- nca_df %>%
        dplyr::filter(!!dplyr::sym(group_by_name) == grp_val) %>%
        dplyr::select(-!!dplyr::sym(group_by_name))
      
      process_subset(subset_df, n_val = nrow(subset_df))
    })
    
  } else {
    list(process_subset(nca_df, n_val = nrow(nca_df)))
  }
}

#=============================================================================
#' @name gm_mean
#'
#' @title Geometric mean
#'
#' @param x A numeric
#' @param na.rm Set to TRUE to remove NAs
#'
#' @returns A numeric
#' @noRd
#=============================================================================

gm_mean = function(x, na.rm=TRUE){
  # Same convention as gm_mean_cv(): NA, zero and negative values are excluded
  # (previously they were still counted in the denominator, biasing it low)
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 0) return(NA_real_)
  exp(mean(log(x)))
}

#=============================================================================
#' @name gm_mean_cv
#'
#' @title Geometric mean CV %
#'
#' @param x A numeric
#' @param na.rm Set to TRUE to remove NAs
#'
#' @returns A numeric
#' @noRd
#=============================================================================

gm_mean_cv = function(x, na.rm = TRUE) {
  logx <- log(x)
  # Removes NaN, Inf, -Inf resulting from log of 0, negative numbers, +/- infinites
  if(na.rm) {
    logx <- logx[is.finite(logx)]
  }
  sd.logx <- sd(logx)
  gmeancv <- sqrt(exp(sd.logx^2) - 1) * 100 # Check wiki on Coefficient of variation
  return(gmeancv)
}

#-------------------------------------------------------------------------------
#' @name pct_change_from
#' @title Percent change from a baseline; NA when the baseline is 0 or not finite
#' @param x values
#' @param baseline baseline value
#' @noRd
#-------------------------------------------------------------------------------
pct_change_from <- function(x, baseline) {
  if (length(baseline) != 1 && length(baseline) != length(x)) baseline <- baseline[1]
  out <- (x - baseline) / baseline * 100
  out[!(is.finite(baseline) & baseline != 0)] <- NA_real_
  out
}

#' @noRd
safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)[1]
}

#-------------------------------------------------------------------------------
#' @name quantile_output
#'
#' @title Creates upper and lower percentiles by TIME for a given dataset
#'
#' @param iiv_sim_input  a dataframe containing TIME column
#' @param yvar           name of column to do the quantile calculation on
#' @param lower_quartile lower quantile for yvar, default 0.025  (2.5%)
#' @param upper_quartile lower quantile for yvar, default 0.0975 (97.5%)
#' @param dp             decimal places for rounding
#'
#' @returns a dataframe
#' @importFrom dplyr group_by mutate ungroup
#' @noRd
#-------------------------------------------------------------------------------

quantile_output <- function(iiv_sim_input,
                            yvar = 'DV',
                            lower_quartile = 0.025,
                            upper_quartile = 0.975,
                            dp   = 5
) {
  obs_only(iiv_sim_input) %>%
    dplyr::group_by(TIME) %>%
    dplyr::mutate(median_yvar = round(stats::quantile(.data[[yvar]], probs = 0.5,            na.rm = TRUE), dp),
                  mean_yvar   = round(mean(.data[[yvar]], na.rm = TRUE), dp),
                  lower_yvar  = round(stats::quantile(.data[[yvar]], probs = lower_quartile, na.rm = TRUE), dp),
                  upper_yvar  = round(stats::quantile(.data[[yvar]], probs = upper_quartile, na.rm = TRUE), dp)) %>%
    dplyr::ungroup()
}

#-------------------------------------------------------------------------------
#' @name pknca_table
#'
#' @title Exposure metrics of a single simulated profile
#'
#' @description
#' Metrics over the whole simulation (columns ending in `_tlast`) and over the
#' time window `[start_time, end_time]` (columns ending in `_ranged`), from
#' observation records only.
#'
#' @param input_simulated_table  a dataframe (usually created from mrgsim)
#' @param output_conc            y variable of interest to perform summary stats calc
#' @param start_time             start time interval for metrics
#' @param end_time               end time interval for metrics
#' @param debug                  unused, kept for the app's calls
#'
#' @returns the simulated rows with the metric columns added
#' @noRd
#-------------------------------------------------------------------------------

pknca_table <- function(input_simulated_table,
                        output_conc,
                        start_time = NULL,
                        end_time   = NULL,
                        debug      = FALSE
) {
  if (is.null(input_simulated_table)) return(NULL)

  df <- dplyr::arrange(obs_only(input_simulated_table), TIME)
  y  <- df[[output_conc]]
  baseline <- y[1] # % change from baseline is always relative to the first time point overall

  full <- conc_metrics(df$TIME, y, baseline)
  win  <- window_rows(df, start_time, end_time)
  rng  <- conc_metrics(win$TIME, win[[output_conc]], baseline)

  # Whole simulation ("_tlast"): one row per time point; only CFBPCT varies by row
  out <- dplyr::select(df, -dplyr::any_of(c("ID", "tad", "TIMEADJ")))
  out[[paste0(output_conc, "_CMIN_tlast")]]       <- full$CMIN
  out[[paste0(output_conc, "_CMAX_tlast")]]       <- full$CMAX
  out[[paste0(output_conc, "_CFBPCT_tlast")]]     <- full$CFBPCT
  out[[paste0(output_conc, "_NADIRPCT_tlast")]]   <- full$NADIRPCT
  out[[paste0(output_conc, "_TMAX_tlast")]]       <- full$TMAX
  out[[paste0(output_conc, "_TMIN_tlast")]]       <- full$TMIN
  out$AUC_tlast                                   <- full$AUC
  out[[paste0(output_conc, "_CAVG_tlast")]]       <- full$CAVG
  out[[paste0(output_conc, "_MEANCFBPCT_tlast")]] <- full$MEANCFBPCT

  # Time window ("_ranged"): single values; % change is the value at the window's end
  out[[paste0(output_conc, "_CMIN_ranged")]]       <- rng$CMIN
  out[[paste0(output_conc, "_CMAX_ranged")]]       <- rng$CMAX
  out[[paste0(output_conc, "_CAVG_ranged")]]       <- rng$CAVG
  out[[paste0(output_conc, "_TMAX_ranged")]]       <- rng$TMAX
  out[[paste0(output_conc, "_TMIN_ranged")]]       <- rng$TMIN
  out$AUC_ranged                                   <- rng$AUC
  out[[paste0(output_conc, "_CFBPCT_ranged")]]     <- dplyr::last(rng$CFBPCT)
  out[[paste0(output_conc, "_MEANCFBPCT_ranged")]] <- rng$MEANCFBPCT
  out[[paste0(output_conc, "_NADIRPCT_ranged")]]   <- rng$NADIRPCT
  out
}

#-------------------------------------------------------------------------------
#' @name conc_metrics
#'
#' @title Exposure metrics of one profile
#'
#' @description
#' Cmin/Cmax (and the first time they occur), AUC by the linear trapezoidal rule,
#' Cavg (AUC divided by the time span), % change from `baseline` at each time
#' point, its nadir, and its time-weighted mean.
#'
#' @param time      sampling times, in increasing order
#' @param y         values at those times
#' @param baseline  reference value for the % change
#'
#' @returns a list; `CFBPCT` has one value per time point, the others are single values
#' @noRd
#-------------------------------------------------------------------------------
conc_metrics <- function(time, y, baseline) {
  cmin <- min(y, na.rm = TRUE)
  cmax <- max(y, na.rm = TRUE)
  cfb  <- pct_change_from(y, baseline)
  auc  <- trapezoid_auc(time, y)
  list(CMIN       = cmin,
       CMAX       = cmax,
       TMIN       = time[y == cmin][1],
       TMAX       = time[y == cmax][1],
       AUC        = auc,
       CAVG       = time_averaged(auc, time, y),
       CFBPCT     = cfb,
       NADIRPCT   = safe_min(cfb),
       MEANCFBPCT = if (all(is.na(cfb))) NA_real_ else time_averaged(trapezoid_auc(time, cfb), time, cfb))
}

#-------------------------------------------------------------------------------
#' @name trapezoid_auc
#'
#' @title Area under the curve by the linear trapezoidal rule
#'
#' @param time  times, in increasing order
#' @param y     values at those times
#'
#' @returns a number; intervals with a missing value count as 0
#' @noRd
#-------------------------------------------------------------------------------
trapezoid_auc <- function(time, y) {
  sum((y + dplyr::lag(y)) * (time - dplyr::lag(time)) / 2, na.rm = TRUE)
}

#-------------------------------------------------------------------------------
#' @name pct_above_y_at_x
#' @title Function that calculates % of population > Y-value at
#'                       a given X-value
#'
#' @param model_is_valid    checkpoint before rest of function proceeds, default FALSE
#' @param input_df          Dataset containing ID column with more than 1 ID
#' @param y_name            Name of column (string) for Y-value
#' @param y_value           Y-value threshold (>), e.g. plasma concentration
#' @param x_name            Name of column (string) for X-value
#' @param x_value           X-value threshold (==), e.g. time
#' @param return_number_ids Return number of IDs that fits the criteria instead of %
#'                    Default FALSE
#'
#' @returns a numeric (either as a proportion (from 0 to 1, where 0 means 0% and
#'                    1 means 100%), or the actual number of IDs)
#' @importFrom dplyr filter sym
#' @noRd
#-------------------------------------------------------------------------------

pct_above_y_at_x <- function(model_is_valid = FALSE,
                             input_df,
                             y_name  = "DV",
                             y_value = NA,
                             x_name  = "TIME",
                             x_value = NA,
                             return_number_ids = FALSE) {

  if (!isTRUE(model_is_valid) || length(x_value) != 1 || length(y_value) != 1 || is.na(x_value) || is.na(y_value)) {
    return(NA)
  }

  input_df <- obs_only(input_df)
  above    <- which(input_df[[y_name]] > y_value & input_df[[x_name]] == x_value)
  n_above  <- length(unique(input_df$ID[above]))

  if (return_number_ids) n_above else round(n_above / length(unique(input_df$ID)) * 100, 1)
}

#-------------------------------------------------------------------------------
#' @name tblNCA_progress
#' @title Modified NonCompart::tblNCA to support progress bars
#'
#' @inheritParams NonCompart::tblNCA
#' @param show_progress display progress messages
#' @noRd
#-------------------------------------------------------------------------------

tblNCA_progress <- function (concData, key = "Subject", colTime = "Time", colConc = "conc",
                             dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg",
                             timeUnit = "h", concUnit = "ug/L", down = "Linear", R2ADJ = 0,
                             MW = 0, SS = FALSE, iAUC = "", excludeDelta = 1, show_progress = TRUE)
{
  rlang::check_installed(c("ncar", "NonCompart"), reason = "to use `tblNCA_progress()`.")
  class(concData) = "data.frame"
  nKey = length(key)
  for (i in 1:nKey) {
    if (sum(is.na(concData[, key[i]])) > 0)
      stop(paste(key[i], "has NA value, which is not allowed!"))
  }
  IDs = unique(as.data.frame(concData[, key], ncol = nKey))
  nID = nrow(IDs)
  if (length(dose) == 1) {
    dose = rep(dose, nID)
  }
  else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }
  res_list = vector("list", nID) # filled per subject, bound once (rbind in a loop is quadratic)
  row_index = nca_index(concData, key) # rows of each subject, split once
  calc_all = function() {
    for (i in 1:nID) {
      if(show_progress) {setProgress(value = i / nID, detail = paste0("Subject ", i, "/", nID))}
      strHeader = paste(paste0(key, "=", vapply(IDs[i, , drop = FALSE], as.character, "")), collapse = ", ")
      tData = nca_subset(concData, key, IDs[i, , drop = FALSE], index = row_index)
      if (nrow(tData) > 0) {
        tRes = NonCompart::sNCA(tData[, colTime], tData[, colConc], dose = dose[i],
                    adm = adm, dur = dur, doseUnit = doseUnit, timeUnit = timeUnit,
                    concUnit = concUnit, R2ADJ = R2ADJ, down = down,
                    MW = MW, SS = SS, iAUC = iAUC, Keystring = strHeader,
                    excludeDelta = excludeDelta)
        res_list[[i]] <<- tRes
      }
    }
  }
  # withProgress() needs a running Shiny session, so only use it when asked to
  if (show_progress) {
    withProgress(message = "Calculating NCA", value = 0, calc_all())
  } else {
    calc_all()
  }
  Res = do.call(rbind, res_list) # NULL entries (subjects without data) are dropped
  Res = cbind(IDs, Res)
  rownames(Res) = NULL
  colnames(Res)[1:nKey] = key
  res_done = Filter(Negate(is.null), res_list)
  attr(Res, "units") = c(rep("", nKey), if (length(res_done) > 0) attr(res_done[[1]], "units"))
  return(Res)
}

#-------------------------------------------------------------------------------
# Internal PDF page helpers for pdfNCA_wm()
#
# Adapted from ncar (R/BasicUtil.R, GPL-3, Kyun-Seop Bae). These were exported
# by earlier versions of ncar but are no longer exported (ncar >= 0.7.2), so
# they are vendored here as internal functions.
#-------------------------------------------------------------------------------

#' @noRd
nca_prep_pdf <- function(FileName, Paper = "letter", FontFamily = "Courier") {
  grDevices::pdf(FileName, paper = Paper, width = 8.5, height = 11, family = FontFamily, title = "NCA Report")
}

#' @noRd
nca_close_pdf <- function() {
  grDevices::dev.off()
}

#' @noRd
nca_add_page <- function(Cex = 0.8, Header1 = "", Header2 = "", Header3 = "",
                         Footer1 = "", Footer2 = "", Footer3 = "",
                         PrintRowNum = FALSE, StartRowNum = 1) {
  if (Cex < 0.1 | Cex > 1.2) stop("Too small or too large cex!")
  nCol <- trunc(242 - 190 * Cex)
  nRow <- trunc(155 - 125 * Cex)
  options(width = max(10, trunc(300 - 250 * Cex))) # restored by pdfNCA_wm() on exit

  graphics::par(oma = c(0, 0, 0, 0), mfrow = c(1, 1), mar = c(0, 0, 0, 0), adj = 0, cex = Cex)
  graphics::plot(0, 0, type = "n", ylim = c(2 * nRow - 1, 0), xlim = c(0, nCol - 1),
                 xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
  if (PrintRowNum == TRUE) {
    for (j in 1:nRow) {
      graphics::text(-2 - 1 + 3 - floor(log10(j + StartRowNum - 1)), 2 * j - 1,
                     paste0(j + StartRowNum - 1, ":"), offset = 0)
    }
  }
  graphics::text(0, -3, Header1, pos = 4)
  graphics::text(nCol / 2, -3, Header2, pos = 3)
  graphics::text(nCol, -3, Header3, pos = 2)

  graphics::text(0, 2 * nRow + 1, Footer1, pos = 4)
  graphics::text(nCol / 2, 2 * nRow + 1, Footer2, pos = 1)
  graphics::text(nCol, 2 * nRow + 1, Footer3, pos = 2)
}

#' @noRd
nca_text1 <- function(Row, Col, Text, Cex = 0.8) {
  if (Cex < 0.1 | Cex > 1.2) stop("Too small or too large cex!")
  nCol <- trunc(242 - 190 * Cex)
  nRow <- trunc(155 - 125 * Cex)
  if (Col > nCol | Row > nRow) stop("Text seems out of paper!")
  graphics::text(Col - 1, 2 * Row - 1, Text, cex = Cex, offset = 0)
}

#' @noRd
nca_text_m <- function(MTxt, StartRow = 0, Cex = 0.8, Header1 = "", Header2 = "", Header3 = "",
                       Footer1 = "", Footer2 = "", Footer3 = "", PrintRowNum = FALSE) {
  if (Cex < 0.1 | Cex > 1.2) stop("Too small or too large cex!")
  nRow <- trunc(155 - 125 * Cex)
  for (i in seq_along(MTxt)) {
    if ((StartRow + i) %% nRow == 1) {
      nca_add_page(Cex = Cex, Header1 = Header1, Header2 = Header2, Header3 = Header3,
                   Footer1 = Footer1, Footer2 = Footer2, Footer3 = Footer3, PrintRowNum = PrintRowNum)
    }
    nca_text1((StartRow + i - 1) %% nRow + 1, 1, MTxt[i])
  }
}

#-------------------------------------------------------------------------------
#' @name pdfNCA_wm
#' @title Modified ncar::pdfNCA to support watermarks
#'
#' @param watermark Insert watermark when TRUE
#' @param internal_version changes temp dir pathing as a workaround for cloud hosting
#' where access rights prevent writing
#' @param debug_msg show debug messages
#' @param show_progress display progress messages
#' @inheritParams ncar::pdfNCA
#' @importFrom graphics axis close.screen lines par points screen split.screen
#' @noRd
#-------------------------------------------------------------------------------

pdfNCA_wm <- function (fileName = "Temp-NCA.pdf", concData, key = "Subject",
                       colTime = "Time", colConc = "conc", dose = 0, adm = "Extravascular",
                       dur = 0, doseUnit = "mg", timeUnit = "h", concUnit = "ug/L",
                       down = "Linear", R2ADJ = 0, MW = 0, SS = FALSE, iAUC = "",
                       excludeDelta = 1, watermark = TRUE, internal_version = TRUE, debug_msg = TRUE,
                       show_progress = TRUE)
{
  rlang::check_installed(c("ncar", "NonCompart"), reason = "to use `pdfNCA_wm()`.")

  if(debug_msg) {
    message(fileName)
    message(getwd())
  }

  if(!internal_version) { # Workaround for AWS hosting
    #fileName <- paste0("/tmp/", fileName) # didn't work
    old_wd <- setwd(tempdir()) # Write to the session temp directory; restored on exit
    on.exit(setwd(old_wd), add = TRUE)
  }

  if(debug_msg) {
    message(getwd())
  }

  class(concData) = "data.frame"
  old_width <- options("width")
  on.exit(options(old_width), add = TRUE)

  if(debug_msg) {
    message("Trying to open pdf device")
  }
  # Open the PDF device first: par() with no device open would create
  # Rplots.pdf in the working directory and leave that device open
  nca_prep_pdf(fileName)
  pdf_device <- grDevices::dev.cur()
  on.exit({
    # also closes the device if an error stops the report half-way
    if (pdf_device %in% grDevices::dev.list()) {
      grDevices::dev.set(pdf_device)
      grDevices::dev.off()
    }
  }, add = TRUE)
  defPar = par(no.readonly = TRUE)

  nca_add_page()
  nca_text1(1, 1, "Individual Noncompartmental Analysis Result (Non-Validated)",
              Cex = 1.2)
  maxx = max(concData[, colTime], na.rm = TRUE)
  maxy = max(concData[, colConc], na.rm = TRUE)
  miny = min(concData[concData[, colConc] > 0, colConc], na.rm = TRUE)
  nKey = length(key)
  IDs = unique(as.data.frame(concData[, key], ncol = nKey))
  nID = nrow(IDs)
  if (length(dose) == 1) {
    dose = rep(dose, nID)
  }
  else if (length(dose) != nID) {
    stop("Count of dose does not match with number of NCAs!")
  }
  row_index = nca_index(concData, key) # rows of each subject, split once
  for (i in 1:nID) {
    if(show_progress) {
      shiny::incProgress(1/nID, detail = paste("Subject ", i, "/", nID))
    }
    strHeader = paste(paste0(key, "=", vapply(IDs[i, , drop = FALSE], as.character, "")), collapse = ", ")
    tData = nca_subset(concData, key, IDs[i, , drop = FALSE], index = row_index)
    if (nrow(tData) > 0) {
      x = tData[, colTime]
      y = tData[, colConc]
      tabRes = NonCompart::sNCA(x, y, dose = dose[i], adm = adm, dur = dur,
                                doseUnit = doseUnit, timeUnit = timeUnit, concUnit = concUnit,
                                down = down, R2ADJ = R2ADJ, MW = MW, SS = SS,
                                iAUC = iAUC, Keystring = strHeader, excludeDelta = excludeDelta)
      UsedPoints = attr(tabRes, "UsedPoints")
      txtRes = ncar::Res2Txt(tabRes, x, y, dose = dose[i], adm = adm,
                             dur = dur, doseUnit = doseUnit, down = down)
      nca_add_page(Header1 = strHeader)
      nca_text_m(txtRes, StartRow = 1, Header1 = strHeader)
      scrnmat = matrix(0, 3, 4)
      scrnmat[1, ] = c(0, 1, 0, 1)
      scrnmat[2, ] = c(0.1, 0.9, 0.5, 0.95)
      scrnmat[3, ] = c(0.1, 0.9, 0.05, 0.5)
      ScrNo = split.screen(scrnmat)
      screen(ScrNo[1])
      par(adj = 0)
      nca_text1(1, 1, strHeader, Cex = 1)
      screen(ScrNo[2])
      par(oma = c(1, 1, 1, 1), mar = c(4, 4, 3, 1), adj = 0.5)

      if(watermark) {
        grid::pushViewport(grid::viewport(angle = 50, name = "WM"))
        grid::grid.text("For Internal Use Only", x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                        gp = grid::gpar(col = "grey", fontsize = 52, alpha = 0.5))
        grid::popViewport()
      }

      plot(x, y, type = "b", cex = 0.7, xlim = c(0, maxx),
           ylim = c(0, maxy), xlab = paste0("Time (", timeUnit,
                                            ")"), ylab = paste0("Concentration (", concUnit,
                                                                ")"))

      screen(ScrNo[3])
      par(oma = c(1, 1, 1, 1), mar = c(4, 4, 3, 1), adj = 0.5)
      x0 = x[!is.na(y) & y > 0]
      y0 = y[!is.na(y) & y > 0]
      if (length(x0) > 0) {

        if(watermark) {
          grid::pushViewport(grid::viewport(angle = 50, name = "WM"))
          grid::grid.text("For Internal Use Only", x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                          gp = grid::gpar(col = "grey", fontsize = 52, alpha = 0.5))
          grid::popViewport()
        }

        plot(x0, log10(y0), type = "b", cex = 0.7, xlim = c(0,
                                                            maxx), ylim = c(log10(miny), log10(maxy)),
             yaxt = "n", xlab = paste0("Time (", timeUnit,
                                       ")"), ylab = paste0("Concentration (log interval) (",
                                                           concUnit, ")"))

        points(x[UsedPoints], log10(y[UsedPoints]), pch = 16)
        yticks = seq(round(min(log10(y0))), ceiling(max(log10(y0))))
        ylabels = sapply(yticks, function(i) as.expression(bquote(10^.(i))))
        axis(2, at = yticks, labels = ylabels)
        x1 = tabRes["LAMZLL"]
        x2 = tabRes["LAMZUL"]
        deltaX = x1 * 0.05
        y1 = log10(2.718281828) * (tabRes["b0"] - tabRes["LAMZ"] *
                                     (x1 - deltaX))
        y2 = log10(2.718281828) * (tabRes["b0"] - tabRes["LAMZ"] *
                                     (x2 + deltaX))
        lines(c(x1 - deltaX, x2 + deltaX), c(y1, y2),
              lty = 2, col = "red")
      }
      close.screen(all.screens = TRUE)
    }
  }
  par(defPar)
  nca_close_pdf()

}

#-------------------------------------------------------------------------------
#' @name exposures_table
#'
#' @title Function to calculate exposures from IIV output
#'
#' @param input_simulated_table  a dataframe (usually created from mrgsim)
#' @param output_conc            y variable of interest to perform summary stats calc
#' @param start_time             start time interval for metrics
#' @param end_time               end time interval for metrics
#' @param carry_out              Provide a vector of column names to retain in the summary table
#' @param debug                  show debugging messages
#'
#' @returns a dataframe with additional columns of summary stats
#' @importFrom dplyr mutate if_else ungroup select filter rename first group_by distinct summarise any_of
#' @noRd
#-------------------------------------------------------------------------------

exposures_table <- function(input_simulated_table,
                            output_conc,
                            start_time = NULL,
                            end_time   = NULL,
                            carry_out  = "ID", # would be replaced during function call
                            debug      = FALSE
) {
  df <- obs_only(input_simulated_table)
  df$YVARNAME <- df[[output_conc]]

  # One row per subject; carry_out columns keep each subject's first value in the window
  window_rows(df, start_time, end_time) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(Cmin  = min(YVARNAME, na.rm = TRUE),
                  Cmax  = max(YVARNAME, na.rm = TRUE),
                  Clast = dplyr::last(YVARNAME, na_rm = TRUE),
                  Tmin  = TIME[which.min(YVARNAME)[1]],
                  Tmax  = TIME[which.max(YVARNAME)[1]],
                  AUC   = trapezoid_auc(TIME, YVARNAME),
                  Cavg  = time_averaged(AUC, TIME, YVARNAME)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c("USUBJID", "ID", "Cmin", "Cmax", "Cavg", "Clast", "AUC", "Tmax", "Tmin", carry_out))) %>%
    dplyr::distinct(USUBJID, .keep_all = TRUE)
}

#-------------------------------------------------------------------------------
#' @name calculate_quantiles
#'
#' @title Function to split a continuous X-variable for a number of quantiles
#'
#' @param df              Name of dataframe
#' @param xvar            Name of x-axis variable to split by
#' @param num_quantiles   Number of discrete quantiles
#'
#' @returns a one-row data frame of upper limits, columns Q1, Q2, ...
#' @noRd
#-------------------------------------------------------------------------------

calculate_quantiles <- function(df, xvar, num_quantiles) {
  n <- as.numeric(num_quantiles)
  q <- stats::quantile(df[[xvar]], probs = seq(1 / n, 1, length.out = n), na.rm = TRUE, names = FALSE)
  as.data.frame(as.list(stats::setNames(q, paste0("Q", seq_len(n)))))
}

#-------------------------------------------------------------------------------
#' @name nca_subset
#'
#' @title Rows of an NCA dataset matching one combination of key values
#'
#' @description Replaces building and evaluating R code from the data values
#'   (which failed for values containing quotes, e.g. "O'Neil", and could run
#'   code embedded in the data).
#'
#' @param concData Concentration dataset.
#' @param key Character vector of key column names.
#' @param id_values One-row data frame of key values (in the order of `key`).
#'
#' @returns The matching rows of `concData`.
#' @noRd
#-------------------------------------------------------------------------------
nca_subset <- function(concData, key, id_values, index = NULL) {
  if (!is.null(index)) {
    rows <- index[[nca_key_string(lapply(seq_along(key), function(j) id_values[[j]]))]]
    return(concData[rows, , drop = FALSE]) # NULL rows give zero rows
  }
  rows <- Reduce(`&`, lapply(seq_along(key), function(j) {
    as.character(concData[[key[j]]]) == as.character(id_values[[j]])
  }))
  concData[rows, , drop = FALSE]
}

#-------------------------------------------------------------------------------
#' @name nca_key_string
#'
#' @title One string per row from the key values (for matching subjects)
#'
#' @param cols list of key columns (vectors of equal length)
#'
#' @returns character vector
#' @noRd
#-------------------------------------------------------------------------------
nca_key_string <- function(cols) {
  do.call(paste, c(lapply(cols, as.character), sep = "\r"))
}

#-------------------------------------------------------------------------------
#' @name nca_index
#'
#' @title Row numbers of each subject in an NCA dataset
#'
#' @description Splitting once is much faster than comparing every row for
#'   every subject (subjects x rows), e.g. for 2000 subjects.
#'
#' @param concData Concentration dataset.
#' @param key Character vector of key column names.
#'
#' @returns A list of row numbers named by the subjects' key strings, for the
#'   `index` argument of `nca_subset()`.
#' @noRd
#-------------------------------------------------------------------------------
nca_index <- function(concData, key) {
  split(seq_len(nrow(concData)), nca_key_string(lapply(key, function(k) concData[[k]])))
}

#-------------------------------------------------------------------------------
#' @name window_rows
#'
#' @title Rows of simulated output in a time window (inclusive on both ends)
#'
#' @param df          simulated output with TIME
#' @param start_time  start of the window
#' @param end_time    end of the window
#'
#' @returns the filtered data frame
#' @noRd
#-------------------------------------------------------------------------------
window_rows <- function(df, start_time, end_time) {
  warn_window_bounds(df$TIME, start_time, end_time)
  dplyr::filter(df, TIME >= start_time, TIME <= end_time)
}

#-------------------------------------------------------------------------------
#' @name warn_window_bounds
#'
#' @title Warn when a time window's start or end has no sample
#'
#' @description
#' Window metrics use only the sampled rows inside the window (no interpolation),
#' so a bound without a sample shortens the window actually used: e.g. `[24, 30]`
#' with samples at 24 and 36 only covers 24, giving an AUC of 0. In a running
#' app this shows a notification (replaced, not repeated, on recalculation);
#' otherwise it is a warning.
#'
#' @param times       sampling times of the simulated output
#' @param start_time  start of the window
#' @param end_time    end of the window
#'
#' @returns `NULL`, invisibly
#' @noRd
#-------------------------------------------------------------------------------
warn_window_bounds <- function(times, start_time, end_time) {
  times <- times[is.finite(times)]
  if (length(times) == 0 || !is.finite(start_time) || !is.finite(end_time)) return(invisible(NULL))
  has_time <- function(x) any(abs(times - x) <= 1e-8 * max(1, abs(x)))
  missing_bounds <- c(start = !has_time(start_time), end = !has_time(end_time))
  if (!any(missing_bounds)) return(invisible(NULL))

  msg <- paste0("The ", paste(names(missing_bounds)[missing_bounds], collapse = " and "),
                " of the time window [", start_time, ", ", end_time,
                "] ", if (sum(missing_bounds) > 1) "have" else "has",
                " no sampled value. Metrics (AUC, Cmax, Cavg, ...) use only the samples inside the window ",
                "without interpolation, so the window actually used is shorter.")
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::showNotification(msg, type = "warning", duration = 10, id = "window_bounds_warning")
  } else {
    warning(msg, call. = FALSE)
  }
  invisible(NULL)
}

#-------------------------------------------------------------------------------
#' @name time_averaged
#'
#' @title Average concentration over the sampled time span (AUC / duration)
#'
#' @param auc   area under the curve over the rows
#' @param time  sampling times
#' @param y     values (used when the time span is zero)
#'
#' @returns a single number
#' @noRd
#-------------------------------------------------------------------------------
time_averaged <- function(auc, time, y) {
  span <- max(time, na.rm = TRUE) - min(time, na.rm = TRUE)
  if (is.finite(span) && span > 0) auc[1] / span else mean(y, na.rm = TRUE)
}
