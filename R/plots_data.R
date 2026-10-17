# Data exploration and observed vs simulated plots

#-------------------------------------------------------------------------------
#' @name do_data_page_plot
#' @title Quick Plot for Data Exploration
#'
#' @description
#' This is the main function for plotting uploaded datasets. It is designed to be
#' flexible enough to handle continuous/continuous, discrete/continuous, and
#' discrete/discrete type of data to cover most use cases.
#'
#'
#' @param nmd The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)
#' @param filter_cmt Filter by this CMT
#' @param x_axis X-axis for plot
#' @param y_axis Y-axis for plot
#' @param color_by Color by this column
#' @param med_line When TRUE, will draw median line by equidistant X-axis bins
#' @param med_line_by Column name to draw median line by
#' @param boxplot Draws a geom_boxplot instead of geom_point and geom_line
#' @param num_quantiles Converts a continuous x-axis into discrete number of quantiles
#' @param dolm Insert linear regression with formula on top of plot
#' @param smoother Insert smoother
#' @param facet_name variable(s) to facet by
#' @param logy Log Y-axis
#' @param lby  Log breaks for Y-axis
#' @param logx Log X-axis
#' @param lbx  Log breaks for X-axis
#' @param plot_title Optional plot title
#' @param label_size font size for geom_text labels (N=x for boxplots or linear regressions)
#' @param discrete_threshold Draws geom_count if there are <= this number of unique Y-values
#' @param boxplot_x_threshold Throws an error when the unique values of x-axis exceeds this number
#' @param error_text_color error text color for element text
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab theme_bw labs scale_size_area
#' @importFrom ggplot2 stat_summary stat_smooth scale_y_log10 scale_x_log10 after_stat geom_count
#' @importFrom ggplot2 annotation_logticks ggtitle theme facet_wrap geom_boxplot label_both vars
#' @importFrom ggplot2 scale_color_manual
#' @importFrom scales hue_pal
#' @importFrom stats setNames time
#' @importFrom dplyr filter distinct sym group_by summarise across all_of count ungroup mutate
#' @importFrom dplyr glimpse
#' @noRd
#-------------------------------------------------------------------------------

do_data_page_plot <- function(nmd,
                              filter_cmt,
                              x_axis,
                              y_axis,
                              color_by,
                              med_line,
                              med_line_by,
                              boxplot,
                              num_quantiles,
                              dolm,
                              smoother,
                              facet_name,
                              logy,
                              lby = logbreaks_y,
                              lbx = logbreaks_x,
                              logx,
                              plot_title,
                              label_size = 3,
                              discrete_threshold = 7,
                              boxplot_x_threshold = 20,
                              error_text_color = "#F8766D",
                              debug = FALSE) {
  
  if(debug) message(paste0("Creating data_page_plot"))
  
  nmd         <- dplyr::ungroup(nmd)
  x_label     <- x_axis
  x_axis_orig <- x_axis
  can_quantize <- FALSE
  
  # ── Quantize ──────────────────────────────────────────────────────────────
  if (num_quantiles > 0) {
    if (is.numeric(nmd[[x_axis_orig]])) {
      boxplot      <- TRUE
      can_quantize <- TRUE
      nmd_q  <- calculate_quantiles(df = nmd, xvar = x_axis_orig, num_quantiles = num_quantiles)
      if (debug) message(paste(utils::capture.output(print(nmd_q)), collapse = "\n"))
      nmd    <- categorize_xvar(df = nmd, quantiles_df = nmd_q, xvar = x_axis_orig)
      if (debug) message(paste(utils::capture.output(print(dplyr::count(nmd, Quantile))), collapse = "\n"))
      x_label <- paste0(x_axis, " Quantiles")
      x_axis  <- "Quantile"
    } else {
      shiny::showNotification(
        paste0("ERROR: Cannot quantize ", x_axis_orig, " as it is not a continuous variable"),
        type = "error", duration = 10)
    }
  }
  
  # ── Filters ───────────────────────────────────────────────────────────────
  if (filter_cmt != 'NULL')
    nmd <- dplyr::filter(nmd, CMT %in% filter_cmt)
  
  if ('EVID' %in% names(nmd)) {
    nmd <- dplyr::filter(nmd, EVID == 0)
    shiny::showNotification("Dosing rows (EVID >= 1) are excluded from the general plot.",
                            type = "message", duration = 10)
  }
  
  # ── Blank handling ────────────────────────────────────────────────────────
  if (debug) message("Testing for blanks")
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)
  
  if (!is.null(facet_name) && facet_name[1] != "") {
    for (facet in facet_name[facet_name != x_axis])
      nmd <- handle_blanks(nmd, facet)
  }
  
  # ── Extract columns + shared condition flag ─────────────────
  x_col          <- nmd[[x_axis]]
  y_col          <- nmd[[y_axis]]
  x_is_numeric   <- is.numeric(x_col)
  y_is_numeric   <- is.numeric(y_col)
  can_draw_lines <- x_is_numeric && y_is_numeric && !boxplot
  
  # ── Compute once ──────────────────────────────────────────────────
  has_color   <- color_by != ""
  color_valid <- has_color && !all(is.na(nmd[[color_by]]))
  
  named_color_vector <- NULL
  
  if (has_color) {
    if (!color_valid) {
      shiny::showNotification(
        paste0("WARNING: All values are NA in ", color_by, ". No coloring performed."),
        type = "warning", duration = 10)
    } else {
      if (can_quantize && color_by == x_axis_orig) color_by <- "Quantile"
      nmd <- handle_blanks(nmd, color_by)
      nmd[[color_by]] <- as.factor(nmd[[color_by]])
      n <- nlevels(nmd[[color_by]])
      named_color_vector <- setNames(scales::hue_pal()(n), levels(nmd[[color_by]]))
    }
  }
  
  # ── Facet validity — compute once, used in two places ─────────────
  has_valid_facet   <- !is.null(facet_name[1]) && facet_name[1] != ""
  facet_same_as_x   <- has_valid_facet && x_axis %in% facet_name
  
  # ── Build base plot once ──────────────────────────────────────────
  make_base_aes <- function(include_group = TRUE) {
    if (color_valid && !is.null(named_color_vector)) {
      aes_out <- if (include_group) {
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                     group = ID, color = !!dplyr::sym(color_by))
      } else {
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                     color = !!dplyr::sym(color_by))
      }
    } else {
      aes_out <- if (include_group) {
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID)
      } else {
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]])
      }
    }
    aes_out
  }
  
  # ── Boxplot branch ────────────────────────────────────────────────────────
  if (boxplot) {
    if (length(unique(x_col)) > boxplot_x_threshold) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(title = paste0(
            'ERROR: There are too many X-axis categories (>', boxplot_x_threshold,
            ') for boxplots.\nTry the "Quantize X-axis" option instead.')) +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         plot.title = ggplot2::element_text(color = error_text_color))
      )
    }
    
    if (num_quantiles == 0) nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    
    treat_y_as_discrete <- is.character(y_col) || length(unique(y_col)) <= discrete_threshold
    if (treat_y_as_discrete) {
      shiny::showNotification(
        paste0("WARNING: Treating Y-axis as discrete as it is a character type, or there are <=",
               discrete_threshold, " unique Y values."),
        type = "warning", duration = 10)
      nmd[[y_axis]] <- as.factor(nmd[[y_axis]])
    } else {
      nmd[[y_axis]] <- as.numeric(nmd[[y_axis]])
      max_y <- max(nmd[[y_axis]], na.rm = TRUE)
    }
    
    if (!can_quantize) nmd <- dplyr::distinct(nmd, ID, .keep_all = TRUE)
    
    # Re-extract after potential type changes
    y_col <- nmd[[y_axis]]
    
    a <- ggplot2::ggplot(data = nmd, make_base_aes(include_group = FALSE))
    if (!is.null(named_color_vector))
      a <- a + ggplot2::scale_color_manual(values = named_color_vector)
    
    # ── Compute max_y once ────────────────────────────────────────
    #max_y <- max(y_col, na.rm = TRUE)
    
    # ── df_count ──────────────────────────────────────────────────────────
    df_count <- nmd
    if (facet_same_as_x) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.",
                              type = "error", duration = 10)
    }
    
    count_syms <- if (has_valid_facet && !facet_same_as_x) dplyr::syms(facet_name) else list()
    
    if (treat_y_as_discrete) {
      df_count <- dplyr::count(df_count, !!dplyr::sym(x_axis),
                               !!dplyr::sym(y_axis), !!!count_syms)
      a <- a + ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                                 label = paste0(n), size = n, group = NULL),
          color = "black", vjust = 0.5, size = label_size)
    } else {
      df_count <- dplyr::count(df_count, !!dplyr::sym(x_axis), !!!count_syms)
      a <- a + ggplot2::geom_boxplot(varwidth = TRUE)
      if (label_size > 0) {
        n_label <- if (can_quantize) paste0("Nobs=", df_count$n) else paste0("N=", df_count$n)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = max_y * 1.02,
                                 label = n_label, group = NULL),
          color = "black", vjust = 2, size = label_size)
      }
    }
    
  } else {
    # ── Non-boxplot branch ─────────────────────────────────────────────────
    a <- ggplot2::ggplot(data = nmd, make_base_aes(include_group = TRUE))
    if (!is.null(named_color_vector))
      a <- a + ggplot2::scale_color_manual(values = named_color_vector)
    a <- a + ggplot2::geom_point(alpha = 0.2) + ggplot2::geom_line(alpha = 0.2)
  }
  
  a <- a +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_axis) +
    ggplot2::theme_bw() +
    ggplot2::labs(color = color_by)
  
  # ── Scalar && + shared flag + extract xvar col once ───────
  if (med_line && can_draw_lines) {
    x_col_med      <- nmd[[x_axis]]
    nmd <- dplyr::mutate(nmd,
                         binned_xvar = quantize(x_col_med,
                                                levels = get_bin_times(x_col_med, bin_num = 20,
                                                                       relative_threshold = 0.05)))
    if (med_line_by == "") {
      a <- a + ggplot2::stat_summary(
        data    = nmd,
        mapping = ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL),
        fun     = median, geom = "line", colour = "black", alpha = 1.0)
    } else {
      a <- a + ggplot2::stat_summary(
        data    = nmd,
        mapping = ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL,
                               color = as.factor(.data[[med_line_by]])),
        fun     = median, geom = "line", alpha = 1.0)
    }
  }
  
  if (smoother && can_draw_lines)
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), se = FALSE, linetype = "dashed")
  
  if (dolm && can_draw_lines) {
    df_stats <- lm_eqn(df = nmd, facet_name = facet_name, x = x_axis, y = y_axis)
    
    # ── Use data directly instead of ggplot_build ─────────────────
    x_col_dolm <- nmd[[x_axis]]
    y_col_dolm <- nmd[[y_axis]]
    med_x      <- (min(x_col_dolm, na.rm = TRUE) + max(x_col_dolm, na.rm = TRUE)) / 2
    max_y      <- max(y_col_dolm, na.rm = TRUE)
    
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), method = "lm",
                                  formula = y ~ x, se = FALSE,
                                  colour = "grey", show.legend = FALSE)
    if (label_size > 0)
      a <- a + ggplot2::geom_text(
        data    = df_stats,
        mapping = ggplot2::aes(label = label, x = med_x, y = max_y,
                               group = NULL, color = NULL),
        hjust = 0.5, vjust = 1, show.legend = FALSE, size = label_size)
  }
  
  # ── Facet — single validity check ─────────────────────────────────
  if (has_valid_facet) {
    if (facet_same_as_x) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.",
                              type = "error", duration = 10)
    } else {
      num_facets <- prod(sapply(facet_name, function(v) length(unique(nmd[[v]]))))
      if (num_facets > 30) {
        shiny::showNotification(
          "ERROR: Too many facets (>30) found. Please filter further or choose another variable(s).",
          type = "error", duration = 10)
      } else {
        facet_formula <- as.formula(paste0("~", paste(facet_name, collapse = "+")))
        a <- a + ggplot2::facet_wrap(facet_formula, labeller = ggplot2::label_both)
      }
    }
  }
  
  if (logy && y_is_numeric)
    a <- a +
    ggplot2::scale_y_log10(breaks = logbreaks_y, labels = logbreaks_y, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "l")
  
  if (logx && x_is_numeric && !boxplot)
    a <- a +
    ggplot2::scale_x_log10(breaks = logbreaks_x, labels = logbreaks_x, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "b")
  
  if (!is.null(plot_title))
    a <- a + ggplot2::ggtitle(plot_title)
  
  return(a)
}

#-------------------------------------------------------------------------------
#' @name do_data_page_ind_plot
#' @title Individual Plot for Data Exploration
#'
#' @description
#' This is the main function for plotting individual plots from uploaded datasets.
#' It is designed to be flexible enough to handle continuous/continuous, discrete/continuous, and
#' discrete/discrete type of data to cover most use cases.
#'
#' @param nmd The NONMEM dataset for plotting (requires ID, TIME, DV at minimum)
#' @param rownums How many rows per page
#' @param colnums How many cols per page
#' @param pagenum Page number to filter by
#' @param filter_id Filter by these IDs
#' @param filter_cmt Filter by this CMT
#' @param sort_by Sort by one or more variables (a list), default NULL
#' @param strat_by Stratify by 1 variable and highlight outliers, default ''
#' @param highlight_range Flag potential outliers when they are higher or below this mean value of the group, a string
#' @param x_axis X-axis for plot
#' @param y_axis Y-axis for plot
#' @param color_by Color by this column
#' @param med_line When TRUE, will draw median line by equidistant X-axis bins
#' @param med_line_by Column name to draw median line by
#' @param boxplot Draws a geom_boxplot instead of geom_point and geom_line
#' @param dolm Insert linear regression with formula on top of plot
#' @param smoother Insert smoother
#' @param facet_name variable to facet by
#' @param logy Log Y-axis
#' @param lby  Log breaks for Y-axis
#' @param logx Log X-axis
#' @param lbx  Log breaks for X-axis
#' @param plot_title Optional plot title
#' @param label_size font size for geom_text labels (N=x for boxplots or linear regressions)
#' @param discrete_threshold Draws geom_count if there are <= this number of unique Y-values
#' @param boxplot_x_threshold Throws an error when the unique values of x-axis exceeds this number
#' @param error_text_color error text color for element text
#' @param highlight_var variable name to be highlighted by a different shape
#' @param highlight_var_values variable values associated with highlight_var to be highlighted
#' @param plot_dosing Plots dosing line and dose text when TRUE
#' @param same_scale Uses fixed scales based on entire dataset when TRUE
#' @param dose_col name of dose column, usually AMT or DOSE
#' @param dose_units (Optional) name of dose units, usually mg or nmol
#' @param lloq_name (Optional) Supply name of lloq column to be plotted as hline
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab theme_bw labs scale_size_area
#' @importFrom ggplot2 stat_summary stat_smooth scale_y_log10 scale_x_log10 after_stat geom_count
#' @importFrom ggplot2 annotation_logticks ggtitle theme facet_wrap geom_boxplot label_both
#' @importFrom ggplot2 scale_color_manual geom_rect coord_cartesian scale_fill_manual element_text element_blank
#' @importFrom scales hue_pal oob_squish_infinite
#' @importFrom dplyr filter distinct sym syms group_by summarise across all_of count ungroup mutate any_of case_when rowwise
#' @importFrom dplyr glimpse
#' @noRd
#-------------------------------------------------------------------------------

do_data_page_ind_plot <- function(nmd,
                                  rownums,
                                  colnums,
                                  pagenum             = 1,
                                  filter_id,
                                  filter_cmt,
                                  sort_by,
                                  strat_by,
                                  highlight_range,
                                  x_axis,
                                  y_axis,
                                  color_by,
                                  med_line,
                                  med_line_by,
                                  boxplot,
                                  dolm,
                                  smoother,
                                  facet_name          = "ID",
                                  logy,
                                  lby                 = logbreaks_y,
                                  lbx                 = logbreaks_x,
                                  logx,
                                  plot_title,
                                  label_size          = 3,
                                  discrete_threshold  = 7,
                                  boxplot_x_threshold = 20,
                                  error_text_color    = "#F8766D",
                                  highlight_var,
                                  highlight_var_values,
                                  plot_dosing         = TRUE,
                                  same_scale          = FALSE,
                                  dose_col            = "DOSE",
                                  dose_units,
                                  lloq_name           = '',
                                  debug               = FALSE) {
  
  if (debug) message("Creating data_page_ind_plot")
  
  # ── Helper to avoid repeated as.numeric(as.character(...)) ────────
  to_numeric <- function(x) as.numeric(as.character(x))
  
  nmd <- dplyr::ungroup(nmd)
  
  # ── Compute dosing flag once ─────────────────────────────────────
  has_evid      <- 'EVID' %in% names(nmd)
  do_dosing     <- plot_dosing && has_evid && !boxplot && dose_col != ""
  
  # ── CMT filter ────────────────────────────────────────────────────────────
  if (filter_cmt != 'NULL') {
    nmd <- if (do_dosing) {
      dplyr::filter(nmd, CMT %in% filter_cmt | EVID == 1 | EVID == 4)
    } else {
      dplyr::filter(nmd, CMT %in% filter_cmt)
    }
  }
  
  # ── Sorting and facet labels ───────────────────────────────────────────────
  if (length(sort_by) > 0) {
    nmd <- create_facet_label(df = nmd, sort_by = sort_by)
  } else {
    nmd <- dplyr::arrange(nmd, ID) %>%
      dplyr::mutate(facet_label = paste0("ID: ", ID))
  }
  
  if (!is.null(strat_by) && strat_by != '' && !boxplot)
    nmd <- categorize_outliers(df              = nmd,
                               highlight_range = highlight_range,
                               y_axis          = y_axis,
                               strat_by        = strat_by,
                               debug           = debug)
  
  nmd$facet_label <- factor(nmd$facet_label, levels = unique(nmd$facet_label))
  
  # ── Color setup once ──────────────────────────────────────────────
  has_color          <- color_by != ""
  named_color_vector <- NULL
  color_valid        <- FALSE
  
  if (has_color) {
    if (all(is.na(nmd[[color_by]]))) {
      shiny::showNotification(
        paste0("WARNING: All values are NA in ", color_by, ". No coloring performed."),
        type = "warning", duration = 10)
    } else {
      color_valid <- TRUE
      nmd         <- handle_blanks(nmd, color_by)
      nmd[[color_by]] <- as.factor(nmd[[color_by]])
      n <- nlevels(nmd[[color_by]])
      named_color_vector <- setNames(scales::hue_pal()(n), levels(nmd[[color_by]]))
    }
  }
  
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)
  
  # ── Extract column vectors for reuse ──────────────────────────────
  x_col        <- nmd[[x_axis]]
  y_col        <- nmd[[y_axis]]
  x_is_numeric <- is.numeric(x_col)
  y_is_numeric <- is.numeric(y_col)
  
  # ── Same-scale limits from full dataset ───────────────────────────────────
  nmdx <- if (!is.character(x_col)) dplyr::filter(nmd, !!dplyr::sym(x_axis) != 0) else nmd
  nmdy <- if (!is.character(y_col)) dplyr::filter(nmd, !!dplyr::sym(y_axis) != 0) else nmd
  
  min_data_x_all <- min(to_numeric(nmdx[[x_axis]]), na.rm = TRUE)
  max_data_x_all <- max(to_numeric(nmdx[[x_axis]]), na.rm = TRUE)
  min_data_y_all <- min(to_numeric(nmdy[[y_axis]]), na.rm = TRUE)
  max_data_y_all <- max(to_numeric(nmdy[[y_axis]]), na.rm = TRUE)
  
  # ── Dosing pre-processing ─────────────────────────────────────────────────
  if (do_dosing) {
    nmd$NAMT <- to_numeric(nmd[[dose_col]])
    nmd$XVAR <- to_numeric(nmd[[x_axis]])
    nmd$YVAR <- to_numeric(nmd[[y_axis]])
    max_dose_all <- max(nmd$NAMT, na.rm = TRUE)
    
    nmd <- nmd %>%
      dplyr::group_by(facet_label) %>%
      dplyr::mutate(
        max_dose = max(NAMT, na.rm = TRUE),
        min_xvar = round(min(XVAR, na.rm = TRUE), digits = 3),
        max_xvar = round(max(XVAR, na.rm = TRUE), digits = 3),
        min_yvar = min(YVAR, na.rm = TRUE),
        max_yvar = max(YVAR, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    nmd[(nmd == Inf | nmd == -Inf)] <- NA
    
    dose_height <- 0.5
    
    nmd <- nmd %>%
      dplyr::mutate(
        max_dose_all = max(NAMT, na.rm = TRUE),
        min_data_y   = min(YVAR, na.rm = TRUE),
        max_data_y   = max(YVAR, na.rm = TRUE),
        min_data_x   = min(XVAR, na.rm = TRUE),
        max_data_x   = max(XVAR, na.rm = TRUE),
        min_yvar     = dplyr::case_when(is.na(min_yvar) ~ min_data_y, TRUE ~ min_yvar),
        max_yvar     = dplyr::case_when(is.na(max_yvar) ~ max_data_y, TRUE ~ max_yvar),
        min_xvar     = dplyr::case_when(is.na(min_xvar) ~ min_data_x, TRUE ~ min_xvar),
        max_xvar     = dplyr::case_when(is.na(max_xvar) ~ max_data_x, TRUE ~ max_xvar),
        scaling_factor = if (same_scale) {
          max_data_y_all / max_dose_all * dose_height
        } else {
          ((max_yvar - min_yvar) * dose_height + min_yvar) / max_dose
        },
        SAMT = NAMT * scaling_factor
      )
  }
  
  if (same_scale)
    nmd <- dplyr::mutate(nmd,
                         min_xvar = min_data_x_all, max_xvar = max_data_x_all,
                         min_yvar = min_data_y_all, max_yvar = max_data_y_all)
  
  # ── Compute filter_id flag once ───────────────────────────────────
  no_id_filter <- is.null(filter_id[1]) || filter_id[1] == ''
  
  if (no_id_filter) {
    unique_ids  <- unique(nmd$ID)
    ids_this_page <- unique_ids[((rownums * colnums) * (pagenum - 1) + 1):
                                  ((rownums * colnums) * pagenum)]
    nmd <- dplyr::filter(nmd, ID %in% ids_this_page)
  } else {
    nmd <- dplyr::filter(nmd, ID %in% filter_id)
  }
  
  # ── Dosing expansion (post-page filter) ───────────────────────────────────
  id_dose_expand <- NULL
  id_dose_unique <- NULL
  
  if (do_dosing) {
    id_dose_expand <- dplyr::filter(nmd, EVID == 1 | EVID == 4)
    
    if (nrow(id_dose_expand) >= 1)
      id_dose_expand <- expand_addl_ii(id_dose_expand, x_axis = x_axis, dose_col = dose_col)
    
    if (debug) { message("Dosing expanded:"); dplyr::glimpse(id_dose_expand) }
    
    id_dose_unique <- dplyr::distinct(id_dose_expand, facet_label,
                                      !!dplyr::sym(dose_col), .keep_all = TRUE)
    id_dose_unique[[dose_col]] <- to_numeric(id_dose_unique[[dose_col]])
    id_dose_unique <- dplyr::mutate(id_dose_unique,
                                    dosename = paste0(round(.data[[dose_col]], digits = 2), dose_units))
  }
  
  if (has_evid) nmd <- dplyr::filter(nmd, EVID == 0)
  
  # Re-extract after filtering
  x_col <- nmd[[x_axis]]
  y_col <- nmd[[y_axis]]
  
  # ── Build base plot once ──────────────────────────────────────────
  make_aes <- function(include_group = TRUE) {
    if (color_valid) {
      if (include_group)
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                     group = ID, color = !!dplyr::sym(color_by))
      else
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                     color = !!dplyr::sym(color_by))
    } else {
      if (include_group)
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]], group = ID)
      else
        ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]])
    }
  }
  
  # ── Boxplot branch ────────────────────────────────────────────────────────
  if (boxplot) {
    if (length(unique(x_col)) > boxplot_x_threshold) {
      return(ggplot2::ggplot() +
               ggplot2::labs(title = paste0('ERROR: There are too many X-axis categories (>',
                                            boxplot_x_threshold, ') for boxplots.')) +
               ggplot2::theme(panel.background = ggplot2::element_blank(),
                              plot.title = ggplot2::element_text(color = error_text_color)))
    }
    
    nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    
    treat_y_as_discrete <- is.character(y_col) || length(unique(y_col)) <= discrete_threshold
    if (treat_y_as_discrete) {
      shiny::showNotification(
        paste0("WARNING: Treating Y-axis as discrete, or there are <=",
               discrete_threshold, " unique Y values."),
        type = "warning", duration = 10)
      nmd[[y_axis]] <- as.factor(nmd[[y_axis]])
    } else {
      nmd[[y_axis]] <- as.numeric(nmd[[y_axis]])
      max_y         <- max(nmd[[y_axis]], na.rm = TRUE)
    }
    
    # ── Distinct once ─────────────────────────────────────────────
    nmd_distinct <- dplyr::distinct(nmd, ID, .keep_all = TRUE)
    
    a <- ggplot2::ggplot(data = nmd_distinct, make_aes(include_group = FALSE))
    if (!is.null(named_color_vector))
      a <- a + ggplot2::scale_color_manual(values = named_color_vector)
    
    facet_same_as_x <- facet_name == x_axis
    
    if (facet_same_as_x)
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.",
                              type = "error", duration = 10)
    
    has_valid_facet <- facet_name != ""
    count_syms      <- if (has_valid_facet && !facet_same_as_x) list(dplyr::sym(facet_name)) else list()
    
    df_count <- if (treat_y_as_discrete) {
      dplyr::count(nmd_distinct, !!dplyr::sym(x_axis), !!dplyr::sym(y_axis), !!!count_syms)
    } else {
      dplyr::count(nmd_distinct, !!dplyr::sym(x_axis), !!!count_syms)
    }
    
    if (treat_y_as_discrete) {
      a <- a + ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                                 label = paste0(n), size = n, group = NULL),
          color = "black", vjust = 2, size = label_size)
    } else {
      a <- a + ggplot2::geom_boxplot(varwidth = TRUE)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = max_y * 1.02,
                                 label = paste0("N=", n), group = NULL),
          color = "black", vjust = 2, size = label_size)
    }
    
  } else {
    # ── Non-boxplot branch ────────────────────────────────────────────────
    a <- ggplot2::ggplot(data = nmd, make_aes(include_group = TRUE))
    if (!is.null(named_color_vector))
      a <- a + ggplot2::scale_color_manual(values = named_color_vector)
    
    if (do_dosing && !is.null(id_dose_expand) &&
        'DOSETIME' %in% colnames(id_dose_expand)) {
      
      id_dose_expand$INFDUR <- id_dose_expand$DOSETIME
      
      if ("RATE" %in% colnames(id_dose_expand) &&
          any(!is.na(id_dose_expand$RATE) & id_dose_expand$RATE > 0)) {
        id_dose_expand <- dplyr::mutate(id_dose_expand,
                                        INFDUR = dplyr::case_when(
                                          !is.na(RATE) & RATE > 0 ~ DOSETIME + (NAMT / RATE),
                                          TRUE                    ~ DOSETIME))
      }
      
      linecolour <- "#ED5C42A0"
      a <- a +
        ggplot2::geom_vline(data = id_dose_expand,
                            ggplot2::aes(xintercept = DOSETIME), alpha = 0.1) +
        ggplot2::geom_rect(data    = id_dose_expand,
                           mapping = ggplot2::aes(y = NULL, xmin = DOSETIME, xmax = INFDUR,
                                                  ymin = min_yvar, ymax = SAMT, group = ID),
                           fill = linecolour, color = linecolour, show.legend = FALSE) +
        ggplot2::geom_text(data    = id_dose_unique,
                           mapping = ggplot2::aes(x = .data[[x_axis]], y = SAMT,
                                                  label = dosename, group = NULL, color = NULL),
                           hjust = 0, vjust = 0, show.legend = FALSE, # hjust 0 and vjust 0 ensures text stays within plot margin
                           size = label_size, alpha = 0.8)
    }
    
    if (lloq_name != '' && lloq_name %in% colnames(nmd)) {
      nmd[[lloq_name]] <- to_numeric(nmd[[lloq_name]])
      a <- a + ggplot2::geom_hline(yintercept = unique(nmd[[lloq_name]]),
                                   colour = "orange", linetype = "dashed",
                                   linewidth = 0.5, alpha = 0.5)
    }
    
    a <- a +
      ggplot2::geom_point(data = nmd, alpha = 1) +
      ggplot2::geom_line(data  = nmd, alpha = 0.7)
    
    if (!is.null(highlight_var) && highlight_var != "" &&
        !is.null(highlight_var_values[1]) && highlight_var_values[1] != "") {
      nmd_highlight <- dplyr::filter(nmd, !!dplyr::sym(highlight_var) %in% highlight_var_values)
      a <- a + ggplot2::geom_point(data = nmd_highlight, shape = 8, size = 4,
                                   alpha = 1, color = "red")
    }
  }
  
  a <- a +
    ggplot2::xlab(x_axis) + ggplot2::ylab(y_axis) +
    ggplot2::theme_bw() + ggplot2::labs(color = color_by)
  
  # ── && for scalar + data directly instead of ggplot_build ────
  if (dolm && x_is_numeric && y_is_numeric && !boxplot) {
    df_stats <- lm_eqn(df = nmd, facet_name = facet_name, x = x_axis, y = y_axis)
    
    if (same_scale) {
      med_x <- (min_data_x_all + max_data_x_all) / 2
      max_y <- max_data_y_all
    } else {
      med_x <- (min(x_col, na.rm = TRUE) + max(x_col, na.rm = TRUE)) / 2
      max_y <- max(y_col, na.rm = TRUE)
    }
    
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), method = "lm",
                                  formula = y ~ x, se = FALSE,
                                  colour = "grey", show.legend = FALSE)
    if (label_size > 0)
      a <- a + ggplot2::geom_text(
        data    = df_stats,
        mapping = ggplot2::aes(label = label, x = med_x, y = max_y,
                               group = NULL, color = NULL),
        hjust = 0.5, vjust = 1, show.legend = FALSE, size = label_size)
  }
  
  # ── Facet_wrap with computed scale/filter args ────────────────────
  if (facet_name != "") {
    if (facet_name == x_axis) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.",
                              type = "error", duration = 10)
    } else {
      facet_scales <- if (same_scale) "fixed" else "free"
      if (no_id_filter) {
        a <- a + ggplot2::facet_wrap(~facet_label, nrow = rownums, ncol = colnums,
                                     scales = facet_scales)
      } else {
        a <- a + ggplot2::facet_wrap(~facet_label, scales = facet_scales)
      }
    }
  }
  
  if (logy && y_is_numeric)
    a <- a +
    ggplot2::scale_y_log10(breaks = logbreaks_y, labels = logbreaks_y, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "l")
  
  if (logx && x_is_numeric && !boxplot)
    a <- a +
    ggplot2::scale_x_log10(breaks = logbreaks_x, labels = logbreaks_x, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "b")
  
  if (same_scale) {
    cc_xlim <- if (do_dosing && !is.null(id_dose_expand) &&
                   'DOSETIME' %in% colnames(id_dose_expand)) {
      c(min(min_data_x_all, min(id_dose_expand$DOSETIME)),
        max(max_data_x_all, max(id_dose_expand$DOSETIME)))
    } else {
      c(min_data_x_all, max_data_x_all)
    }
    if (debug) {
      message(paste0("cc_ylim: ", min_data_y_all, " ", max_data_y_all))
      message(paste0("cc_xlim: ", cc_xlim))
    }
    a <- a + ggplot2::coord_cartesian(xlim = cc_xlim,
                                      ylim = c(min_data_y_all, max_data_y_all))
  }
  
  if (!is.null(plot_title))
    a <- a + ggplot2::ggtitle(plot_title)
  
  return(a)
}

#-------------------------------------------------------------------------------
#' @name lowerFn
#'
#' @title Function to support draw_correlation_plot
#'
#' @param data input df for ggplot
#' @param mapping mapping for ggplot
#' @param method plotting method for ggplot (default "lm")
#' @param ... other parameters to pass onto geoms
#'
#' @returns a ggplot object
#' @noRd
#-------------------------------------------------------------------------------

lowerFn <- function(data, mapping, method = "lm", ...) { ## Plots linear regression of the continuous
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_point(alpha = 0.4, size = 0.8) +
    ggplot2::geom_smooth(method = method,
                         formula = y ~ x,
                         color = "black",
                         se = FALSE,
                         ...)
  p
}

#-------------------------------------------------------------------------------
#' @name draw_correlation_plot
#'
#' @title Draw a correlation plot from a NONMEM-formatted dataset
#'
#' @param input_df Input dataframe
#' @param corr_variables Vector (string) of names to be used in the plot
#' @param color_sep Variable (string) name used for colour separator
#' @param catcov_threshold assumes a covariate is categorical if the unique values
#'                         in the covariate are less than this threshold (default 10),
#'                         should be less than nsubj that is available from the dataset
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom dplyr distinct select all_of
#' @importFrom ggplot2 theme_bw
#' @noRd
#-------------------------------------------------------------------------------

draw_correlation_plot <- function(input_df,
                                  corr_variables,
                                  color_sep = "",
                                  catcov_threshold = 10,
                                  debug = FALSE) {
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("The correlation plot requires the 'GGally' package. ",
         "Install it with install.packages(\"GGally\").", call. = FALSE)
  }
  if(debug) {
    message("Creating correlation plot")
  }
  corr_data_id <- input_df %>% dplyr::distinct(ID, .keep_all = TRUE)

  if(color_sep %in% names(corr_data_id)) {
    cov_columnsf <- unique(c(corr_variables, color_sep)) # add the colour separator
  } else {
    cov_columnsf <- c(corr_variables)
  }

  #corr_data_id_trimmed <- corr_data_id[, cov_columnsf] %>% as.data.frame() # strange error
  corr_data_id_trimmed <- dplyr::select(corr_data_id, dplyr::all_of(cov_columnsf))

  # Vectorised factor conversion — avoids repeated subsetting in a loop
  corr_data_id_trimmed[] <- lapply(corr_data_id_trimmed, function(col) {
    if (length(unique(col)) < catcov_threshold) as.factor(col) else col
  })
  
  # alpha as a FIXED parameter outside aes() — avoids spurious scale + legend
  mapping <- if (color_sep %in% names(corr_data_id)) {
    ggplot2::aes(color = as.factor(.data[[color_sep]]))
  } else {
    NULL
  }

  corr_plot <- GGally::ggpairs(
    corr_data_id_trimmed,
    mapping             = mapping,
    cardinality_threshold = 30,
    lower               = list(continuous = GGally::wrap(lowerFn, method = "lm", alpha = 0.4)),
    progress            = FALSE          # disabling the progress bar removes per-panel overhead
  ) +
    ggplot2::theme_bw()
  
  return(corr_plot)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name print_demog_plots
#'
#' @title Print demographics plots.
#'
#' @description
#' By default AGE, WT, and SEX are shown.
#'
#'
#' @param data         input dataframe, must contain SEX (Male == 0, Female == 1),
#'               AGE, WT
#'
#' @returns a cowplot ggplot object
#' @importFrom dplyr case_when
#' @importFrom cowplot get_legend plot_grid
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print_demog_plots <- function(data) {

  data <- data %>% dplyr::mutate(SEX = dplyr::case_when(SEX == 0 ~ "Male",
                                                        SEX == 1 ~ "Female"))

  p.age <- ggplot2::ggplot(data, ggplot2::aes(x = AGE, fill = SEX)) +
    ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(x = 'Age (years)', y = 'Count')

  if(max(data$AGE) < 18) {
    p.age <- p.age +
      ggplot2::geom_histogram(colour= "white", binwidth = 1) + ggplot2::facet_grid(. ~ SEX)
  } else {
    p.age <- p.age +
      ggplot2::geom_histogram(colour= "white", binwidth = 5) + ggplot2::facet_grid(. ~ SEX)
  }

  p.age <- p.age +
    ggplot2::theme(legend.position = "none") +  # Remove the legend
    ggplot2::theme(axis.title.y = ggplot2::element_blank())  # Remove the y-axis label

  p.wt <- ggplot2::ggplot(data, ggplot2::aes(x = WT, fill = SEX)) +
    ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(x = 'Weight (kg)', y = '')

  if(max(data$WT) < 75) {
    p.wt <- p.wt +
      ggplot2::geom_histogram(colour= "white", binwidth = 1) + ggplot2::facet_grid(. ~ SEX)
  } else {
    p.wt <- p.wt +
      ggplot2::geom_histogram(colour= "white", binwidth = 5) + ggplot2::facet_grid(. ~ SEX)
  }

  p.wt <- p.wt +
    ggplot2::theme(legend.position = "none") + # Remove the legend
    ggplot2::theme(axis.title.y = ggplot2::element_blank())  # Remove the y-axis label

  # Create a separate legend plot with custom colors
  legend_plot <- cowplot::get_legend(
    ggplot2::ggplot(data, ggplot2::aes(x = AGE, fill = SEX)) +
      ggplot2::geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      ggplot2::scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink"), name = "Sex") +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, title = NULL))  # Set the number of columns in the legend
  )

  # Combine the plots side by side
  plots <- cowplot::plot_grid(p.age, p.wt, ncol = 2)

  # Combine the plots and the shared legend using plot_grid()
  if(length(unique(data$SEX)) == 2) {
    combined_plot <- cowplot::plot_grid(
      plots,
      legend_plot,
      ncol = 1,
      rel_heights = c(1, 0.1)  # Adjust the relative heights of the plots and legend
    )
  } else { # Otherwise don't show legend
    combined_plot <- plots
  }

  # p.sex <-ggplot2::ggplot(data)+
  #   #add_watermark() +
  #   ggplot2::geom_bar(ggplot2::aes(x=SEX),colour="white",fill='grey40')+
  #   ggplot2::labs(x='Sex', y='Frequency')+
  #   ggplot2::theme_bw(base_size = 14)

  # Plotting EGFR - uncomment if required
  # p.egfr <- ggplot2::ggplot(data)+
  #   add_watermark() +
  #   ggplot2::geom_histogram(ggplot2::aes(x=EGFR),colour="white",fill='grey40',boundary=1)+ # boundary is used to nudge the bars
  #   ggplot2::labs(x='eGFR (ml/min/1.73 m2)', y='Frequency')+
  #   ggplot2::theme_bw(base_size = 14) +
  #   ggplot2::scale_x_continuous(lim=c(0,150),breaks=c(0,15,30,60,90,120,150))

  return(combined_plot)

} # End of print_demog_plots function

#-------------------------------------------------------------------------------
#' @name print_cov_plot
#'
#' @title Prints distribution of a custom covariate
#'
#' @param data         input dataframe, must contain SEX (Male == 0, Female == 1),
#'                AGE, WT
#' @param lo_percentile  Default 0.025, (2.5th percentile)
#' @param hi_percentile  Default 0.975, (97.5th percentile)
#' @returns a ggplot object
#' @noRd
#-------------------------------------------------------------------------------

print_cov_plot <- function(data, lo_percentile = 0.025, hi_percentile = 0.975) {
  x_string <- names(data)[1]

  data_median <- round(quantile(data[[x_string]], probs = 0.5), 1)
  data_lo     <- round(quantile(data[[x_string]], probs = lo_percentile), 1)
  data_hi     <- round(quantile(data[[x_string]], probs = hi_percentile), 1)

  title <- paste0("Median = ", data_median, ", [95%: ", data_lo, " - ", data_hi, "]")

  plot_object <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_string]])) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::geom_histogram(alpha = 0.3, fill = "#FFB600", color = "black") +
    ggplot2::geom_vline(xintercept = data_median, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = data_lo, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = data_hi, linetype = "dashed", alpha = 0.5) +
    # ggplot2::annotate(x = data_median, y = 0, label = paste0("\n",data_median), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    # ggplot2::annotate(x = data_lo, y = 0, label = paste0("\n",data_lo), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    # ggplot2::annotate(x = data_hi, y = 0, label = paste0("\n",data_hi), geom = "text", lineheight = 0.6, vjust = 1, hjust = 0, size = 3) +
    #ggplot2::geom_histogram(ggplot2::aes(y = ..density..), alpha = 0.4, fill = "black") +
    #geom_density(alpha = 0.1, fill = "red")
    ggplot2::labs(y = "Count",
                  caption = "solid line = median, dashed line = 95% range") +
    ggplot2::ggtitle(title)
  return(plot_object)
}

#-------------------------------------------------------------------------------
#' NOT CURRENTLY USED
#'
#' @name compare_dist_histogram
#'
#' @title Sanity check plots comparing newly created
#'                         distributions vs NONMEM dataset (or databases)
#'
#' @param df         Dataframe of newly created distribution
#' @param variable_name  Name of variable (string) to use for plotting histograms
#' @param variable_label Nice label of variable_name
#' @param lo_percentile  Default 0.025, (2.5th percentile)
#' @param hi_percentile  Default 0.975, (97.5th percentile)
#'
#' @returns a ggplot object
#' @importFrom dplyr case_when mutate filter
#' @noRd
#-------------------------------------------------------------------------------

compare_dist_histogram <- function(df, variable_name, variable_label,
                                   lo_percentile = 0.025, hi_percentile = 0.975) {

  df <- df %>% dplyr::mutate(SEX = dplyr::case_when(SEX == 0 ~ "Male",
                                                    SEX == 1 ~ "Female"))

  df_male   <- df %>% dplyr::filter(SEX == "Male")
  df_female <- df %>% dplyr::filter(SEX == "Female")

  df_male_median <- round(quantile(df_male[[variable_name]], probs = 0.5), 1)
  df_male_lo     <- round(quantile(df_male[[variable_name]], probs = lo_percentile), 1)
  df_male_hi     <- round(quantile(df_male[[variable_name]], probs = hi_percentile), 1)

  df_female_median <- round(quantile(df_female[[variable_name]], probs = 0.5), 1)
  df_female_lo     <- round(quantile(df_female[[variable_name]], probs = lo_percentile), 1)
  df_female_hi     <- round(quantile(df_female[[variable_name]], probs = hi_percentile), 1)

  range_text    <- (hi_percentile - lo_percentile) * 100

  plot_histo <- ggplot2::ggplot()+
    #add_watermark() +
    ggplot2::geom_histogram(data = df_male, ggplot2::aes(y=..count../sum(..count..) * 100, x= .data[[variable_name]]), fill='blue', binwidth = 5, alpha = 0.25) +
    ggplot2::geom_histogram(data = df_female, ggplot2::aes(y=..count../sum(..count..) * 100, x= .data[[variable_name]]), fill='red', binwidth = 5, alpha = 0.25) +
    ggplot2::geom_vline(xintercept = df_male_median, color = 'blue', alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_male_lo, color = 'blue', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_male_hi, color = 'blue', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_median, color = 'red', alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_lo, color = 'red', linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = df_female_hi, color = 'red', linetype = "dashed", alpha = 0.5) +
    ggplot2::annotate(x = df_male_median, y = -Inf, label = paste0("\n",df_male_median), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_male_lo, y = -Inf, label = paste0("\n",df_male_lo), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_male_hi, y = -Inf, label = paste0("\n",df_male_hi), geom = "text", color = 'blue', lineheight = 0.6, vjust = 0.8, size = 3) +
    ggplot2::annotate(x = df_female_median, y = -Inf, label = paste0("\n",df_female_median), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::annotate(x = df_female_lo, y = -Inf, label = paste0("\n",df_female_lo), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::annotate(x = df_female_hi, y = -Inf, label = paste0("\n",df_female_hi), geom = "text", color = 'red', lineheight = 0.6, vjust = 1.5, size = 3) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = variable_label, y='Percentage of Population (%)',
                  caption = paste0('Blue = Male, Red = Female, Solid Line = Median, Dashed Line = ', range_text, '% range')) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))

  return(plot_histo)
}

#-------------------------------------------------------------------------------
#' @name plot_data_with_nm
#'
#' @title Main plot for Simulation tab
#'
#' @param input_dataset1       simulated dataframe for Model 1
#' @param input_dataset2       simulated dataframe for Model 2
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param color_data_by        Color by a variable in the NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string (Model 1)
#' @param yvar_2               Name of Y-variable to be plotted, string (Model 2)
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param stat_summary_data_by Inserts median line by variable
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#' @param line_color_1         Color for Model 1
#' @param line_color_2         Color for Model 2
#'
#' @returns a ggplot object
#' @importFrom dplyr mutate
#' @importFrom scales oob_squish_infinite
#' @noRd
#-------------------------------------------------------------------------------

plot_data_with_nm <- function(
    input_dataset1           = NULL,
    input_dataset2           = NULL,
    nonmem_dataset           = NULL,
    color_data_by            = NULL,
    xvar                     = NULL,
    yvar                     = NULL,
    yvar_2                   = NULL,
    log_x_axis               = FALSE,
    log_x_ticks              = logbreaks_x,
    log_x_labels             = logbreaks_x,
    log_y_axis               = FALSE,
    log_y_ticks              = logbreaks_y,
    log_y_labels             = logbreaks_y,
    geom_point_sim_option    = FALSE,
    geom_point_data_option   = FALSE,
    stat_summary_data_option = FALSE,
    stat_summary_data_by     = NULL,
    nm_yvar                  = NULL,
    xlabel                   = xvar,
    ylabel                   = yvar,
    debug                    = FALSE,
    title                    = NULL,
    line_color_1             = "#F8766D",
    line_color_2             = "#7570B3") {
  
  if (debug) message("Running plot_data_with_nm()")
  
  # Start with an empty base; all geoms supply their own data/aes
  p1 <- ggplot2::ggplot()
  
  # Evaluate once, use everywhere
  use_color_by <- !is.null(color_data_by) &&
    !is.null(nonmem_dataset) &&
    color_data_by %in% names(nonmem_dataset)
  
  # ── Internal helper: add line + optional points for a dataset (Fix #5)
  add_line_geom <- function(p, data, yname, color, add_points) {
    aes_mapping <- ggplot2::aes(x = .data[[xvar]], y = .data[[yname]])
    p <- p + ggplot2::geom_line(data = data, aes_mapping, color = color)
    if (add_points)
      p <- p + ggplot2::geom_point(data = data, aes_mapping, color = color)
    p
  }
  
  # ── Nonmem dataset layer
  if (!is.null(nonmem_dataset)) {
    if ('EVID' %in% names(nonmem_dataset))
      nonmem_dataset <- dplyr::filter(nonmem_dataset, EVID == 0)
    
    if (use_color_by) {
      nonmem_dataset[[color_data_by]] <- as.factor(nonmem_dataset[[color_data_by]])
      
      nm_line_aes <- ggplot2::aes(x = .data[[xvar]], y = .data[[nm_yvar]],
                                  group = ID, color = .data[[color_data_by]])
    } else {
      nm_line_aes <- ggplot2::aes(x = .data[[xvar]], y = .data[[nm_yvar]], group = ID)
    }
    
    nm_color_arg <- if (use_color_by) NULL else list(color = "grey")
    
    p1 <- p1 + do.call(ggplot2::geom_line,
                       c(list(data = nonmem_dataset, mapping = nm_line_aes, alpha = 0.2),
                         nm_color_arg))
    
    if (geom_point_data_option)
      p1 <- p1 + do.call(ggplot2::geom_point,
                         c(list(data = nonmem_dataset, mapping = nm_line_aes, alpha = 0.2),
                           nm_color_arg))
    
    if (stat_summary_data_option) {
      # Extract xvar column once
      xvar_col    <- nonmem_dataset[[xvar]]
      nonmem_dataset <- dplyr::mutate(nonmem_dataset,
                                      binned_xvar = quantize(xvar_col,
                                                             levels = get_bin_times(xvar_col, bin_num = 20,
                                                                                    relative_threshold = 0.05)))
      
      if (is.null(stat_summary_data_by) || stat_summary_data_by == "") {
        p1 <- p1 + ggplot2::stat_summary(
          data    = nonmem_dataset,
          mapping = ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]]),
          fun     = median, geom = "line", colour = "black", alpha = 0.8)
      } else {
        p1 <- p1 + ggplot2::stat_summary(
          data    = nonmem_dataset,
          mapping = ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]],
                                 color = as.factor(.data[[stat_summary_data_by]])),
          fun     = median, geom = "line", alpha = 0.8)
      }
    }
  }
  
  # ── Sim dataset layers (using helper)
  if (!is.null(input_dataset1))
    p1 <- add_line_geom(p1, input_dataset1, yvar,   line_color_1, geom_point_sim_option)
  
  if (!is.null(input_dataset2))
    p1 <- add_line_geom(p1, input_dataset2, yvar_2, line_color_2, geom_point_sim_option)
  
  # ── Axis scaling
  if (log_x_axis) {
    p1 <- p1 +
      ggplot2::scale_x_log10(breaks = log_x_ticks, labels = log_x_labels, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p1 <- smart_x_axis(p1, xlabel = xlabel, debug = debug)
  }
  
  if (log_y_axis) {
    p1 <- p1 +
      ggplot2::scale_y_log10(breaks = log_y_ticks, labels = log_y_labels, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "l")
  }
  
  # ── Theme and labels
  p1 <- p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = if (use_color_by) "right" else "none")
  
  if (debug) message("plot_data_with_nm() OK")
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name plot_three_data_with_nm
#'
#' @title Main function for the plot in Parameter Sensitivity Analysis
#'
#' @param input_dataset_min    simulated dataframe for Min Parameter
#' @param input_dataset_mid    simulated dataframe for Mid Parameter
#' @param input_dataset_max    simulated dataframe for Max Parameter
#' @param param_name           Name of parameter, string
#' @param param_min_value      Min value of parameter, numeric
#' @param param_mid_value      Mid value of parameter, numeric
#' @param param_max_value      Max value of parameter, numeric
#' @param x_min                Min X-axis value for time intervals to filter datasets by
#' @param x_max                Max X-axis value for time intervals to filter datasets by
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param geom_ribbon_option   Plots AUC when TRUE
#' @param geom_vline_option    Plots time intervals as vertical lines when TRUE
#' @param stat_summary_data_option Plots median line of dataset
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#'
#' @returns a ggplot object
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom ggplot2 theme
#' @noRd
#-------------------------------------------------------------------------------

plot_three_data_with_nm <- function(
    input_dataset_min,
    input_dataset_mid,
    input_dataset_max,
    param_name = "ID", # dummy value
    param_min_value = -1, # dummy value
    param_mid_value = -2, # dummy value
    param_max_value = -3, # dummy value
    x_min = NULL,
    x_max = NULL,
    nonmem_dataset = NULL,
    xvar = 'TIMEADJ',
    yvar = NULL,
    log_x_axis = FALSE,
    log_x_ticks = logbreaks_x,
    log_x_labels= logbreaks_x,
    log_y_axis = FALSE,
    log_y_ticks = logbreaks_y,
    log_y_labels= logbreaks_y,
    geom_point_sim_option = FALSE,
    geom_point_data_option = FALSE,
    geom_ribbon_option = FALSE,
    geom_vline_option = FALSE,
    stat_summary_data_option = FALSE,
    nm_yvar = NULL,
    xlabel = xvar,
    ylabel = yvar,
    debug  = FALSE,
    title  = NULL
) {

  # Correct NULL check across multiple objects ──────────────────
  if (all(vapply(list(input_dataset_min, input_dataset_mid, input_dataset_max),
                 is.null, logical(1)))) return(NULL)
  
  # Single helper replaces three identical mutate blocks ─────────
  label_dataset <- function(df, id_label, param_value) {
    if (is.null(df)) return(NULL)
    dplyr::mutate(df, ID = id_label, "{param_name}" := param_value)
  }
  
  df_min <- label_dataset(input_dataset_min, "Min", param_min_value)
  df_mid <- label_dataset(input_dataset_mid, "Mid", param_mid_value)
  df_max <- label_dataset(input_dataset_max, "Max", param_max_value)
  
  combined_input <- dplyr::bind_rows(df_min, df_mid, df_max)
  combined_input[[param_name]] <- fct_inorder(
    as.factor(combined_input[[param_name]])
  )
  
  max_x_value <- max(combined_input[[xvar]])
  
  p1 <- ggplot2::ggplot(
    data    = combined_input,
    mapping = ggplot2::aes(x     = .data[[xvar]],
                           y     = .data[[yvar]],
                           group = ID,
                           color = !!dplyr::sym(param_name))
  )
  
  # ── Nonmem overlay ───────────────────────────────────────────────────────
  if (!is.null(nonmem_dataset)) {
    if ('EVID' %in% names(nonmem_dataset))
      nonmem_dataset <- dplyr::filter(nonmem_dataset, EVID == 0)
    
    nm_aes <- ggplot2::aes(x     = .data[[xvar]],
                           y     = .data[[nm_yvar]],
                           group = ID,
                           color = NULL)
    
    p1 <- p1 +
      ggplot2::geom_line(data = nonmem_dataset, nm_aes, color = 'grey', alpha = 0.2)
    
    if (geom_point_data_option)
      p1 <- p1 +
      ggplot2::geom_point(data = nonmem_dataset, nm_aes, color = 'grey', alpha = 0.2)
    
    if (stat_summary_data_option) {
      # ── Fix #7: extract column once ──────────────────────────────────────
      xvar_col       <- nonmem_dataset[[xvar]]
      nonmem_dataset <- dplyr::mutate(nonmem_dataset,
                                      binned_xvar = quantize(xvar_col,
                                                             levels = get_bin_times(xvar_col, bin_num = 20,
                                                                                    relative_threshold = 0.05)))
      p1 <- p1 +
        ggplot2::stat_summary(
          data    = nonmem_dataset,
          mapping = ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]],
                                 group = NULL, color = NULL),
          geom    = "line", fun = median, colour = "black", alpha = 0.8)
    }
  }
  
  # Compute ribbon data and add geoms only when needed ──────
  if (geom_ribbon_option) {
    ribbon_colors <- c("Min" = "#1B9E77", "Mid" = "#D95F02", "Max" = "#7570B3")
    ribbon_alphas <- c("Min" = 0.3,       "Mid" = 0.4,       "Max" = 0.5)
    
    ribbon_data <- dplyr::filter(combined_input,
                                 .data[[xvar]] >= x_min & .data[[xvar]] <= x_max)
    
    for (id_label in c("Min", "Mid", "Max")) {
      p1 <- p1 + ggplot2::geom_ribbon(
        data    = dplyr::filter(ribbon_data, ID == id_label),
        mapping = ggplot2::aes(ymax = .data[[yvar]], ymin = 0),
        alpha   = ribbon_alphas[[id_label]],
        fill    = ribbon_colors[[id_label]])
    }
  }
  
  # Use xvar instead of hardcoded "TIMEADJ" ─────────────────────
  if (geom_vline_option) {
    if (x_min > min(combined_input[[xvar]]) || x_max < max_x_value) {
      p1 <- p1 +
        ggplot2::geom_vline(xintercept = x_min, linetype = "longdash", alpha = 0.3) +
        ggplot2::geom_vline(xintercept = x_max, linetype = "longdash", alpha = 0.3)
    }
  }
  
  p1 <- p1 +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme(legend.position = "right")
  
  if (geom_point_sim_option)
    p1 <- p1 + ggplot2::geom_point(alpha = 0.7)
  
  if (log_x_axis) {
    p1 <- p1 +
      ggplot2::scale_x_log10(breaks = log_x_ticks, labels = log_x_labels, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p1 <- smart_x_axis(p1, xlabel = xlabel, max_x = max_x_value)
  }
  
  if (log_y_axis)
    p1 <- p1 +
    ggplot2::scale_y_log10(breaks = log_y_ticks, labels = log_y_labels, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "l")
  
  return(p1)
}

#-------------------------------------------------------------------------------
#' @name plot_iiv_data_with_nm
#'
#' @title Function to plot variability data
#'
#' @param input_dataset1       simulated dataframe for Model 1
#' @param input_dataset2       simulated dataframe for Model 2
#' @param nonmem_dataset       uploaded NONMEM dataset
#' @param xvar                 Name of X-variable to be plotted, string
#' @param yvar                 Name of Y-variable to be plotted, string (Model 1)
#' @param yvar_2               Name of Y-variable to be plotted, string (Model 2)
#' @param log_x_axis,log_x_ticks,log_x_labels,log_y_axis,log_y_ticks,log_y_labels
#'  Log options and axis tick labels
#' @param geom_point_sim_option,geom_point_data_option Plotting sampling points
#' @param stat_summary_data_option Plots median line of dataset
#' @param show_ind_profiles    If TRUE, plots individual profiles instead of prediction intervals
#' @param y_median             Prediction interval median (when show_ind_profiles == FALSE)
#' @param y_mean               Prediction interval mean (when show_ind_profiles == FALSE)
#' @param show_y_mean          Show Mean of Y-value in a dashed line
#' @param y_min                Prediction interval min (when show_ind_profiles == FALSE)
#' @param y_max                Prediction interval max (when show_ind_profiles == FALSE)
#' @param line_color_1         Color for Model 1 prediction intervals
#' @param line_color_2         Color for Model 2 prediction intervals
#' @param nm_yvar              Name of Y-variable for NONMEM dataset, string
#' @param xlabel               Nice label for X-axis, string
#' @param ylabel               Nice label for Y-axis, string
#' @param debug                When TRUE, displays debugging messages
#' @param title                Title for plot
#' @param show_x_intercept     Display X-intercept for threshold calculation
#' @param x_intercept_value    Numeric for X-intercept
#' @param show_y_intercept     Display Y-intercept for threshold calculation
#' @param y_intercept_value    Numeric for Y-intercept
#'
#' @returns a ggplot object
#' @importFrom dplyr select mutate all_of
#' @noRd
#-------------------------------------------------------------------------------

plot_iiv_data_with_nm <- function(
    input_dataset1 = NULL,
    input_dataset2 = NULL,
    nonmem_dataset = NULL,
    xvar = NULL,
    yvar = NULL,
    yvar_2 = NULL,
    log_x_axis = FALSE,
    log_x_ticks = logbreaks_x,
    log_x_labels= logbreaks_x,
    log_y_axis  = FALSE,
    log_y_ticks = logbreaks_y,
    log_y_labels= logbreaks_y,
    geom_point_sim_option = FALSE,
    geom_point_data_option = FALSE,
    show_ind_profiles = FALSE,
    y_median = NULL,
    y_mean = NULL,
    show_y_mean = FALSE,
    y_min = NULL,
    y_max = NULL,
    line_color_1 = 'black',
    line_color_2 = 'black',
    stat_summary_data_option = FALSE,
    nm_yvar = NULL,
    xlabel = xvar,
    ylabel = yvar,
    debug  = FALSE,
    title  = NULL,
    show_x_intercept = FALSE,
    x_intercept_value = NULL,
    show_y_intercept = FALSE,
    y_intercept_value = NULL
) {
  
  if (debug) {
    message("Running plot_iiv_data_with_nm()")
  }
  
  # Consistent namespacing; trim columns early ───────────────────
  sim_cols <- c("ID", "median_yvar", "mean_yvar", "lower_yvar", "upper_yvar")
  
  if (!is.null(input_dataset1)) {
    if (debug) message('dataset1 provided')
    input_dataset1 <- dplyr::select(input_dataset1,
                                    dplyr::all_of(c(sim_cols, xvar, yvar)))
  }
  
  if (!is.null(input_dataset2)) {
    if (debug) message('dataset2 provided')
    input_dataset2 <- dplyr::select(input_dataset2,
                                    dplyr::all_of(c(sim_cols, xvar, yvar_2)))
  }
  
  # Always start with empty base; all geoms supply their own data ─
  p1 <- ggplot2::ggplot()
  
  if (show_x_intercept && !is.null(x_intercept_value) && !is.na(x_intercept_value))
    p1 <- p1 + ggplot2::geom_vline(xintercept = x_intercept_value,
                                   linetype = "longdash", alpha = 0.3)
  
  if (show_y_intercept && !is.null(y_intercept_value) && !is.na(y_intercept_value))
    p1 <- p1 + ggplot2::geom_hline(yintercept = y_intercept_value,
                                   linetype = "longdash", alpha = 0.3)
  
  # ── Nonmem overlay ────────────────────────────────────────────────────────
  if (!is.null(nonmem_dataset)) {
    if ('EVID' %in% names(nonmem_dataset))
      nonmem_dataset <- dplyr::filter(nonmem_dataset, EVID == 0)
    
    nonmem_dataset <- dplyr::select(nonmem_dataset,
                                    dplyr::all_of(c("ID", xvar, nm_yvar)))
    
    nm_aes <- ggplot2::aes(x = .data[[xvar]], y = .data[[nm_yvar]], group = ID)
    
    p1 <- p1 +
      ggplot2::geom_line(data = nonmem_dataset, nm_aes, color = 'grey', alpha = 0.2)
    
    if (geom_point_data_option)
      p1 <- p1 +
      ggplot2::geom_point(data = nonmem_dataset, nm_aes, color = 'grey', alpha = 0.2)
    
    if (stat_summary_data_option) {
      # Extract column once ────────────────────────────────────────
      xvar_col       <- nonmem_dataset[[xvar]]
      nonmem_dataset <- dplyr::mutate(nonmem_dataset,
                                      binned_xvar = quantize(xvar_col,
                                                             levels = get_bin_times(xvar_col, bin_num = 20,
                                                                                    relative_threshold = 0.05)))
      p1 <- p1 +
        ggplot2::stat_summary(
          data    = nonmem_dataset,
          mapping = ggplot2::aes(x = binned_xvar, y = .data[[nm_yvar]]),
          fun     = median, geom = "line", colour = "black", alpha = 0.7)
    }
  }
  
  # Helper replaces duplicated dataset1/dataset2 layer blocks ─────
  add_sim_layers <- function(p, dataset, yvar_name, line_color) {
    if (is.null(dataset)) return(p)
    
    if (!show_ind_profiles) {
      p <- p +
        ggplot2::geom_ribbon(
          data    = dataset,
          mapping = ggplot2::aes(x    = .data[[xvar]],
                                 ymax = .data[[y_max]],
                                 ymin = .data[[y_min]]),
          fill  = line_color, alpha = 0.4, color = line_color) +
        ggplot2::geom_line(
          data    = dataset,
          mapping = ggplot2::aes(x = .data[[xvar]], y = .data[[y_median]]),
          color   = line_color, linewidth = 1.2)
    } else {
      p <- p +
        ggplot2::geom_line(
          data    = dataset,
          mapping = ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_name]],
                                 group = ID), # one line per subject
          color = line_color, alpha = 0.4)    # one colour per model
    }
    
    if (show_y_mean)
      p <- p +
        ggplot2::geom_line(
          data      = dataset,
          mapping   = ggplot2::aes(x = .data[[xvar]], y = .data[[y_mean]]),
          color     = line_color, linewidth = 1.2, linetype = "dashed")
    p
  }
  
  p1 <- add_sim_layers(p1, input_dataset1, yvar,   line_color_1)
  p1 <- add_sim_layers(p1, input_dataset2, yvar_2, line_color_2)
  
  # ── Axis scaling ──────────────────────────────────────────────────────────
  if (log_x_axis) {
    p1 <- p1 +
      ggplot2::scale_x_log10(breaks = log_x_ticks, labels = log_x_labels, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p1 <- smart_x_axis(p1, xlabel = xlabel)
  }
  
  if (log_y_axis)
    p1 <- p1 +
    ggplot2::scale_y_log10(breaks = log_y_ticks, labels = log_y_labels, oob = scales::oob_squish_infinite) +
    ggplot2::annotation_logticks(sides = "l")
  
  p1 <- p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "none")
  
  if (debug) message('iiv plot generated')
  
  return(p1)
}

#=============================================================================
#' @name quantize
#'
#' @title Used for dividing bins for median line plots
#' @param x vector of values (e.g. x-axis)
#' @param levels bins
#' @param ... extra params to pass through to cut()
#' @noRd
#=============================================================================

quantize <- function (x, levels, ...) {
  stopifnot(!anyNA(levels), is.numeric(levels), is.numeric(x))
  midpoints <- (head(levels, -1) + tail(levels, -1))/2
  breaks <- c(-Inf, midpoints, Inf)
  idx <- cut(x, breaks, labels = FALSE, ...)
  levels[idx]
}

#-------------------------------------------------------------------------------
#' @name binary_cat_dist
#' @title Function to generate a binary categorical covariate
#'
#' @param n         number of subjects
#' @param percent   percent of subjects (approximate) in first category
#' @param catvalue1 numeric value for first category
#' @param catvalue2 numeric value for second category
#'
#' @returns a numeric vector
#' @noRd
#-------------------------------------------------------------------------------
binary_cat_dist <- function(n = 20, percent = 50, catvalue1 = 1, catvalue2 = 0) {

  # Calculate the number of 1s and 2s
  n_in_first  <- round(n * percent/100)
  n_in_second <- n - n_in_first

  # Generate the vector
  vec <- sample(c(rep(catvalue1, n_in_first), rep(catvalue2, n_in_second)))

  return(vec)
}

#-------------------------------------------------------------------------------
#' @name plot_iiv_exp_data
#'
#' @title Function to plot variability exposure data
#'
#' @param input_dataset        Input dataset of Model 1 and/or Model 2 that contains MODEL ID CMIN CAVG CMAX AUC
#' @param yvar                 Name of Y-variable (exposure metric) to be plotted, string
#' @param ylab                 Optional name for yvar
#' @param model_1_name         Optional name for Model 1
#' @param model_2_name         Optional name for Model 2
#' @param model_1_color        Color for Model 1
#' @param model_2_color        Color for Model 2
#' @param show_stats           Display texts of stats for each box plot
#' @param xlab                 Optional name for xvar
#' @param title                Title for plot
#'
#' @returns a ggplot object
#' @importFrom dplyr select mutate all_of summarise
#' @importFrom ggplot2 theme_bw geom_boxplot scale_fill_manual geom_text
#' @noRd
#-------------------------------------------------------------------------------

plot_iiv_exp_data <- function(input_dataset,
                              yvar = 'yvar',
                              ylab = yvar,
                              model_1_name = '',
                              model_2_name = '',
                              model_1_color = "#F8766D",
                              model_2_color = "#7570B3",
                              show_stats = TRUE,
                              xlab = '',
                              title = "") {

  input_dataset$MODEL <- gsub("Model 1", model_1_name, input_dataset$MODEL)
  input_dataset$MODEL <- gsub("Model 2", model_2_name, input_dataset$MODEL)
  input_dataset$MODEL <- fct_inorder(input_dataset$MODEL)

  # Define the colors for the models
  model_colors <- c()

  # Check if "Model 1" exists in the data
  if (model_1_name %in% input_dataset$MODEL) {
    model_colors[model_1_name] <- model_1_color
  }

  # Check if "Model 2" exists in the data
  if (model_2_name %in% input_dataset$MODEL) {
    model_colors[model_2_name] <- model_2_color
  }

  p <- ggplot2::ggplot(data = input_dataset, ggplot2::aes(x = MODEL, y = .data[[yvar]], group = MODEL, fill = MODEL)) +
    ggplot2::theme_bw() +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::scale_fill_manual(values = model_colors)

  if(show_stats) {
    # Calculate statistics for each group
    stats_df <- input_dataset %>%
      dplyr::group_by(MODEL) %>%
      dplyr::summarise(maximum = max(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       upper975 = quantile(.data[[yvar]], probs = 0.975, na.rm = TRUE) %>% round(digits = 2),
                       upper95  = quantile(.data[[yvar]], probs = 0.95, na.rm = TRUE) %>% round(digits = 2),
                       mean     = mean(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       median   = median(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2),
                       lower025 = quantile(.data[[yvar]], probs = 0.025, na.rm = TRUE) %>% round(digits = 2),
                       lower05  = quantile(.data[[yvar]], probs = 0.05, na.rm = TRUE) %>% round(digits = 2),
                       minimum  = min(.data[[yvar]], na.rm = TRUE) %>% round(digits = 2)) %>%
      dplyr::ungroup()

    p <- p +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = maximum, label = paste("Max:", maximum)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = upper95, label = paste("95%:", upper95)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = mean,    label = paste("Mean:", mean)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = median,  label = paste("Median:", median)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = lower05, label = paste("5%:", lower05)), vjust = -1) +
      ggplot2::geom_text(data = stats_df, ggplot2::aes(x = MODEL, y = minimum, label = paste("Min:", minimum)), vjust = 1)

    # p <- p +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = maximum, label = paste("Max:", maximum))) +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = upper95, label = paste("95%:", upper95))) +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = mean,    label = paste("Mean:", mean))) +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = median,  label = paste("Median:", median))) +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = lower05, label = paste("5%:", lower05))) +
    #   ggrepel::geom_text_repel(data = stats_df, ggplot2::aes(x = MODEL, y = minimum, label = paste("Min:", minimum)))

  }

  if(ylab != '') {
    p <- p + ggplot2::labs(x = "", y = ylab)
  } else {
    p <- p + ggplot2::labs(x = "", y = yvar)
  }

  p <- p +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "none")

  return(p)
}

#-------------------------------------------------------------------------------
#' @name categorize_xvar
#'
#' @title Function that categorizes X-axis variable according to the quantile df
#'
#' @param df              Name of dataframe containing columns corresponding to number of quantiles
#' @param quantiles_df    Name of dataframe containing the quantiles limits
#' @param xvar            Name of x-axis variable to split by
#'
#' @importFrom dplyr case_when filter mutate sym syms summarise
#' @returns a character string containing the quantile limits
#' @noRd
#-------------------------------------------------------------------------------

categorize_xvar <- function(df, quantiles_df, xvar) {

  # Check for any blanks / NAs in X-axis
  # if(any(is.na(df[[xvar]]))) {
  #   df <- df %>% filter(!is.na(!!dplyr::sym(xvar)))
  #   shiny::showNotification(paste0("WARNING: Some ", xvar, " values are NA and are removed prior to plotting."), type = "warning", duration = 10)
  # }

  # print("before rename quantiles_df:")
  # print(knitr::kable(quantiles_df))

  # Rename the quantile df to have limits inserted as part of the column name
  for(i in seq_along(quantiles_df)) {
    ending_bracket   <- "]" # Note that [ ] means inclusive intervals, ( ) means exclusive intervals
    if(i == 1) { # Special case for first quantile
      starting_bracket <- "\n["
      names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(min(df[[xvar]], na.rm = TRUE)), "-", round(quantiles_df[[1,1]]), ending_bracket)
    } else {
      starting_bracket <- "\n("
      #names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(quantiles_df[,..i-1]), "-", round(quantiles_df[,..i]), ending_bracket)
      names(quantiles_df)[i] <- paste0(names(quantiles_df)[i], starting_bracket, round(quantiles_df[[1,i-1]]), "-", round(quantiles_df[[1,i]]), ending_bracket)
    }
  }

  # print("after rename quantiles_df:")
  # print(knitr::kable(quantiles_df))

  conditions <- Map(
    function(x, y) rlang::expr(!!dplyr::sym(xvar) <= !!x ~ !!y),
    quantiles_df,
    names(quantiles_df)
  )

  conditions <- c(rlang::expr(is.na(!!dplyr::sym(xvar)) ~ "NA"), conditions)

  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(!!!conditions)) # triple-bang for list

  # Rename NAs
  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(is.na(Quantile) ~ "NA", TRUE ~ Quantile))

  # Reorder factor levels to place "NA" first
  df <- df %>%
    dplyr::mutate(Quantile = fct_relevel(Quantile, "NA", names(quantiles_df)))

  return(df)
}

#-------------------------------------------------------------------------------
#' @name categorize_outliers
#'
#' @title Function that categorizes outliers relative to a supplied stratification
#' group
#'
#' @param df              Name of input dataframe
#' @param highlight_range A character string in percentage e.g. "80%" to define outlier threshold
#' @param y_axis          Y-axis to derive the arithmetic mean for ID and group
#' @param strat_by        Column name to be used as a stratification variable for group
#' @param debug           Show debugging messages
#'
#' @importFrom dplyr group_by sym summarise ungroup left_join mutate case_when
#' @returns a dataframe with the new column called "outlier_status", with three categories: "Above", "Below", "Within"
#' @noRd
#-------------------------------------------------------------------------------

categorize_outliers <- function(df,
                                highlight_range,
                                y_axis,
                                strat_by,
                                debug = FALSE) {

  # Remove the percentage sign and convert to numeric
  highlight_numeric <- as.numeric(gsub("%", "", highlight_range)) / 100

  if(!is.character(df[[y_axis]])) {
    group_means <- df %>%
      dplyr::group_by(!!dplyr::sym(strat_by)) %>%
      dplyr::summarise(meanYVARGRP = mean(!!dplyr::sym(y_axis))) %>%
      dplyr::ungroup()

    id_means <- df %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(meanYVARID = mean(!!dplyr::sym(y_axis))) %>%
      dplyr::ungroup()

    # Join it back to main df
    df <- dplyr::left_join(df, group_means, by = strat_by)
    df <- dplyr::left_join(df, id_means,    by = "ID")

    df <- df %>%
      dplyr::mutate(outlier_status = dplyr::case_when(
        meanYVARID > (meanYVARGRP * (1 + highlight_numeric)) ~ "Above",
        meanYVARID < (meanYVARGRP * (1 - highlight_numeric)) ~ "Below",
        TRUE                                                 ~ "Within")
      )
  } else { # Do nothing if Y-axis is of character type
    df <- df %>%
      dplyr::mutate(outlier_status = "Within")
  }

  df <- df %>%
    dplyr::mutate(facet_label = dplyr::case_when(
      outlier_status == "Above" ~ paste0("**>", highlight_numeric * 100,"%** ", facet_label), # paste0(facet_label, " [>", highlight_numeric * 100, "%]")
      outlier_status == "Below" ~ paste0("**<", (1 - highlight_numeric) * 100,"%** ", facet_label), # paste0(facet_label, " [<", (1 - highlight_numeric) * 100, "%]")
      outlier_status == "Within"~ facet_label
    ))

  if(debug) {
    message("categorize_outliers done")
    # Check levels in the data
    #print(levels(nmd$outlier_status))
  }

  return(df)
}

#-------------------------------------------------------------------------------
#' @name get_bin_times
#'
#' @title Function that derives bin times of x_axis
#' group
#'
#' @param dfcol           Input dataframe column of x-axis
#' @param bin_num         Maximum number of bins, integer
#' @param relative_threshold relative threshold of lumping bins that are close together
#'
#' @returns a vector of unique bin times
#' @noRd
#-------------------------------------------------------------------------------

get_bin_times <- function(dfcol, bin_num = 20, relative_threshold = 0.05) {

  # If number of unique times are less than 20, use that instead
  if(length(unique(as.numeric(dfcol))) < bin_num) {bin_num <- length(unique(as.numeric(dfcol)))}

  # Determine the unique bin boundaries based on quantiles
  bin_times <- unique(quantile(as.numeric(dfcol), probs = seq(0, 1, length.out = bin_num + 1), na.rm = TRUE))

  # Initialize the lumped bin times
  lumped_bin_times <- c(bin_times[1])  # Start with the first value

  # Iterate through the bin times
  for (i in 2:length(bin_times)) {
    # Check the relative difference between the current value and the last value in lumped_bin_times
    if (abs(bin_times[i] / lumped_bin_times[length(lumped_bin_times)] - 1) > relative_threshold) {
      # If the relative difference exceeds the threshold, add the current value to lumped_bin_times
      lumped_bin_times <- c(lumped_bin_times, bin_times[i])
    }
  }

  return(lumped_bin_times)
}

#-------------------------------------------------------------------------------
#' @name draw_histogram_plot
#'
#' @title Draw a histogram plot from a NONMEM-formatted dataset for covariates
#'
#' @param input_df Input dataframe
#' @param hist_variables Vector (string) of names to be used in the plot
#' @param bin_size Size of bin_width used for histogram
#' @param debug show debugging messages
#'
#' @returns a ggplot object
#' @importFrom dplyr distinct select all_of mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 theme_bw after_stat geom_histogram geom_density labs
#' @importFrom ggplot2 facet_wrap aes
#' @noRd
#-------------------------------------------------------------------------------

draw_histogram_plot <- function(input_df,
                                hist_variables,
                                bin_size = 5,
                                debug = FALSE) {
  if(debug) {
    message("Creating histogram plot for variables: ", paste(hist_variables, collapse = ", "))
  }
  
  hist_data_id <- input_df %>% dplyr::distinct(ID, .keep_all = TRUE)
  
  # Coerce specified variables to numeric
  hist_data_id <- hist_data_id %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(hist_variables),
        .fns = ~ as.numeric(.),
        .names = "{col}"
      )
    )
  
  # Reshape data to long format for multiple variables
  hist_data_long <- hist_data_id %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(hist_variables),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create the histogram plot with facets for each variable
  hist_plot <- ggplot2::ggplot(hist_data_long, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(
      binwidth = bin_size,
      fill = "skyblue",
      color = "white",
      alpha = 1
    ) +
    ggplot2::facet_wrap(~Variable, scales = "free") + # Facet by variable
    ggplot2::theme_bw()
  
  return(hist_plot)
}
