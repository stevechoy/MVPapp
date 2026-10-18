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
#' @param logx Log X-axis
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
  
  # ── Quantize: a continuous x-axis becomes quantile bins (shown as boxplots) ──
  if (num_quantiles > 0) {
    if (is.numeric(nmd[[x_axis_orig]])) {
      boxplot      <- TRUE
      can_quantize <- TRUE
      nmd_q  <- calculate_quantiles(df = nmd, xvar = x_axis_orig, num_quantiles = num_quantiles)
      if (debug) message_object(nmd_q)
      nmd    <- categorize_xvar(df = nmd, quantiles_df = nmd_q, xvar = x_axis_orig)
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
                            type = "message", duration = 10, id = "note_general_plot_evid")
  }
  
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)
  
  has_facet       <- !is.null(facet_name[1]) && facet_name[1] != ""
  facet_same_as_x <- has_facet && x_axis %in% facet_name
  if (has_facet) {
    for (facet in facet_name[facet_name != x_axis])
      nmd <- handle_blanks(nmd, facet)
  }
  
  x_is_numeric   <- is.numeric(nmd[[x_axis]])
  y_is_numeric   <- is.numeric(nmd[[y_axis]])
  can_draw_lines <- x_is_numeric && y_is_numeric && !boxplot
  
  if (can_quantize && color_by == x_axis_orig) color_by <- "Quantile"
  col <- setup_color(nmd, color_by)
  nmd <- col$data
  
  # ── Boxplot branch ────────────────────────────────────────────────────────
  if (boxplot) {
    if (length(unique(nmd[[x_axis]])) > boxplot_x_threshold) {
      return(too_many_categories_plot(boxplot_x_threshold, error_text_color,
                                      hint = '\nTry the "Quantize X-axis" option instead.'))
    }
    
    if (num_quantiles == 0) nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    
    y_col <- nmd[[y_axis]]
    treat_y_as_discrete <- is.character(y_col) || length(unique(y_col)) <= discrete_threshold
    if (treat_y_as_discrete) {
      shiny::showNotification(
        paste0("WARNING: Treating Y-axis as discrete as it is a character type, or there are <=",
               discrete_threshold, " unique Y values."),
        type = "warning", duration = 10)
      nmd[[y_axis]] <- as.factor(nmd[[y_axis]])
    } else {
      nmd[[y_axis]] <- as.numeric(nmd[[y_axis]])
      max_y <- max(nmd[[y_axis]], na.rm = TRUE) # count labels sit just above the highest value
    }
    
    # One value per subject, unless the bins are quantiles of observations
    if (!can_quantize) nmd <- dplyr::distinct(nmd, ID, .keep_all = TRUE)
    
    a <- ggplot2::ggplot(data = nmd, xy_aes(x_axis, y_axis, col$color_by, group = FALSE))
    
    # Counts per box (and per facet)
    count_syms <- if (has_facet && !facet_same_as_x) dplyr::syms(facet_name) else list()
    if (treat_y_as_discrete) {
      df_count <- dplyr::count(nmd, !!dplyr::sym(x_axis), !!dplyr::sym(y_axis), !!!count_syms)
      a <- a + ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                                 label = paste0(n), size = n, group = NULL),
          color = "black", vjust = 0.5, size = label_size)
    } else {
      df_count <- dplyr::count(nmd, !!dplyr::sym(x_axis), !!!count_syms)
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
    a <- ggplot2::ggplot(data = nmd, xy_aes(x_axis, y_axis, col$color_by)) +
      ggplot2::geom_point(alpha = 0.2) + ggplot2::geom_line(alpha = 0.2)
  }
  
  if (!is.null(col$values)) a <- a + ggplot2::scale_color_manual(values = col$values)
  a <- a +
    ggplot2::xlab(x_label) +
    ggplot2::ylab(y_axis) +
    ggplot2::theme_bw()
  if (!is.null(col$color_by)) a <- a + ggplot2::labs(color = col$color_by) # only when colour is mapped
  
  # Median line over x-axis bins (so that unevenly timed samples line up)
  if (med_line && can_draw_lines) {
    nmd$binned_xvar <- quantize(nmd[[x_axis]], levels = get_bin_times(nmd[[x_axis]], bin_num = 20, relative_threshold = 0.05))
    a <- a + if (med_line_by == "") {
      ggplot2::stat_summary(data = nmd, mapping = ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL),
                            fun = median, geom = "line", colour = "black", alpha = 1.0)
    } else {
      ggplot2::stat_summary(data = nmd, mapping = ggplot2::aes(x = binned_xvar, y = .data[[y_axis]], group = NULL,
                                                               color = as.factor(.data[[med_line_by]])),
                            fun = median, geom = "line", alpha = 1.0)
    }
  }
  
  if (smoother && can_draw_lines)
    a <- a + ggplot2::stat_smooth(ggplot2::aes(group = NULL), se = FALSE, linetype = "dashed")
  
  if (dolm && can_draw_lines) {
    x_range <- range(nmd[[x_axis]], na.rm = TRUE)
    a <- add_lm_layers(a, lm_eqn(df = nmd, facet_name = facet_name, x = x_axis, y = y_axis),
                       label_x = sum(x_range) / 2, label_y = max(nmd[[y_axis]], na.rm = TRUE), label_size = label_size)
  }
  
  if (facet_same_as_x) {
    shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.", type = "error", duration = 10)
  } else if (has_facet) {
    num_facets <- prod(sapply(facet_name, function(v) length(unique(nmd[[v]]))))
    if (num_facets > 30) {
      shiny::showNotification(
        "ERROR: Too many facets (>30) found. Please filter further or choose another variable(s).",
        type = "error", duration = 10)
    } else {
      # vars() with symbols copes with non-syntactic names such as "Dose group"
      a <- a + ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(facet_name)), labeller = ggplot2::label_both)
    }
  }
  
  a <- add_log_axes(a, logx = logx && x_is_numeric && !boxplot, logy = logy && y_is_numeric)
  
  if (!is.null(plot_title))
    a <- a + ggplot2::ggtitle(plot_title)
  
  a
}

#-------------------------------------------------------------------------------
#' @name setup_color
#'
#' @title Colour mapping for the data exploration plots
#'
#' @param nmd       dataset
#' @param color_by  column to colour by, or "" for none
#'
#' @returns a list: `data` (the column turned into a factor), `color_by`
#'   (NULL when there is no colouring, e.g. all values NA) and `values` (a named
#'   palette for scale_color_manual(), or NULL)
#' @noRd
#-------------------------------------------------------------------------------
setup_color <- function(nmd, color_by) {
  none <- list(data = nmd, color_by = NULL, values = NULL)
  if (is.null(color_by) || color_by == "") return(none)
  if (all(is.na(nmd[[color_by]]))) {
    shiny::showNotification(paste0("WARNING: All values are NA in ", color_by, ". No coloring performed."),
                            type = "warning", duration = 10)
    return(none)
  }
  nmd <- handle_blanks(nmd, color_by)
  nmd[[color_by]] <- as.factor(nmd[[color_by]])
  lev <- levels(nmd[[color_by]])
  list(data = nmd, color_by = color_by, values = stats::setNames(scales::hue_pal()(length(lev)), lev))
}

#-------------------------------------------------------------------------------
#' @name xy_aes
#'
#' @title x/y mapping, optionally grouped by ID and coloured by a column
#'
#' @param x,y       column names
#' @param color_by  column name, or NULL
#' @param group     group by ID (one line per subject)
#'
#' @returns a ggplot2 mapping
#' @noRd
#-------------------------------------------------------------------------------
xy_aes <- function(x, y, color_by = NULL, group = TRUE) {
  extra <- list()
  if (group)              extra$group  <- rlang::sym("ID")
  if (!is.null(color_by)) extra$colour <- rlang::sym(color_by)
  ggplot2::aes(x = .data[[x]], y = .data[[y]], !!!extra)
}

#-------------------------------------------------------------------------------
#' @name too_many_categories_plot
#'
#' @title Empty plot with an error title, used instead of a boxplot with too many boxes
#'
#' @param threshold         maximum number of x-axis categories
#' @param error_text_color  title colour
#' @param hint              optional text appended to the title
#'
#' @returns a ggplot object
#' @noRd
#-------------------------------------------------------------------------------
too_many_categories_plot <- function(threshold, error_text_color, hint = "") {
  ggplot2::ggplot() +
    ggplot2::labs(title = paste0('ERROR: There are too many X-axis categories (>', threshold, ') for boxplots.', hint)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(color = error_text_color))
}

#-------------------------------------------------------------------------------
#' @name add_lm_layers
#'
#' @title Linear regression line(s) and their equation labels
#'
#' @param p           ggplot object
#' @param df_stats    labels from lm_eqn() (one row per facet)
#' @param label_x     x position of the labels
#' @param label_y     y position of the labels
#' @param label_size  label size; 0 draws no labels
#'
#' @returns the ggplot object
#' @noRd
#-------------------------------------------------------------------------------
add_lm_layers <- function(p, df_stats, label_x, label_y, label_size) {
  p <- p + ggplot2::stat_smooth(ggplot2::aes(group = NULL), method = "lm", formula = y ~ x, se = FALSE,
                                colour = "grey", show.legend = FALSE)
  if (label_size > 0) {
    p <- p + ggplot2::geom_text(data = df_stats,
                                mapping = ggplot2::aes(label = label, x = label_x, y = label_y, group = NULL, color = NULL),
                                hjust = 0.5, vjust = 1, show.legend = FALSE, size = label_size)
  }
  p
}

#-------------------------------------------------------------------------------
#' @name add_log_axes
#'
#' @title Log10 x- and/or y-axis with log tick marks
#'
#' @param p     ggplot object
#' @param logx  log x-axis
#' @param logy  log y-axis
#'
#' @returns the ggplot object
#' @noRd
#-------------------------------------------------------------------------------
add_log_axes <- function(p, logx = FALSE, logy = FALSE) {
  if (logx) {
    p <- p +
      ggplot2::scale_x_log10(breaks = logbreaks_x, labels = logbreaks_x, minor_breaks = log_minor_breaks, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "b")
  }
  if (logy) {
    p <- p +
      ggplot2::scale_y_log10(breaks = logbreaks_y, labels = logbreaks_y, minor_breaks = log_minor_breaks, oob = scales::oob_squish_infinite) +
      ggplot2::annotation_logticks(sides = "l")
  }
  p
}

#-------------------------------------------------------------------------------
#' @name add_observed_layers
#'
#' @title Observed data (e.g. a NONMEM dataset) drawn behind simulated profiles
#'
#' @description
#' Grey lines per subject (or coloured by `color_by`), optional points, and an
#' optional median line over x-axis bins (so that unevenly timed samples line
#' up). Dosing records (EVID != 0) are left out. The layers do not inherit the
#' plot's own mapping.
#'
#' @param p             ggplot object
#' @param data          observed dataset, or NULL
#' @param xvar,yvar     x and y columns
#' @param color_by      column to colour the lines by, or NULL for grey
#' @param show_points   add points
#' @param show_median   add the median line
#' @param median_by     column to draw one median line per value of, or NULL / ""
#' @param median_alpha  transparency of the median line
#'
#' @returns the ggplot object
#' @noRd
#-------------------------------------------------------------------------------
add_observed_layers <- function(p, data, xvar, yvar, color_by = NULL, show_points = FALSE,
                                show_median = FALSE, median_by = NULL, median_alpha = 0.8) {
  if (is.null(data)) return(p)
  if ("EVID" %in% names(data)) data <- dplyr::filter(data, EVID == 0)
  
  if (is.null(color_by)) {
    mapping <- ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], group = ID)
    fixed   <- list(color = "grey")
  } else {
    data[[color_by]] <- as.factor(data[[color_by]])
    mapping <- ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], group = ID, color = .data[[color_by]])
    fixed   <- list()
  }
  layer_args <- c(list(data = data, mapping = mapping, alpha = 0.2, inherit.aes = FALSE), fixed)
  p <- p + do.call(ggplot2::geom_line, layer_args)
  if (show_points) p <- p + do.call(ggplot2::geom_point, layer_args)
  
  if (show_median) {
    data$binned_xvar <- quantize(data[[xvar]], levels = get_bin_times(data[[xvar]], bin_num = 20, relative_threshold = 0.05))
    p <- p + if (is.null(median_by) || median_by == "") {
      ggplot2::stat_summary(data = data, mapping = ggplot2::aes(x = binned_xvar, y = .data[[yvar]]),
                            fun = median, geom = "line", colour = "black", alpha = median_alpha, inherit.aes = FALSE)
    } else {
      ggplot2::stat_summary(data = data, mapping = ggplot2::aes(x = binned_xvar, y = .data[[yvar]],
                                                                color = as.factor(.data[[median_by]])),
                            fun = median, geom = "line", alpha = median_alpha, inherit.aes = FALSE)
    }
  }
  p
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
#' @param boxplot Draws a geom_boxplot instead of geom_point and geom_line
#' @param dolm Insert linear regression with formula on top of plot
#' @param facet_name variable to facet by
#' @param logy Log Y-axis
#' @param logx Log X-axis
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
                                  boxplot,
                                  dolm,
                                  facet_name          = "ID",
                                  logy,
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
  
  to_numeric <- function(x) as.numeric(as.character(x))
  
  nmd       <- dplyr::ungroup(nmd)
  has_evid  <- 'EVID' %in% names(nmd)
  do_dosing <- plot_dosing && has_evid && !boxplot && dose_col != ""
  
  # ── CMT filter (dosing records are kept for the dose bars) ─────────────────
  if (filter_cmt != 'NULL') {
    nmd <- if (do_dosing) {
      dplyr::filter(nmd, CMT %in% filter_cmt | EVID == 1 | EVID == 4)
    } else {
      dplyr::filter(nmd, CMT %in% filter_cmt)
    }
  }
  
  # ── One panel per subject, labelled and sorted by the sort_by columns ──────
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
  
  col <- setup_color(nmd, color_by)
  nmd <- col$data
  nmd <- handle_blanks(nmd, y_axis)
  nmd <- handle_blanks(nmd, x_axis)
  
  x_is_numeric <- is.numeric(nmd[[x_axis]])
  y_is_numeric <- is.numeric(nmd[[y_axis]])
  
  # ── Axis limits over all subjects (zeros left out), used with same_scale ────
  non_zero <- function(v) if (is.character(v)) v else v[!is.na(v) & v != 0]
  x_lim <- range(to_numeric(non_zero(nmd[[x_axis]])), na.rm = TRUE)
  y_lim <- range(to_numeric(non_zero(nmd[[y_axis]])), na.rm = TRUE)
  
  # ── Dose bars: heights proportional to the dose, the largest reaching half of
  #    the panel's y range (the whole dataset's with same_scale), drawn from the
  #    panel's lowest value ──────────────────────────────────────────────────────
  if (do_dosing) {
    dose_height <- 0.5
    nmd$NAMT <- to_numeric(nmd[[dose_col]])
    nmd$YVAR <- to_numeric(nmd[[y_axis]])
    max_dose_all <- max(nmd$NAMT, na.rm = TRUE)
    
    finite_or <- function(x, fallback) ifelse(is.finite(x), x, fallback)
    nmd <- nmd %>%
      dplyr::group_by(facet_label) %>%
      dplyr::mutate(max_dose = suppressWarnings(max(NAMT, na.rm = TRUE)),
                    min_yvar = suppressWarnings(min(YVAR, na.rm = TRUE)),
                    max_yvar = suppressWarnings(max(YVAR, na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      # a panel without values uses the whole dataset's range
      dplyr::mutate(max_dose = finite_or(max_dose, NA),
                    min_yvar = finite_or(min_yvar, min(YVAR, na.rm = TRUE)),
                    max_yvar = finite_or(max_yvar, max(YVAR, na.rm = TRUE)))
    
    nmd$SAMT <- nmd$NAMT * if (same_scale) {
      y_lim[2] / max_dose_all * dose_height
    } else {
      ((nmd$max_yvar - nmd$min_yvar) * dose_height + nmd$min_yvar) / nmd$max_dose
    }
    if (same_scale) nmd$min_yvar <- y_lim[1]
  }
  
  # ── Subjects on this page (or the selected IDs) ───────────────────────────────
  no_id_filter <- is.null(filter_id[1]) || filter_id[1] == ''
  if (no_id_filter) {
    per_page <- rownums * colnums
    page_ids <- unique(nmd$ID)[(per_page * (pagenum - 1) + 1):(per_page * pagenum)]
    nmd <- dplyr::filter(nmd, ID %in% page_ids)
  } else {
    nmd <- dplyr::filter(nmd, ID %in% filter_id)
  }
  
  # ── Dosing records, with additional doses (ADDL) written out ─────────────────
  id_dose_expand <- NULL
  id_dose_unique <- NULL
  if (do_dosing) {
    id_dose_expand <- dplyr::filter(nmd, EVID == 1 | EVID == 4)
    if (nrow(id_dose_expand) >= 1)
      id_dose_expand <- expand_addl_ii(id_dose_expand, x_axis = x_axis, dose_col = dose_col)
    if (debug) { message("Dosing expanded:"); dplyr::glimpse(id_dose_expand) }
    
    # one text label per dose amount and panel
    id_dose_unique <- dplyr::distinct(id_dose_expand, facet_label, !!dplyr::sym(dose_col), .keep_all = TRUE)
    id_dose_unique[[dose_col]] <- to_numeric(id_dose_unique[[dose_col]])
    id_dose_unique$dosename    <- paste0(round(id_dose_unique[[dose_col]], digits = 2), dose_units)
  }
  
  if (has_evid) nmd <- dplyr::filter(nmd, EVID == 0)
  
  # ── Boxplot branch ────────────────────────────────────────────────────────
  if (boxplot) {
    if (length(unique(nmd[[x_axis]])) > boxplot_x_threshold) {
      return(too_many_categories_plot(boxplot_x_threshold, error_text_color))
    }
    
    nmd[[x_axis]] <- as.factor(nmd[[x_axis]])
    
    y_col <- nmd[[y_axis]]
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
    
    nmd_distinct <- dplyr::distinct(nmd, ID, .keep_all = TRUE) # one value per subject
    a <- ggplot2::ggplot(data = nmd_distinct, xy_aes(x_axis, y_axis, col$color_by, group = FALSE))
    
    # Counts per box (and per facet)
    count_syms <- if (facet_name != "" && facet_name != x_axis) list(dplyr::sym(facet_name)) else list()
    if (treat_y_as_discrete) {
      df_count <- dplyr::count(nmd_distinct, !!dplyr::sym(x_axis), !!dplyr::sym(y_axis), !!!count_syms)
      a <- a + ggplot2::geom_count() + ggplot2::scale_size_area(max_size = 12)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]],
                                 label = paste0(n), size = n, group = NULL),
          color = "black", vjust = 2, size = label_size)
    } else {
      df_count <- dplyr::count(nmd_distinct, !!dplyr::sym(x_axis), !!!count_syms)
      a <- a + ggplot2::geom_boxplot(varwidth = TRUE)
      if (label_size > 0)
        a <- a + ggplot2::geom_text(
          data    = df_count,
          mapping = ggplot2::aes(x = .data[[x_axis]], y = max_y * 1.02,
                                 label = paste0("N=", n), group = NULL),
          color = "black", vjust = 2, size = label_size)
    }
    
  } else {
    # ── Profiles, with dose bars, LLOQ line and highlighted points ──────────────
    a <- ggplot2::ggplot(data = nmd, xy_aes(x_axis, y_axis, col$color_by))
    
    if (do_dosing && !is.null(id_dose_expand) && 'DOSETIME' %in% colnames(id_dose_expand)) {
      # an infusion's bar spans the infusion (start to start + amount / rate)
      rate     <- if ("RATE" %in% colnames(id_dose_expand)) id_dose_expand$RATE else NA_real_
      duration <- ifelse(!is.na(rate) & rate > 0, id_dose_expand$NAMT / rate, 0)
      id_dose_expand$INFDUR <- id_dose_expand$DOSETIME + duration
      
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
      a <- a + ggplot2::geom_hline(yintercept = unique(to_numeric(nmd[[lloq_name]])),
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
  
  if (!is.null(col$values)) a <- a + ggplot2::scale_color_manual(values = col$values)
  a <- a +
    ggplot2::xlab(x_axis) + ggplot2::ylab(y_axis) +
    ggplot2::theme_bw()
  if (!is.null(col$color_by)) a <- a + ggplot2::labs(color = col$color_by) # only when colour is mapped
  
  if (dolm && x_is_numeric && y_is_numeric && !boxplot && !logx && !logy) { # lm label is on the linear scale
    # One label per panel: panels are split by facet_label (one per individual)
    lm_facet <- if (facet_name != "" && facet_name != x_axis) "facet_label" else ""
    x_range  <- if (same_scale) x_lim else range(nmd[[x_axis]], na.rm = TRUE)
    a <- add_lm_layers(a, lm_eqn(df = nmd, facet_name = lm_facet, x = x_axis, y = y_axis),
                       label_x    = sum(x_range) / 2,
                       label_y    = if (same_scale) y_lim[2] else max(nmd[[y_axis]], na.rm = TRUE),
                       label_size = label_size)
  }
  
  if (facet_name != "") {
    if (facet_name == x_axis) {
      shiny::showNotification("ERROR: Facet variable cannot be the same as X-axis.",
                              type = "error", duration = 10)
    } else {
      facet_scales <- if (same_scale) "fixed" else "free"
      a <- a + if (no_id_filter) {
        ggplot2::facet_wrap(~facet_label, nrow = rownums, ncol = colnums, scales = facet_scales)
      } else {
        ggplot2::facet_wrap(~facet_label, scales = facet_scales)
      }
    }
  }
  
  a <- add_log_axes(a, logx = logx && x_is_numeric && !boxplot, logy = logy && y_is_numeric)
  
  if (same_scale) {
    # the shared x-range also covers all dose times
    cc_xlim <- if (do_dosing && !is.null(id_dose_expand) && 'DOSETIME' %in% colnames(id_dose_expand)) {
      range(c(x_lim, id_dose_expand$DOSETIME))
    } else {
      x_lim
    }
    if (debug) message("cc_xlim: ", paste(cc_xlim, collapse = " "), "; cc_ylim: ", paste(y_lim, collapse = " "))
    a <- a + ggplot2::coord_cartesian(xlim = cc_xlim, ylim = y_lim)
  }
  
  if (!is.null(plot_title))
    a <- a + ggplot2::ggtitle(plot_title)
  
  a
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
    lower               = list(continuous = GGally::wrap(lowerFn, method = "lm", alpha = 0.4),
                               combo      = GGally::wrap("facethist", bins = 30)), # explicit bins avoids stat_bin() messages
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
#' @param log_x_axis,log_y_axis Log x- / y-axis
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
    log_y_axis               = FALSE,
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
  
  use_color_by <- !is.null(color_data_by) && !is.null(nonmem_dataset) && color_data_by %in% names(nonmem_dataset)
  
  p1 <- ggplot2::ggplot() %>%
    add_observed_layers(nonmem_dataset, xvar, nm_yvar,
                        color_by     = if (use_color_by) color_data_by,
                        show_points  = geom_point_data_option,
                        show_median  = stat_summary_data_option,
                        median_by    = stat_summary_data_by)
  
  # Simulated profiles: a line (and optional points) per model
  add_sim_line <- function(p, data, yname, color) {
    if (is.null(data)) return(p)
    mapping <- ggplot2::aes(x = .data[[xvar]], y = .data[[yname]])
    p <- p + ggplot2::geom_line(data = data, mapping, color = color)
    if (geom_point_sim_option) p <- p + ggplot2::geom_point(data = data, mapping, color = color)
    p
  }
  p1 <- add_sim_line(p1, input_dataset1, yvar,   line_color_1)
  p1 <- add_sim_line(p1, input_dataset2, yvar_2, line_color_2)
  
  if (!log_x_axis) {
    # Axis maximum from the data rather than from an expensive ggplot_build()
    max_x_all <- max_x_of(xvar, input_dataset1, input_dataset2, nonmem_dataset)
    p1 <- smart_x_axis(p1, xlabel = xlabel, max_x = if (is.finite(max_x_all)) max_x_all else NULL, debug = debug)
  }
  p1 <- add_log_axes(p1, logx = log_x_axis, logy = log_y_axis)
  
  show_legend <- use_color_by || (isTRUE(stat_summary_data_option) && !is.null(stat_summary_data_by) && nzchar(stat_summary_data_by))
  p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = if (show_legend) "right" else "none")
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
#' @param log_x_axis,log_y_axis Log x- / y-axis
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
    log_y_axis = FALSE,
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

  if (is.null(input_dataset_min) && is.null(input_dataset_mid) && is.null(input_dataset_max)) return(NULL)
  
  # The three runs, labelled Min / Mid / Max and by the parameter value used
  label_dataset <- function(df, id_label, param_value) {
    if (is.null(df)) return(NULL)
    dplyr::mutate(df, ID = id_label, "{param_name}" := param_value)
  }
  combined_input <- dplyr::bind_rows(label_dataset(input_dataset_min, "Min", param_min_value),
                                     label_dataset(input_dataset_mid, "Mid", param_mid_value),
                                     label_dataset(input_dataset_max, "Max", param_max_value))
  combined_input[[param_name]] <- fct_inorder(as.factor(combined_input[[param_name]]))
  
  x_values <- combined_input[[xvar]]
  
  p1 <- ggplot2::ggplot(
    data    = combined_input,
    mapping = ggplot2::aes(x     = .data[[xvar]],
                           y     = .data[[yvar]],
                           group = ID,
                           color = !!dplyr::sym(param_name))
  ) %>%
    add_observed_layers(nonmem_dataset, xvar, nm_yvar,
                        show_points = geom_point_data_option,
                        show_median = stat_summary_data_option)
  
  # Shaded areas under each curve within [x_min, x_max]
  if (geom_ribbon_option) {
    ribbon_colors <- c("Min" = "#1B9E77", "Mid" = "#D95F02", "Max" = "#7570B3")
    ribbon_alphas <- c("Min" = 0.3,       "Mid" = 0.4,       "Max" = 0.5)
    ribbon_data   <- dplyr::filter(combined_input, .data[[xvar]] >= x_min & .data[[xvar]] <= x_max)
    for (id_label in c("Min", "Mid", "Max")) {
      p1 <- p1 + ggplot2::geom_ribbon(
        data    = dplyr::filter(ribbon_data, ID == id_label),
        mapping = ggplot2::aes(ymax = .data[[yvar]], ymin = 0),
        alpha   = ribbon_alphas[[id_label]],
        fill    = ribbon_colors[[id_label]])
    }
  }
  
  # Window bounds, unless the window is the whole simulation
  if (geom_vline_option && (x_min > min(x_values) || x_max < max(x_values))) {
    p1 <- p1 +
      ggplot2::geom_vline(xintercept = x_min, linetype = "longdash", alpha = 0.3) +
      ggplot2::geom_vline(xintercept = x_max, linetype = "longdash", alpha = 0.3)
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
  
  if (!log_x_axis) p1 <- smart_x_axis(p1, xlabel = xlabel, max_x = max(x_values))
  add_log_axes(p1, logx = log_x_axis, logy = log_y_axis)
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
#' @param log_x_axis,log_y_axis Log x- / y-axis
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
    log_y_axis  = FALSE,
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
  
  if (debug) message("Running plot_iiv_data_with_nm()")
  
  p1 <- ggplot2::ggplot()
  
  if (show_x_intercept && !is.null(x_intercept_value) && !is.na(x_intercept_value))
    p1 <- p1 + ggplot2::geom_vline(xintercept = x_intercept_value, linetype = "longdash", alpha = 0.3)
  
  if (show_y_intercept && !is.null(y_intercept_value) && !is.na(y_intercept_value))
    p1 <- p1 + ggplot2::geom_hline(yintercept = y_intercept_value, linetype = "longdash", alpha = 0.3)
  
  p1 <- add_observed_layers(p1, nonmem_dataset, xvar, nm_yvar,
                            show_points  = geom_point_data_option,
                            show_median  = stat_summary_data_option,
                            median_alpha = 0.7)
  
  # Per model: the prediction interval and median (or one line per subject),
  # plus an optional dashed mean line
  add_sim_layers <- function(p, dataset, yvar_name, line_color) {
    if (is.null(dataset)) return(p)
    
    if (!show_ind_profiles) {
      p <- p +
        ggplot2::geom_ribbon(
          data    = dataset,
          mapping = ggplot2::aes(x = .data[[xvar]], ymax = .data[[y_max]], ymin = .data[[y_min]]),
          fill  = line_color, alpha = 0.4, color = line_color) +
        ggplot2::geom_line(
          data    = dataset,
          mapping = ggplot2::aes(x = .data[[xvar]], y = .data[[y_median]]),
          color   = line_color, linewidth = 1.2)
    } else {
      p <- p +
        ggplot2::geom_line(
          data    = dataset,
          mapping = ggplot2::aes(x = .data[[xvar]], y = .data[[yvar_name]], group = ID),
          color = line_color, alpha = 0.4)
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
  
  if (!log_x_axis) {
    max_x_all <- max_x_of(xvar, input_dataset1, input_dataset2)
    p1 <- smart_x_axis(p1, xlabel = xlabel, max_x = if (is.finite(max_x_all)) max_x_all else NULL)
  }
  p1 <- add_log_axes(p1, logx = log_x_axis, logy = log_y_axis)
  
  p1 +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlabel, y = ylabel) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(legend.position = "none")
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

  # Identical model names would merge both models into one box
  if (identical(model_1_name, model_2_name)) {
    model_1_name <- paste0(model_1_name, " (Model 1)")
    model_2_name <- paste0(model_2_name, " (Model 2)")
  }
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
#' @returns df with a `Quantile` factor column ("NA" first, then the bins in order)
#' @noRd
#-------------------------------------------------------------------------------

categorize_xvar <- function(df, quantiles_df, xvar) {
  x     <- df[[xvar]]
  upper <- unlist(quantiles_df[1, ], use.names = FALSE)
  lower <- c(min(x, na.rm = TRUE), upper[-length(upper)])

  # Labels such as "Q1\n[40-59]", "Q2\n(59-80]": the first bin includes its lower limit
  labels <- paste0(names(quantiles_df), "\n", c("[", rep("(", length(upper) - 1)),
                   round(lower), "-", round(upper), "]")

  # Each value goes in the first bin whose upper limit it does not exceed
  bin <- findInterval(x, upper, left.open = TRUE) + 1
  df$Quantile <- fct_relevel(ifelse(is.na(x), "NA", labels[bin]), "NA", labels)
  df
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
    # Means are taken over observation records only (dosing rows usually have
    # DV = NA or 0), ignoring missing values
    df_obs <- if ("EVID" %in% names(df)) dplyr::filter(df, EVID == 0) else df

    group_means <- df_obs %>%
      dplyr::group_by(!!dplyr::sym(strat_by)) %>%
      dplyr::summarise(meanYVARGRP = mean(!!dplyr::sym(y_axis), na.rm = TRUE)) %>%
      dplyr::ungroup()

    id_means <- df_obs %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(meanYVARID = mean(!!dplyr::sym(y_axis), na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Join it back to main df
    df <- dplyr::left_join(df, group_means, by = strat_by)
    df <- dplyr::left_join(df, id_means,    by = "ID")

    df <- df %>%
      dplyr::mutate(outlier_status = dplyr::case_when(
        !is.na(meanYVARID) & meanYVARID > (meanYVARGRP * (1 + highlight_numeric)) ~ "Above",
        !is.na(meanYVARID) & meanYVARID < (meanYVARGRP * (1 - highlight_numeric)) ~ "Below",
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

  x <- as.numeric(dfcol)
  x <- x[is.finite(x)]

  # A single time (e.g. a trough-only dataset) or no usable time gives at
  # most one bin, and there is nothing to lump
  n_unique <- length(unique(x))
  if(n_unique <= 1) {
    return(unique(x))
  }

  # If number of unique times are less than 20, use that instead
  if(n_unique < bin_num) {bin_num <- n_unique}

  # Determine the unique bin boundaries based on quantiles
  bin_times <- unname(unique(quantile(x, probs = seq(0, 1, length.out = bin_num + 1))))

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

#-------------------------------------------------------------------------------
#' Print individual-plot pages with the same panel size on every page
#'
#' ggplot2 drops empty rows of a facet_wrap, so a last page with fewer
#' subjects than rows x columns would stretch its panels over the whole page.
#' Here a page with fewer panel rows is drawn in a shorter, top-aligned area
#' (the rest of the page stays white), so each panel keeps the size it has on
#' a full page.
#'
#' @param plots list of ggplot objects, one per page
#' @param rownums number of panel rows on a full page
#' @param on_page function(i, n) called before page i of n is drawn (progress)
#' @noRd
#-------------------------------------------------------------------------------
print_ind_pages <- function(plots, rownums, on_page = function(i, n) NULL) {
  # Panel rows on a page, and the height (inches) of everything that is not a
  # panel: titles, strips, axes, margins. Panel rows share the rest equally.
  page_layout <- function(g) {
    is_panel  <- grepl("^panel", g$layout$name)
    is_fixed  <- as.character(grid::unitType(g$heights)) != "null"
    fixed_in  <- sum(grid::convertHeight(g$heights[is_fixed], "inches", valueOnly = TRUE))
    list(rows = length(unique(g$layout$t[is_panel])), fixed = fixed_in)
  }
  
  grobs   <- lapply(plots, ggplot2::ggplotGrob)
  layouts <- lapply(grobs, page_layout)
  
  # A full page is the reference; with only one (short) page nothing to compare
  full <- Filter(function(l) l$rows >= rownums, layouts)
  ref_fixed <- if (length(full) > 0) full[[1]]$fixed else NULL
  
  for (i in seq_along(grobs)) {
    on_page(i, length(grobs))
    if (i > 1) grid::grid.newpage() # a fresh PDF device already starts on page 1
    page_in <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
    l <- layouts[[i]]
    height_in <- page_in
    if (!is.null(ref_fixed) && l$rows < rownums) {
      # height of one panel row on a full page, times the rows used here
      panel_in  <- (page_in - ref_fixed) / rownums
      height_in <- min(page_in, l$fixed + l$rows * panel_in)
    }
    grid::pushViewport(grid::viewport(y = 1, just = "top", height = grid::unit(height_in, "inches")))
    grid::grid.draw(grobs[[i]])
    grid::popViewport()
  }
  invisible(plots)
}
