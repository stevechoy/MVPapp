#' @name today_numeric
#' @title Get today's date as YYYYMMDD as seed input
#'
#' @returns A numeric
#' @examples
#' \dontrun{
#' today_numeric()
#' }
#' @importFrom magrittr %>% %<>% %T>% %$%
#' @export
today_numeric  <- function() {
  format(Sys.Date(), "%Y%m%d") %>% as.numeric()
}

#---------------------------------------------------------------------------
#' @name lm_eqn_old
#' @title Linear regression text (not used)
#'
#' @param df A dataframe
#' @param x column name (string) of x-variable
#' @param y column name (string) of y-variable
#' @importFrom stats as.formula coef lm
#' @returns A character string containing the expression of the R2 coefficient
#' for use in ggplot labels or title
#---------------------------------------------------------------------------

lm_eqn_old <- function(df, x, y){

  # Special handling of whether df is an object or a character string
  if(is.data.frame(df)) {
    df_plot <- df
    df_name <- deparse(substitute(df)) # Stores name of dataframe as string
  } else {
    df_plot <- get(df) # I.e. removes the quotation marks of the string to get the object
    df_name <- df
  }

  string.name <- paste0(y, "~", x)
  m <- lm(as.formula(string.name), df_plot)

  if(is.na(coef(m)[2])) {
    eq <- "" # no slope available
  } else {
    if(coef(m)[2] < 0) {
      # eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,
      #                  list(a = format(unname(coef(m)[1]), digits = 2),
      #                       b = format(unname(abs(coef(m)[2])), digits = 2),
      #                       r2 = format(summary(m)$r.squared, digits = 3)))

      a <- format(unname(coef(m)[1]), digits = 2)
      b <- format(unname(abs(coef(m)[2])), digits = 2)
      r2 <- format(summary(m)$r.squared, digits = 3)

      eq <- paste0("y = ", a, " - ", b, "\u00B7x, r\u00B2 = ", r2) # plotly unicode hack

    } else {
      # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
      #                  list(a = format(unname(coef(m)[1]), digits = 2),
      #                       b = format(unname(coef(m)[2]), digits = 2),
      #                       r2 = format(summary(m)$r.squared, digits = 3)))

      a <- format(unname(coef(m)[1]), digits = 2)
      b <- format(unname(coef(m)[2]), digits = 2)
      r2 <- format(summary(m)$r.squared, digits = 3)

      eq <- paste0("y = ", a, " + ", b, "\u00B7x, r\u00B2 = ", r2) # plotly unicode hack
    }
    #eq <- as.character(as.expression(eq)) # uncomment for normal ggplot
  }

  return(eq)
}

#---------------------------------------------------------------------------
#' @name lm_eqn
#' @title Linear regression text (for plotly)
#'
#' @param df A dataframe
#' @param facet_name Optional column name (string) to facet by
#' @param x column name (string) of x-variable
#' @param y column name (string) of y-variable
#'
#' @returns A dataframe containing the column "label" containing the string
#' formula of R2 coefficient for use in ggplot labels or title
#' @importFrom dplyr group_by summarise if_else sym syms case_when n
#' @export
#---------------------------------------------------------------------------

lm_eqn <- function(df, facet_name, x, y) {
  if(!is.null(facet_name) && facet_name[1] != "") {
    df_stats <- df %>%
      dplyr::group_by(!!!dplyr::syms(facet_name))
  } else {
    df_stats <- df
  }

  string_name <- paste0(y, "~", x)

  if(x == y) { # workaround for when same variable is used for both y and x,
    # which returns an NA slope. It seems summarise can't handle this well
    df_stats <- df_stats %>%
      dplyr::summarise(rsq = 1,
                       slope = 1,
                       intercept = 0)
  } else {

    df_stats <- df_stats %>%
      dplyr::summarise(
        rsq = if(any(!is.na(!!dplyr::sym(y)))) { # safeguard for when there are no valid y-values to calculate lm
          summary(lm(as.formula(string_name)))$r.squared
        } else {
          NA_real_
        },
        slope = if(any(!is.na(!!dplyr::sym(y)))) {
          coef(lm(as.formula(string_name)))[2]
        } else {
          NA_real_
        },
        intercept = if(any(!is.na(!!dplyr::sym(y)))) {
          coef(lm(as.formula(string_name)))[1]
        } else {
          NA_real_
        }
      )
  }

  df_stats <- df_stats %>%
    dplyr::mutate(slope_direction = dplyr::if_else(slope >= 0, " + ", " - "),
                  label = dplyr::case_when(
                    is.na(intercept) ~ "", # this means no non-NA data in this group at all
                    !is.na(slope) ~
                      paste0("y = ",
                             format(unname(intercept), digits = 2),
                             slope_direction,
                             format(unname(abs(slope)), digits = 2),
                             "\u00B7x, R\u00B2 = ",
                             format(unname(rsq), digits = 3)),
                    is.na(slope) ~ # if there's no slope, display only the intercept
                      paste0("y = ", format(unname(intercept), digits = 2), " [mean]")
                  )
    )

  return(df_stats)
}

#-------------------------------------------------------------------------------
#' @name add_linear_regression_formula
#' @title Add Linear Regression Formula (not currently used)
#' @description
#' A function that adds linear regression formula and place it as a text annotation
#' on the top of a plot. Requires ggplot object as input.
#' @param p ggplot object
#'
#' @returns A ggplot object with the formula placed on the top
#' @importFrom ggplot2 ggplot_build annotate
#' @importFrom stats median
#' @export
#-------------------------------------------------------------------------------
add_linear_regression_formula <- function(p) {
  data <- ggplot2::ggplot_build(p)$data[[1]]
  med_x <- median(data$x, na.rm = TRUE)
  max_y <- max(data$y, na.rm = TRUE)

  p + ggplot2::annotate("text",
                        x = med_x,
                        y = max_y,
                        label = lm_eqn_old(data, "x", "y"),
                        parse = FALSE, # TRUE for ggplots, FALSE for plotly
                        hjust = 0,
                        vjust = 1)
}

#-------------------------------------------------------------------------------
#' @name generate_log_breaks
#' @title Generating log breaks and axis labels, mainly for plotly
#'
#' @param base_values the base value(s) to apply the exponent to
#' @param start the starting exponent
#' @param end the last exponent
#'
#' @returns a numeric
#' @export
#-------------------------------------------------------------------------------

generate_log_breaks <- function(base_values, start, end) {
  powers      <- seq(from = start, to = end)
  breaks      <- rep(0, length(base_values) * length(powers))

  for (i in seq_along(powers)) {
    breaks[((i - 1) * length(base_values) + 1):(i * length(base_values))] <- base_values * 10^powers[i]
  }

  return(breaks)
}

#' @keywords internal
logbreaks_y <- generate_log_breaks(c(1,3), -10, 10) %>% signif(digits = 2)
#' @keywords internal
logbreaks_x <- logbreaks_y
#' @keywords internal
logbreaks_y_log10 <- generate_log_breaks(c(1), -10, 10) %>% signif(digits = 2)
#' @keywords internal
logbreaks_x_log10 <- logbreaks_y_log10
#' @keywords internal
logbreaks_y_minor <- generate_log_breaks(c(1:9), -10, 10) %>% signif(digits = 2)
#' @keywords internal
logbreaks_x_minor <- logbreaks_y_minor

#' @keywords internal
log10_axis_label <- rep("", length(logbreaks_y_minor))
#' @keywords internal
log10_axis_label[seq(1, length(logbreaks_y_minor), 9)] <- as.character(logbreaks_y_minor)[seq(1, length(logbreaks_y_minor), 9)] # every 9th tick is labelled


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
#' @importFrom tibble glimpse
#' @export
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
      if (debug) print(knitr::kable(nmd_q))
      nmd    <- categorize_xvar(df = nmd, quantiles_df = nmd_q, xvar = x_axis_orig)
      if (debug) print(knitr::kable(dplyr::count(nmd, Quantile)))
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
    }
    
    if (!can_quantize) nmd <- dplyr::distinct(nmd, ID, .keep_all = TRUE)
    
    # Re-extract after potential type changes
    y_col <- nmd[[y_axis]]
    
    a <- ggplot2::ggplot(data = nmd, make_base_aes(include_group = FALSE))
    if (!is.null(named_color_vector))
      a <- a + ggplot2::scale_color_manual(values = named_color_vector)
    
    # ── Compute max_y once ────────────────────────────────────────
    max_y <- max(y_col, na.rm = TRUE)
    
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
#' @importFrom tibble glimpse
#' @importFrom forcats fct_inorder
#' @importFrom purrr map_chr
#' @export
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
    
    #max_y    <- max(nmd[[y_axis]], na.rm = TRUE)
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
                           hjust = 1, vjust = 1, show.legend = FALSE,
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
#' @export
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
#' @importFrom GGally ggpairs wrap
#' @importFrom ggplot2 theme_bw
#' @export
#-------------------------------------------------------------------------------

draw_correlation_plot <- function(input_df,
                                  corr_variables,
                                  color_sep = "",
                                  catcov_threshold = 10,
                                  debug = FALSE) {
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
#' @export
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
#' @export
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
#' @export
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
#' @param append_id_text        A string prefix to be inserted for each ID
#' @param ext_db                Default NULL, supply R object of external database
#' @param parallel_sim          Default TRUE, uses the future and mrgsim.parallel packages !Not implemented live!
#' @param parallel_n            The number of subjects required before parallelization is used !Not implemented live!
#'
#' @returns a df
#'
#' @importFrom dplyr mutate select rename
#' @importFrom data.table merge.data.table fwrite fread
#' @export
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
                           append_id_text  = "m1-",
                           ext_db          = NULL,
                           parallel_sim    = FALSE,
                           parallel_n      = 200) {
  
  if (is.null(input_model_object)) {
    if (debug) message("input_model_object is NULL")
    return(NULL)
  }
  
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
    
    input_model_object <- mrgsolve::update(input_model_object, digits = 5)
    
    set.seed(seed)
    
    solved_output <- safely_qsim(
      input_model_object,
      data    = ext_db_ev,
      obsonly = TRUE,
      tgrid   = sampling_times,
      tad     = TRUE,
      output  = "df"
    )
    
    if (is.null(solved_output$error))
      solved_output$result <- data.table::merge.data.table(
        solved_output$result, ext_db, by = "ID", all.x = TRUE
      )
    
  } else {
    # Covers nsubj <= 1 AND the nsubj > 1 / ext_db NULL edge case ──
    if (nsubj > 1 && is.null(ext_db)) {
      if (debug) message("nsubj > 1 but ext_db is NULL — falling back to single-subject sim")
    }
    
    ev_df <- transform_ev_df(
      input_model_object, ev_df,
      model_dur, model_rate, pred_model, debug,
      wt_based_dosing = wt_based_dosing,
      wt_name         = wt_name
    )
    
    solved_output <- input_model_object %>%
      mrgsolve::obsonly() %>%
      mrgsolve::zero_re() %>%
      safely_mrgsim_df(events = ev_df,
                       tgrid  = sampling_times,
                       tad    = TRUE)
  }
  
  # ── Shared post-processing ────────────────────────────────────────────────
  if (is.null(solved_output$error)) {
    solved_output <- solved_output$result %>%
      dplyr::rename(TIME    = time) %>%
      dplyr::mutate(TIMEADJ = TIME / divide_by,
                    ID      = as.factor(paste0(append_id_text, ID)))
  } else {
    shiny::showNotification(
      paste0(solved_output$error, " Potentially due to non-sensible parameter values."),
      type = "error", duration = 10)
    solved_output <- NULL
  }
  
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
#' @export
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

  df <- switch(df_name,
               "CDC" = cdc.expand,
               "WHO" = who.expand,
               "NHANES" = nhanes.filtered
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
#' @importFrom purrr modify_if
#' @importFrom data.table as.data.table setDT setcolorder
#' @export
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
    data <- data.table::as.data.table(
      lapply(orig_data, function(x) suppressWarnings(as.numeric(as.character(x))))
    )
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
#' @export
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
#' @export
#=============================================================================

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
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
#' @export
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
#' @export
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

  return(combined_plot) # requires gridExtra package, deprecated

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
#' @export
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
#' @name extract_matrix
#'
#' @title Function to output a matrix from mrgsolve model object
#'
#' @param input_model_object  mrgsolve model object to extract from
#' @param name_of_matrix      either "omega" or "sigma", will use "omat" or "smat" accordingly
#' @param remove_upper_tri    When TRUE, turns upper triangle of matrix to NA_real_
#' @param remove_zero         In case any of them are 0, don't replace the diagonals
#' @param coerce_to_character Default TRUE, turns all output into a character as a workaround to prevent rounding
#' @param debug               When TRUE, show debugging messages
#'
#' @returns a matrix
#' @export
#-------------------------------------------------------------------------------

extract_matrix <- function(input_model_object,
                           name_of_matrix      = "omega",
                           remove_upper_tri    = TRUE,
                           remove_zero         = TRUE,
                           coerce_to_character = TRUE,
                           debug               = FALSE) {

  if(debug) {
    message("running extract_matrix() for ", name_of_matrix)
  }

  if(name_of_matrix == "omega") {
    om <- mrgsolve::omat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(om))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_omega(input_model_object, name = "omega") %>%
      mrgsolve::omat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }

  if(name_of_matrix == "sigma") {
    sm <- mrgsolve::smat(input_model_object)
    names_list <- as.list(unlist(mrgsolve::labels(sm))) # must use mrgsolve::labels
    omsm_matrix <- mrgsolve::collapse_sigma(input_model_object, name = "sigma") %>%
      mrgsolve::smat() %>%
      mrgsolve::as.matrix() # must use mrgsolve::as.matrix, S4
  }

  if(debug) {
    print(omsm_matrix)
  }

  if(remove_upper_tri) {
    omsm_matrix[upper.tri(omsm_matrix, diag = FALSE)] <- NA_real_
  }
  if(remove_zero) {
    diag_elements <- diag(omsm_matrix) # In case any of them are 0, don't replace the diagonals
    omsm_matrix[omsm_matrix == 0] <- NA_real_
    diag(omsm_matrix) <- diag_elements
  }

  if(coerce_to_character) {
    omsm_matrix <- apply(omsm_matrix, c(1, 2), as.character) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }

  colnames(omsm_matrix) <- names_list

  if(debug) {
    message("finish extract_matrix()")
  }

  return(omsm_matrix)
}

#-------------------------------------------------------------------------------
#' @name reconstruct_matrices
#'
#' @title
#' Converts a matrix object into a list
#' @description
#' Each element in the list corresponds to each omega block, corresponding to provided model object
#'
#' @param input_model_object original model object that contains matrix information
#' @param input_matrix       single collapsed matrix (assumes NA's for upper triangle and non-blocks)
#' @param name_of_matrix     "omega" or "sigma"
#' @param coerce_to_numeric  Default TRUE, will turn all input to numeric
#' @param debug              When TRUE, show debugging messages
#'
#' @note
#' The original model object is needed as the configurations of matrices
#'        are dependent on how $OMEGA and/or $SIGMA is defined in the model code.
#'        It seems like for each repetition of $OMEGA and/or $SIGMA, a new matrix
#'        is created.
#'
#' @returns a list with each element containing a block matrix
#' @export
#-------------------------------------------------------------------------------

reconstruct_matrices <- function(input_model_object,
                                 input_matrix,
                                 name_of_matrix    = "omega",
                                 coerce_to_numeric = TRUE,
                                 debug             = FALSE) {

  if(nrow(input_matrix) == 0) {
    if(debug) {
      message('no input matrix')
    }
    return(input_matrix)
  }

  if(debug) {
    message('Reconstructing Matrix')
  }

  ## Store how many matrices there are from the model object
  if(name_of_matrix == "omega") {
    number_of_matrices <- length(input_model_object$omega)
  }

  if(name_of_matrix == "sigma") {
    number_of_matrices <- length(input_model_object$sigma)
  }

  if(coerce_to_numeric) {
    input_matrix <- apply(input_matrix, c(1, 2), as.numeric) %>%
      matrix(., nrow = nrow(.), ncol= ncol(.))
  }

  result        <- list()
  start_row_col <- 1

  for(n in 1:number_of_matrices) {

    if(name_of_matrix == "omega") {
      nrow_current_matrix <- nrow(input_model_object$omega[[n]])
    }
    if(name_of_matrix == "sigma") {
      nrow_current_matrix <- nrow(input_model_object$sigma[[n]])
    }

    result[[n]] <- list()

    end_row_col <- start_row_col + nrow_current_matrix - 1
    if(debug) {
      message("Current matrix: ", n, ", start row/col: ", start_row_col)
    }
    result[[n]] <- input_matrix[start_row_col:end_row_col, start_row_col:end_row_col]

    # Mirror lower triangle to upper triangle
    lower_triangle <- lower.tri(result[[n]])
    result[[n]][!lower_triangle] <- t(result[[n]])[!lower_triangle]
    result[[n]][is.na(result[[n]])] <- 0 # Required for omat() as NA's don't work
    result[[n]] <- result[[n]] %>% as.matrix() # Required for 1x1 matrix otherwise it becomes a numeric

    start_row_col <- start_row_col + nrow_current_matrix # advancing to new starting row/col for next matrix

  } # end of number of matrix
  if(debug) {
    message('Matrix reconstruction complete')
  }
  return(result)
}

#-------------------------------------------------------------------------------
#' @name update_variability
#'
#' @title Updates variability for a mrgmod object
#'
#' @param input_model_object   original model object to be updated
#' @param input_matrix         list of matrices containing new parameter values
#' @param name_of_matrix       "omega" or "sigma"
#' @param check_validity       Default TRUE, where it checks that the matrix is sensible
#'                       If check fails, the model object is NOT updated
#' @param debug                When TRUE, show debugging messages
#'
#' @returns a mrgmod object
#' @export
#-------------------------------------------------------------------------------

update_variability <- function(input_model_object,
                               input_matrix,
                               name_of_matrix = "omega",
                               check_validity = TRUE,
                               debug          = FALSE) {

  if(check_validity) {
    matrix_is_valid <- check_matrix(input_model_object = input_model_object, input_matrix = input_matrix, debug = debug)
    if(!matrix_is_valid) {
      if(debug) {
        message("Matrix not valid. Returning original input model object")
      }
      return(input_model_object)
    } # model object is not updated if matrix is invalid
  }

  if(name_of_matrix == "omega") {
    tmp <- input_model_object %>% mrgsolve::omat(input_matrix)
  }

  if(name_of_matrix == "sigma") {
    tmp <- input_model_object %>% mrgsolve::smat(input_matrix)
  }

  return(tmp)
}

#-------------------------------------------------------------------------------
#' @name check_matrix
#'
#' @title Checks for validity of matrix
#'
#' @param input_model_object mrgmod object
#' @param input_matrix       a list where each element is a matrix
#' @param check_diagonal     Default TRUE, check that each diagonal must be >= 0
#' @param check_symmetry     Default FALSE, check for symmetry
#' @param debug              Show debugging messages
#' @param display_error      Displays a showNotification message pop-up
#'
#' @returns logical (TRUE/FALSE), where TRUE means pass (valid matrix)
#' @importFrom stringr str_detect
#' @export
#-------------------------------------------------------------------------------

check_matrix <- function(input_model_object,
                         input_matrix,
                         check_diagonal = TRUE,
                         check_symmetry = FALSE,
                         debug = FALSE,
                         display_error = TRUE) {
  #print(input_matrix)
  for(i in 1:length(input_matrix)) {

    if(check_diagonal) {
      if(any(diag(input_matrix[[i]]) < 0)) {
        if(display_error) {
          shiny::showNotification("ERROR: Each diagonal (variance) must be >= 0", type = "error", duration = 10)
        }
        return(FALSE)
      }
    } # End of check diagonal

    if(any(stringr::str_detect(input_model_object$code, "@correlation"))) {
      check_eigenvalues <- FALSE

    } else {
      check_eigenvalues <- TRUE
    }

    if(check_eigenvalues) { # check that the matrix must be positive semi-definite assuming '@correlation' is not used in ANY code
      if(debug) {
        message("No @correlation found, proceeding to checking eigenvalues")
      }
      if(any(eigen(input_matrix[[i]])$values < 0)) {
        if(display_error) {
          shiny::showNotification("ERROR: The matrix is not positive semi-definite", type = "error", duration = 10)
        }
        return(FALSE)
      }
    } # End of check eigenvalues

    if(check_symmetry) {
      if (!all(identical(input_matrix[[i]], t(input_matrix[[i]])))) {
        return(FALSE)
      }
    } # End of check symmetry
  } # End of each matrix loop

  if(debug) {
    message("Check matrix PASS")
  }
  return(TRUE)
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
#' @export
#-------------------------------------------------------------------------------

quantile_output <- function(iiv_sim_input,
                            yvar = 'DV',
                            lower_quartile = 0.025,
                            upper_quartile = 0.975,
                            dp   = 5
) {
  iiv_sim_input <- iiv_sim_input %>%
    dplyr::group_by(TIME) %>%
    dplyr::mutate(median_yvar   = quantile(.data[[yvar]], probs = 0.5) %>% round(digits = dp),
                  mean_yvar     = mean(.data[[yvar]]) %>% round(digits = dp),
                  lower_yvar    = quantile(.data[[yvar]], probs = lower_quartile) %>% round(digits = dp),
                  upper_yvar    = quantile(.data[[yvar]], probs = upper_quartile) %>% round(digits = dp)) %>%
    dplyr::ungroup()
}

#-------------------------------------------------------------------------------
#' @name safely_eval
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `eval`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `eval`
#' @export
#-------------------------------------------------------------------------------

safely_eval  <- purrr::safely(eval)

#-------------------------------------------------------------------------------
#' @name safely_parse
#'
#' @title purrr:safely wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `parse`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `parse`
#' @export
#-------------------------------------------------------------------------------

safely_parse <- purrr::safely(parse)

#-------------------------------------------------------------------------------
#' @name eval_parse
#'
#' @title Short-hand function to eval, and then parse a string
#'
#' @param text Some text
#'
#' @returns A list with two elements:
#' * `result`: The result of `eval(parse(text))`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#' @export
#-------------------------------------------------------------------------------

eval_parse <- function(text) {
  eval(parse(text = text))
}

#-------------------------------------------------------------------------------
#' @name pknca_table
#'
#' @title Function to calculate summary statistics from simulated output
#'
#' @param input_simulated_table  a dataframe (usually created from mrgsim)
#' @param output_conc            y variable of interest to perform summary stats calc
#' @param start_time             start time interval for metrics
#' @param end_time               end time interval for metrics
#' @param debug                  show debugging messages
#'
#' @returns a dataframe with additional columns of summary stats
#' @importFrom dplyr mutate if_else ungroup select filter rename first
#' @export
#-------------------------------------------------------------------------------

pknca_table <- function(input_simulated_table,
                        output_conc,
                        start_time = NULL,
                        end_time   = NULL,
                        debug      = FALSE
) {

  if(is.null(input_simulated_table)) {
    return(NULL)
  }

  reactive_cmin <- paste0(output_conc, '_CMIN_ranged')
  reactive_cmax <- paste0(output_conc, '_CMAX_ranged')
  reactive_cavg <- paste0(output_conc, '_CAVG_ranged')
  reactive_tmax <- paste0(output_conc, '_TMAX_ranged')
  reactive_tmin <- paste0(output_conc, '_TMIN_ranged')
  reactive_cfbpct <- paste0(output_conc, '_CFBPCT_ranged')
  reactive_mcfbpct <- paste0(output_conc, '_MEANCFBPCT_ranged')
  reactive_nadirpct <- paste0(output_conc, '_NADIRPCT_ranged')

  zero_tlast_cmin <- paste0(output_conc, '_CMIN_tlast')
  zero_tlast_cmax <- paste0(output_conc, '_CMAX_tlast')
  zero_tlast_cavg <- paste0(output_conc, '_CAVG_tlast')
  zero_tlast_tmax <- paste0(output_conc, '_TMAX_tlast')
  zero_tlast_tmin <- paste0(output_conc, '_TMIN_tlast')
  zero_tlast_cfbpct <- paste0(output_conc, '_CFBPCT_tlast')
  zero_tlast_mcfbpct <- paste0(output_conc, '_MEANCFBPCT_tlast')
  zero_tlast_nadirpct <- paste0(output_conc, '_NADIRPCT_tlast')

  input_simulated_table$YVARNAME <- input_simulated_table[[output_conc]]

  if (debug) {
    # message(head(input_simulated_table))
    # message(paste0(max(input_simulated_table$YVARNAME)[1]))
  }

  metrics_table <- input_simulated_table %>%
    dplyr::mutate(CMIN = min(YVARNAME, na.rm = TRUE)[1],
                  CMAX = max(YVARNAME, na.rm = TRUE)[1],  ### First element if multiple values found
                  CAVG = mean(YVARNAME, na.rm = TRUE),
                  DVBL = dplyr::first(YVARNAME),
                  CFBPCT = ((YVARNAME - DVBL) / DVBL) * 100,
                  MEANCFBPCT = mean(CFBPCT, na.rm = TRUE), # only accurate if sampling points are equidistant
                  NADIRPCT = min(CFBPCT, na.rm = TRUE)[1]
    )

  DVBL_overall <- metrics_table$DVBL[1] # Note: Baseline is always first ever TIME overall

  metrics_table <- metrics_table %>%
    dplyr::mutate(TMAX = .$TIME[.$CMAX[1] == YVARNAME][1],
                  TMIN = .$TIME[.$CMIN[1] == YVARNAME][1])

  metrics_table  <- metrics_table %>%
    dplyr::mutate(YLAG       = dplyr::lag(YVARNAME ),
                  XLAG       = dplyr::lag(TIME),
                  dYVAR      = (YVARNAME  + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR      = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC_tlast    = sum(dYVAR)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ID, -tad, -TIMEADJ, -YLAG, -XLAG, -dYVAR, -DVBL)

  if (debug) {
    # message(start_time)
    # message(end_time)
    # tmp <- input_simulated_table %>% dplyr::filter(TIME >= start_time)
    # message(dplyr::glimpse(tmp))
    # tmp2 <- input_simulated_table %>% dplyr::filter(TIME <= start_time)
    # message(dplyr::glimpse(tmp2))
  }

  ### Repeat metrics for time range # note that the time range is inclusive on both ends
  metrics_table_time <- input_simulated_table %>% dplyr::filter(TIME >= start_time, TIME <= end_time) %>%
    dplyr::mutate(CMIN = min(YVARNAME, na.rm = TRUE)[1],
                  CMAX = max(YVARNAME, na.rm = TRUE)[1], ### First element if multiple values found
                  CAVG = mean(YVARNAME, na.rm = TRUE),
                  CFBPCT = ((YVARNAME - DVBL_overall) / DVBL_overall) * 100, # Note: Baseline is always first ever TIME overall
                  MEANCFBPCT = mean(CFBPCT, na.rm = TRUE), # only accurate if sampling points are equidistant
                  NADIRPCT = min(CFBPCT, na.rm = TRUE)[1]
    )

  metrics_table_time <- metrics_table_time %>%
    dplyr::mutate(TMAX = .$TIME[.$CMAX[1] == YVARNAME][1], ### First element if multiple values found
                  TMIN = .$TIME[.$CMIN[1] == YVARNAME][1])

  metrics_table_time  <- metrics_table_time %>%
    dplyr::mutate(YLAG          = dplyr::lag(YVARNAME ),
                  XLAG          = dplyr::lag(TIME),
                  dYVAR         = (YVARNAME + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR         = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC_ranged    = sum(dYVAR)) %>%
    dplyr::ungroup()

  CMIN_time <-  metrics_table_time[["CMIN"]][1]
  CMAX_time <-  metrics_table_time[["CMAX"]][1]
  CAVG_time <-  metrics_table_time[["CAVG"]][1]
  TMAX_time <-  metrics_table_time[["TMAX"]][1]
  TMIN_time <-  metrics_table_time[["TMIN"]][1]
  AUC_time  <-  metrics_table_time$AUC_ranged[1]
  CFBPCT_time <- dplyr::last(metrics_table_time[["CFBPCT"]])
  MCFBPCT_time <- metrics_table_time[["MEANCFBPCT"]][1]
  NADIRPCT_time <- metrics_table_time[["NADIRPCT"]][1]

  metrics_table <- metrics_table %>%
    dplyr::mutate(!!reactive_cmin := CMIN_time,
                  !!reactive_cmax := CMAX_time,
                  !!reactive_cavg := CAVG_time,
                  !!reactive_tmax := TMAX_time,
                  !!reactive_tmin := TMIN_time,
                  AUC_ranged := AUC_time,
                  !!reactive_cfbpct := CFBPCT_time,
                  !!reactive_mcfbpct := MCFBPCT_time,
                  !!reactive_nadirpct := NADIRPCT_time) %>%
    dplyr::rename(!!zero_tlast_cmin := CMIN,
                  !!zero_tlast_cmax := CMAX,
                  !!zero_tlast_cavg := CAVG,
                  !!zero_tlast_tmax := TMAX,
                  !!zero_tlast_tmin := TMIN,
                  !!zero_tlast_cfbpct := CFBPCT,
                  !!zero_tlast_mcfbpct := MEANCFBPCT,
                  !!zero_tlast_nadirpct := NADIRPCT) %>%
    dplyr::select(-YVARNAME)

  return(metrics_table)
}

#-------------------------------------------------------------------------------
#' Exported from shinyBS:::buildTooltipOrPopoverOptionsList
#' @keywords internal
#-------------------------------------------------------------------------------
MVPbuildTooltipOrPopoverOptionsList <- function (title, placement, trigger, options, content)
{
  if (is.null(options)) {
    options = list()
  }
  if (!missing(content)) {
    if (is.null(options$content)) {
      options$content = shiny::HTML(content)
    }
  }
  if (is.null(options$placement)) {
    options$placement = placement
  }
  if (is.null(options$trigger)) {
    if (length(trigger) > 1)
      trigger = paste(trigger, collapse = " ")
    options$trigger = trigger
  }
  if (is.null(options$title)) {
    options$title = title
  }
  return(options)
}

#' Internal HTML Dependency for shinyBS
#'
#' This object represents the internal HTML dependency for the `shinyBS` package.
#' It is used to include the required JavaScript and CSS files for `shinyBS` functionality
#' without relying on unexported objects from the `shinyBS` package.
#'
#' @details
#' The `shinyBSDep` object is a list that defines the HTML dependency for `shinyBS`.
#' It includes metadata such as the name, version, source files, and other attributes.
#' The object is stored internally to avoid direct use of `shinyBS:::shinyBSDep`,
#' which is discouraged by CRAN guidelines.
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{name}{Character string specifying the name of the dependency (`"shinyBS"`).}
#'   \item{version}{Character string specifying the version of the dependency (`"0.61.1"`).}
#'   \item{src}{A list specifying the source location of the dependency. Contains:
#'     \itemize{
#'       \item{\code{href}: Character string specifying the relative path (`"sbs"`).}
#'     }
#'   }
#'   \item{meta}{\code{NULL}, indicating no metadata is provided.}
#'   \item{script}{Character string specifying the JavaScript file (`"shinyBS.js"`).}
#'   \item{stylesheet}{Character string specifying the CSS file (`"shinyBS.css"`).}
#'   \item{head}{\code{NULL}, indicating no additional HTML is provided for the `<head>` section.}
#'   \item{attachment}{\code{NULL}, indicating no attachments are provided.}
#'   \item{package}{\code{NULL}, indicating no specific package is associated.}
#'   \item{all_files}{Logical value (\code{TRUE}) indicating that all files are included.}
#' }
#'
#' @note
#' The `shinyBSDep` object is marked as internal and is not exported. It is intended
#' for use within the package to avoid reliance on unexported objects from `shinyBS`.
#'
#' @keywords internal
MVPshinyBSDep <- list(
  name = "shinyBS",
  version = "0.61.1",
  src = list(
    href = "sbs"
  ),
  meta = NULL,
  script = "shinyBS.js",
  stylesheet = "shinyBS.css",
  head = NULL,
  attachment = NULL,
  package = NULL,
  all_files = TRUE
)

# Set the class attribute
attr(MVPshinyBSDep, "class") <- "html_dependency"

#-------------------------------------------------------------------------------
#' @name update_resistant_popover
#'
#' @title Function that adds on to the an existing bsPopover
#'        such that the tooltip persists after updateSelectInput etc
#' @inheritParams shinyBS::bsPopover
#' @param options other options
#' @returns A popover
#' @export
#-------------------------------------------------------------------------------

update_resistant_popover <- function(id, title, content, placement = "bottom", trigger = "hover", options = NULL){
  options = MVPbuildTooltipOrPopoverOptionsList(title, placement, trigger, options, content)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      var target = document.querySelector('#", id, "');
      var observer = new MutationObserver(function(mutations) {
        setTimeout(function() {
          shinyBS.addTooltip('", id, "', 'popover', ", options, ");
        }, 200);
      });
      observer.observe(target, { childList: true });
    });
  ")))
  htmltools::attachDependencies(bsTag, MVPshinyBSDep)
}

#-------------------------------------------------------------------------------
#' @name smart_x_axis
#'
#' @title Function to improve default x-axis breaks
#'
#' @param p1               ggplot object
#' @param max_x            Max x-axis value, will be derived from p1 if it is set to NULL
#' @param xvar             x variable string to be used to derive max_x
#' @param xlabel           Will only apply when xlabel is either "Time (hours)",
#'                         "Time (weeks)", "Time (days)", or "Time (months)
#' @param debug            Shows debug messages
#'
#' @returns a ggplot object
#' @importFrom scales pretty_breaks
#' @export
#-------------------------------------------------------------------------------

smart_x_axis <- function(p1,
                         max_x  = NULL,
                         xlabel,
                         debug  = FALSE) {
  
  if (is.null(max_x)) {
    # ggplot_build() is expensive — only called when max_x not supplied
    # x.range upper bound is expanded by ~5% by default, so we reverse that
    max_x <- ggplot2::ggplot_build(p1)$layout$panel_params[[1]]$x.range[[2]] / 1.05
  }
  
  if (debug) message("Max X: ", max_x)
  
  # Guard: skip axis modification if max_x is unusable
  if (is.na(max_x) || !is.finite(max_x)) return(p1)
  
  # switch short-circuits on first match; if/else if avoids case_when 
  # vector overhead on a scalar value
  x_tick_size <- switch(xlabel,
                        "Time (hours)" = {
                          if      (max_x <=    4) 0.5
                          else if (max_x <=   12) 1
                          else if (max_x <=   24) 2
                          else if (max_x <=   48) 4
                          else if (max_x <=   96) 12
                          else if (max_x <=  168) 24
                          else if (max_x <=  480) 48
                          else if (max_x <= 2016) 168
                          else                    672
                        },
                        "Time (days)" = {
                          if      (max_x <=  7) 1
                          else if (max_x <= 14) 2
                          else if (max_x <= 56) 7
                          else if (max_x <= 84) 14
                          else                  28
                        },
                        "Time (weeks)" = {
                          if      (max_x <=  2) 0.2
                          else if (max_x <=  4) 0.5
                          else if (max_x <= 16) 1
                          else if (max_x <= 32) 2
                          else if (max_x <= 52) 4
                          else                  12
                        },
                        "Time (months)" = {
                          if      (max_x <=  1) 0.25
                          else if (max_x <=  4) 0.5
                          else if (max_x <= 12) 1
                          else if (max_x <= 24) 2
                          else if (max_x <= 48) 4
                          else                  12
                        },
                        NULL  # unrecognised xlabel — fall through to pretty_breaks below
  )
  
  if (debug) message("x_tick_size: ", x_tick_size)
  
  p1 + if (is.null(x_tick_size) || ceiling(max_x / x_tick_size) >= 25) {
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  } else {
    ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
  }
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
#' @export
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
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 theme
#' @export
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
  combined_input[[param_name]] <- forcats::fct_inorder(
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
#' @export
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
                                 color = ID),
          alpha = 0.4)
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
#' @import mrgsolve
#' @export
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
#' @export
#-------------------------------------------------------------------------------

update_model_object <- function(input_model_object, input_new_df, convert_colnames = FALSE) {
  tmp1 <- input_new_df

  if(convert_colnames) names(tmp1) <- c("name", "value")

  param_list <- as.list(setNames(tmp1[["value"]], tmp1[["name"]]))

  mrgsolve::param(input_model_object, param_list)
  #return(new_mod)
}

#-------------------------------------------------------------------------------
#' @name convert_to_plotly_watermark
#'
#' @title Converts a ggplot object to plotly object and then apply watermark, and other options
#'
#' @param ggplot_object  a ggplot object
#' @param opacity        the alpha or transparency, goes from 0 to 1
#' @param font_name      font name as a string
#' @param format         one of "png", "svg", "jpeg", "webp"
#' @param filename       name of output file when saved
#' @param width          width in pixels. Default NULL - uses current resolution
#' @param height         height in pixels. Default NULL - uses current resolution
#' @param debug          Set to TRUE to show debug messages
#' @param try_tooltip    Testing tooltip display option, relevant for tornado plots
#' @param plotly_watermark Set to TRUE to insert "For Internal Use Only"
#'
#' @returns a plotly object
#' @importFrom plotly ggplotly add_annotations config renderPlotly plotlyOutput
#' @export
#-------------------------------------------------------------------------------

convert_to_plotly_watermark <- function(ggplot_object,
                                        # logx        = FALSE,
                                        # logy        = FALSE,
                                        # logticks    = c(1,2,3,4,5,6,7,8,9,10),
                                        # loglabels   = c(1,10),
                                        opacity     = 0.05,
                                        font_name   = "Arial",
                                        format      = "png",
                                        filename    = "newplot",
                                        width       = NULL,
                                        height      = NULL,
                                        debug       = FALSE,
                                        try_tooltip = FALSE,
                                        plotly_watermark = TRUE) {
  
  if (debug) {
    message("Converting ggplot object to plotly")
  }
  
  if(try_tooltip) {
    tmp <- plotly::ggplotly(ggplot_object, tooltip = "text")
  } else {
    tmp <- plotly::ggplotly(ggplot_object)
  }
  
  tmp <- tmp %>%
    plotly::add_annotations(
      text = ifelse(plotly_watermark, "For Internal Use Only", ""),
      xref = "paper",
      yref = "paper",
      x = 0.5,
      y = 0.5,
      showarrow = FALSE,
      font = list(family = font_name, size = 58, color = paste0("rgba(0, 0, 0, ", opacity, ")"))
    ) %>%
    plotly::config(toImageButtonOptions = list(format   = format, # one of png, svg, jpeg, webp
                                               filename = filename,
                                               height   = height,
                                               width    = width,
                                               scale    = 1 ),
                   modeBarButtonsToAdd = c('drawopenpath',
                                           'drawline',
                                           'drawcircle',
                                           'drawrect',
                                           'eraseshape'),
                   #modeBarButtonsToRemove = c('lasso2d')) ## For some unknown reason removal of lasso2d breaks the plot
                   displayModeBar = TRUE,
                   displaylogo = FALSE) #,
  #scrollZoom = TRUE,
  
  if (debug) {
    message("Converting ggplot object to plotly successful.")
  }
  
  return(tmp)
}

#=============================================================================
#' @name quantize
#'
#' @title Used for dividing bins for median line plots
#' @param x vector of values (e.g. x-axis)
#' @param levels bins
#' @param ... extra params to pass through to cut()
#' @export
#=============================================================================

quantize <- function (x, levels, ...) {
  stopifnot(!anyNA(levels), is.numeric(levels), is.numeric(x))
  midpoints <- (head(levels, -1) + tail(levels, -1))/2
  breaks <- c(-Inf, midpoints, Inf)
  idx <- cut(x, breaks, labels = FALSE, ...)
  levels[idx]
}

#=============================================================================
#' @name sanitize_numeric_input
#'
#' @title expects numeric as input, and cleans it
#'
#' @description
#' 1. If value is empty (NA) and allow_zero = TRUE, will return 0
#' 2. If value is empty (NA) and allow_zero = FALSE, will return 1
#' 3. If value is <= 0 and allow_zero = TRUE, will return 0
#' 4. If value is <= 0 and allow_zero = FALSE, will return 1
#' 5. If a legal maximum or minimum is provided and the value falls outside of
#'    those ranges, the legal value will be used instead.
#' 6. If a return_value has been specified, any time conditions 1-5 is triggered, the
#'    return_value will be used instead. (therefore the return_value MUST make sense)
#'
#' In all other cases the value is returned as-is.
#'
#' @param numeric_input  a numeric e.g. from numericInput
#' @param allow_zero     Default TRUE, turns the numeric to 0 if input is NA or <= 0, otherwise 1
#' @param as_integer     Default FALSE, coerce input into integer
#' @param legal_minimum  Lower bound numeric that is accepted
#' @param legal_maximum  Upper bound numeric that is accepted
#' @param return_value   If the input is bad, return value X. Default NULL i.e.
#'                       returns 0 (if allow_zero = TRUE) or 1 (if allow_zero = FALSE)
#' @param display_error  Displays showNotification message box
#' @returns a numeric
#' @export
#=============================================================================

sanitize_numeric_input <- function(numeric_input,
                                   allow_zero = TRUE,
                                   as_integer = FALSE,
                                   return_value = NULL,
                                   legal_maximum = NULL,
                                   legal_minimum = NULL,
                                   display_error = FALSE) {

  sanitized_input <- as.numeric(numeric_input)

  if(as_integer) {
    sanitized_input <- as.integer(sanitized_input)
  }

  # Handling NA's
  if(is.na(sanitized_input)) {
    if(allow_zero) {
      sanitized_input <- 0
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }

  # Handling zeroes
  if(sanitized_input == 0) {
    if(allow_zero) {
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }

  # Handling negative numbers
  if(sanitized_input < 0) {
    if(allow_zero) {
      sanitized_input <- 0
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    } else {
      sanitized_input <- 1
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      if(display_error) {shiny::showNotification(paste0("WARNING: Bad numeric input. Trying ", sanitized_input, " instead."), type = "warning", duration = 5)}
      return(sanitized_input)
    }
  }

  if(!is.null(legal_maximum)) {
    if(sanitized_input > legal_maximum) {
      if(display_error) {
        shiny::showNotification(paste0("WARNING: Bad numeric input (exceeded legal maximum). Trying ", legal_maximum, " instead."), type = "warning", duration = 5)
      }
      sanitized_input <- legal_maximum
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      return(sanitized_input)
    }
  }

  if(!is.null(legal_minimum)) {
    if(sanitized_input < legal_minimum) {
      if(display_error) {
        shiny::showNotification(paste0("WARNING: Bad numeric input (below legal minimum). Trying ", legal_minimum, " instead."), type = "warning", duration = 5)
      }
      sanitized_input <- legal_minimum
      if(!is.null(return_value)) {
        sanitized_input <- return_value
      }
      return(sanitized_input)
    }
  }

  return(sanitized_input)
}

#-------------------------------------------------------------------------------
#' @name create_value_box
#' @title Short-hand for creating NCA metrics valueBoxes
#'
#' @param input_dataset       dataset to extract metrics from
#' @param name_ends_with      name of metrics column that ends with x (string)
#' @param value_box_subtitle  name of subtitle in value box
#' @param width               width of valueBox
#' @param color               color of valueBox
#' @param sigdig              significant digits to output
#' @param dp                  if TRUE, uses decimal rounding instead
#' @returns a shinydashboard valueBox
#' @importFrom shinydashboard valueBox box
#' @export
#-------------------------------------------------------------------------------

create_value_box <- function(input_dataset,
                             name_ends_with,
                             value_box_subtitle,
                             width  = infoBox_width,
                             color,
                             sigdig = 4,
                             dp     = FALSE) {
  
  # Coerce once rather than in each branch
  sigdig <- as.integer(sigdig)
  
  metric_value <- if (is.null(input_dataset)) {
    NA_real_
  } else {
    input_dataset %>%
      dplyr::select(dplyr::ends_with(name_ends_with)) %>%
      dplyr::pull() %>%   # extracts the single column as a vector
      unique()
  }
  
  # Direct function calls — no pipe overhead on scalar operations
  metric_value <- if (dp) round(metric_value, digits = sigdig) else signif(metric_value, digits = sigdig)
  
  shinydashboard::valueBox(
    value    = tags$p(style = font_size, metric_value),
    subtitle = value_box_subtitle,
    width    = width,
    color    = color,
    href     = NULL
  )
}

#-------------------------------------------------------------------------------
#' @name check_and_combine_df
#'
#' @title combined 2 datasets for download
#' @param model_1_is_valid  logical TRUE/FALSE
#' @param model_2_is_valid  logical TRUE/FALSE
#' @param input_df_1        Model 1 simulated data
#' @param input_df_2        Model 2 simulated data
#'
#' @returns a df
#' @importFrom dplyr intersect full_join
#' @export
#-------------------------------------------------------------------------------

check_and_combine_df <- function(model_1_is_valid,
                                 model_2_is_valid,
                                 input_df_1 = NULL,
                                 input_df_2 = NULL) {

  if(model_1_is_valid & model_2_is_valid) {

    common_columns <- dplyr::intersect(names(input_df_1), names(input_df_2))
    combined_model <- dplyr::full_join(input_df_1, input_df_2, by = common_columns)

    return(combined_model)

  } else if (model_1_is_valid) {
    return(input_df_1)
  } else if (model_2_is_valid) {
    return(input_df_2)
  } else {stop('There are no valid datasets to be downloaded.')}
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
#' @export
#-------------------------------------------------------------------------------

pct_above_y_at_x <- function(model_is_valid = FALSE,
                             input_df,
                             y_name  = "DV",
                             y_value = NA,
                             x_name  = "TIME",
                             x_value = NA,
                             return_number_ids = FALSE) {

  if(!model_is_valid | is.null(x_value) | is.null(y_value)) {
    return(NA)
  }

  if(model_is_valid & !is.na(y_value) & !is.na(x_value) ) {

    input_df_filtered <- input_df %>%
      dplyr::filter(!!dplyr::sym(y_name)  > y_value,
                    !!dplyr::sym(x_name) == x_value)

    number_of_ids_in_df          <- length(unique(input_df$ID))
    number_of_ids_in_df_filtered <- length(unique(input_df_filtered$ID))

    if(return_number_ids) {
      return(number_of_ids_in_df_filtered)
    } else {
      return(round(number_of_ids_in_df_filtered/number_of_ids_in_df * 100,1))
    }

  } else {
    return(NA)
  }
}

#-------------------------------------------------------------------------------
#' @name create_alert
#' @title Function that generates a shinyalert
#'
#' @param ppm_name       Name of Project Pharmacometrician
#' @param ppm_email      Email of Project Pharmacometrician
#'
#' @returns a shinyalert() popup
#' @export
#-------------------------------------------------------------------------------

create_alert <- function(ppm_name = "Firstname Lastname",
                         ppm_email = "dummy.email@company.com") {
  
  if(requireNamespace("shinyalert", quietly = TRUE)) {
    
    email_html <- paste0("<a href='mailto:",
                         ppm_email,
                         "?subject=Model%20Visualization%20Platform%20(MVP)%20Usage'>",
                         ppm_name,
                         "</a>")
    
    warning_text <- paste0("The unlocked model is provided for exploratory purposes only.<br>Please consult with your Project Pharmacometrician (PPM), ",
                           email_html,
                           ", for more information.<br><br>",
                           "<b><font color='red'>Usage of any output produced in this App without the PPM's prior knowledge and approval is strictly prohibited.</font></b>")
    
    password_alert <- shinyalert::shinyalert(
      title = "Disclaimer",
      text = warning_text,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  } else {
    password_alert <- shiny::showNotification(paste0("Model unlocked (contact person: ", ppm_name,")."), type = "message", duration = 10)
  }
  
  return(password_alert)
}

#-------------------------------------------------------------------------------
#' @name useShinydashboardMVP
#'
#' @title Manually define useShinydashboard before it will be removed in a future
#' release of shinyWidgets
#'
#' @note
#' https://github.com/dreamRs/shinyWidgets/blob/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/useShinydashboard.R
#'
#' @returns Attaches shinydashboard
#'
#' @export
#-------------------------------------------------------------------------------
useShinydashboardMVP <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}

#-------------------------------------------------------------------------------
#' @name general_warning_modal
#' @title Creates a general warning modal
#' @param title            Title for the modal box
#' @param text_description UI elements
#'
#' @returns a modal dialog
#' @export
#-------------------------------------------------------------------------------

general_warning_modal <- function(title = "Warning", text_description = "Test") {
  shiny::showModal(shiny::modalDialog(
    title  = title,
    text_description,
    footer = shiny::modalButton("OK"),
    size   = "m",
    easyClose = FALSE,
    fade = FALSE
  ))
}

#-----------------------------------------------------------------------------
#' @name split_data_frame
#' @title Function to split dataframes into roughly equal portions to facilitate
#' distribution to each core
#'
#' @param df data.frame to split
#' @param N number of chunks / cores
#'
#' @returns A list containing N data.frames. If N=1, then the original df
#'         is returned unchanged
#' @export
#-----------------------------------------------------------------------------

split_data_frame <- function(df, N = 4) {
  if(N == 1) {
    return(list(df)) # Note we're coercing df to a list when single core is desired
  } else if (N < 1) { # This may be redundant as n.cores already have a safeguard
    stop("N can't be less than one")
  } else if (N > nrow(df)) { # This may be redundant as n.cores already have a safeguard
    stop("N must be less than nrows(df)")
  }

  # split the data.frame into a list
  res <- split(df, cut(seq_len(nrow(df)), N, labels = FALSE))
  return(res)
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
#' @export
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
#' @name check_cov_name
#'
#' @title Function to return a DUMMY name if supplied string is
#' reserved
#'
#' @param orig_name                 original name
#' @param replaced_name             replacement name
#' @param list_of_reserved_strings  unavailable names
#'
#' @returns a string
#' @export
#-------------------------------------------------------------------------------

check_cov_name <- function(orig_name, replaced_name = "DUMMY", list_of_reserved_strings = c("AGE", "AGEMO", "SEX", "WT", "BMI", "BSA")) {

  if (orig_name %in% list_of_reserved_strings) {
    new_name <- replaced_name
    shiny::showNotification(paste0("WARNING: Covariate name is reserved. Will show up as ", replaced_name, " instead."), type = "warning", duration = 10)
  } else {
    new_name <- orig_name
  }
  return(new_name)
}

#-------------------------------------------------------------------------------
#' @name check_cov_name_duplicate
#' @title Function to check covariate names do not duplicate
#'
#' @param current_id : id of current textInput
#' @param all_ids    : list of ids for textInputs to check
#' @export
#-------------------------------------------------------------------------------

check_cov_name_duplicate <- function(current_id, all_ids) {
  shiny::observeEvent(input[[current_id]], {
    if(current_id != "") { # do not perform check if textInput is empty i.e. ""
      if(any(sapply(all_ids[all_ids != current_id], function(x) input[[current_id]] == input[[x]]))) {
        shiny::showNotification(paste0("ERORR: Covariate names must be unique from each other. Name is reset."), type = "error", duration = 10)
        shiny::updateTextInput(session, current_id, value = "")
      }
    }
  })
}

#------------------------------------------------------------------------------
#' @name add_watermark
#' @title Function to add a watermark to a ggplot
#'
#' @description
#' The function is based on the example found here:
#' https://www.r-bloggers.com/2012/05/adding-watermarks-to-plots/.
#'
#' @param watermark_toggle Set to TRUE to insert a watermark layer for ggplot
#' @param lab Text to be displayed
#' @param col Color of watermark text
#' @param alpha Text transparency
#' @param fontface "plain" | "bold" | "italic" | "bold.italic"
#' @param rot rotation (0,360), NA = from lower left to upper right corner
#' @param width text width relative to plot
#' @param pos x- and y-position relative to plot (vector of length 2)
#' @param align "left" | "right" | "center" | "centre" | "top" | "bottom",
#' can also be given as vector of length 2, first horizontal, then vertical
#' @import grid
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
#' @export
#------------------------------------------------------------------------------
add_watermark <- function(watermark_toggle = TRUE,
                          lab = "For Internal Use Only",  # Text to be displayed
                          col = "grey",                   # text colour
                          alpha = 0.7,                    # text transparency [0,1]
                          fontface = "plain",              # "plain" | "bold" | "italic" | "bold.italic"
                          rot = 0,                       # rotation (0,360); NA = from lower left to upper right corner
                          width = 0.6,                    # text width relative to plot
                          pos = c(0.5, 0.5),              # x- and y-position relative to plot (vector of length 2)
                          align = "centre"                # alignment of text relative to position
                          #   "left" | "right" | "center" | "centre" | "top" | "bottom"
                          #   can also be given as vector of length 2, first horizontal, then vertical
) {

  if(watermark_toggle) {

    watermark_grob <- grid::grob(
      lab = lab, cl = "watermark",
      col = col, alpha = alpha, fontface = fontface,
      rot = rot, width = width, pos = pos, align = align
    )
    ggplot2::annotation_custom(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf,
                               watermark_grob)
  } else {
    ggplot2::geom_blank()
  }
}

# ----- Draw details for watermark -----
#' @name drawDetails.watermark
#' @title Auxillary function for watermark in ggplot (S3method?)
#' @param x  x
#' @param ... Other parameters passed on
#' @import grid
#' @export
drawDetails.watermark <- function(x, ...) {
  plot_width_half  <- grid::convertUnit(
    grid::unit(1, "npc"),
    unitTo = "mm",
    val = TRUE,
    axisFrom = "x"
  ) / 2
  plot_height_half <- grid::convertUnit(
    grid::unit(1, "npc"),
    unitTo = "mm",
    val = TRUE,
    axisFrom = "y"
  ) / 2
  rotation <-
    if (is.na(x$rot)) {
      atan(plot_height_half / plot_width_half) * 180 / pi
    } else {
      x$rot
    }
  rotation_max90 <-
    if (abs(rotation) > 90) {
      abs(180 - abs(rotation))
    } else {
      abs(rotation)
    }
  target_text_width <- min(
    plot_width_half / cos(rotation_max90 * pi / 180),
    plot_height_half / cos((90 - rotation_max90) * pi / 180)
  ) * 2
  scale_to_target_width <- target_text_width /
    grid::convertUnit(grid::grobWidth(grid::textGrob(x$lab)), unitTo = "mm", val = TRUE) *
    x$width
  grid::grid.text(x$lab,
                  rot = rotation,
                  gp = grid::gpar(
                    cex = scale_to_target_width,
                    col = x$col,
                    fontface = x$fontface,
                    alpha = x$alpha),
                  x = grid::unit(x$pos[1], "npc"), y = grid::unit(x$pos[2], "npc"), just = x$align
  )
}

#-------------------------------------------------------------------------------
#' @name tblNCA_progress
#' @title Modified NonCompart::tblNCA to support progress bars
#'
#' @inheritParams NonCompart::tblNCA
#' @param show_progress display progress messages
#' @export
#-------------------------------------------------------------------------------

tblNCA_progress <- function (concData, key = "Subject", colTime = "Time", colConc = "conc",
                             dose = 0, adm = "Extravascular", dur = 0, doseUnit = "mg",
                             timeUnit = "h", concUnit = "ug/L", down = "Linear", R2ADJ = 0,
                             MW = 0, SS = FALSE, iAUC = "", excludeDelta = 1, show_progress = TRUE)
{
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
  Res = vector()
  withProgress(message = "Calculating NCA", value = 0, {
    for (i in 1:nID) {
      if(show_progress) {setProgress(value = i / nID, detail = paste0("Subject ", i, "/", nID))}
      strHeader = paste0(key[1], "=", IDs[i, 1])
      strCond = paste0("concData[concData$", key[1], "=='",
                       IDs[i, 1], "'")
      if (nKey > 1) {
        for (j in 2:nKey) {
          strCond = paste0(strCond, " & concData$", key[j],
                           "=='", IDs[i, j], "'")
          strHeader = paste0(strHeader, ", ", key[j], "=",
                             IDs[i, j])
        }
      }
      strCond = paste0(strCond, ",]")
      tData = eval(parse(text = strCond))
      if (nrow(tData) > 0) {
        tRes = NonCompart::sNCA(tData[, colTime], tData[, colConc], dose = dose[i],
                    adm = adm, dur = dur, doseUnit = doseUnit, timeUnit = timeUnit,
                    concUnit = concUnit, R2ADJ = R2ADJ, down = down,
                    MW = MW, SS = SS, iAUC = iAUC, Keystring = strHeader,
                    excludeDelta = excludeDelta)
        Res = rbind(Res, tRes)
      }
    }
  })
  Res = cbind(IDs, Res)
  rownames(Res) = NULL
  colnames(Res)[1:nKey] = key
  attr(Res, "units") = c(rep("", nKey), attr(tRes, "units"))
  return(Res)
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
#' @export
#-------------------------------------------------------------------------------

pdfNCA_wm <- function (fileName = "Temp-NCA.pdf", concData, key = "Subject",
                       colTime = "Time", colConc = "conc", dose = 0, adm = "Extravascular",
                       dur = 0, doseUnit = "mg", timeUnit = "h", concUnit = "ug/L",
                       down = "Linear", R2ADJ = 0, MW = 0, SS = FALSE, iAUC = "",
                       excludeDelta = 1, watermark = TRUE, internal_version = TRUE, debug_msg = TRUE,
                       show_progress = TRUE)
{

  if(debug_msg) {
    message(fileName)
    message(getwd())
  }

  if(!internal_version) { # Workaround for AWS hosting
    #fileName <- paste0("/tmp/", fileName) # didn't work
    setwd("/tmp") # Trying setwd method
  }

  if(debug_msg) {
    message(getwd())
  }

  class(concData) = "data.frame"
  defPar = par(no.readonly = TRUE)

  if(debug_msg) {
    message("Trying to open pdf device")
  }
  ncar::PrepPDF(fileName)

  ncar::AddPage()
  ncar::Text1(1, 1, "Individual Noncompartmental Analysis Result (Non-Validated)",
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
  Res = vector()
  for (i in 1:nID) {
    if(show_progress) {
      shiny::incProgress(1/nID, detail = paste("Subject ", i, "/", nID))
    }
    strHeader = paste0(key[1], "=", IDs[i, 1])
    strCond = paste0("concData[concData$", key[1], "=='",
                     IDs[i, 1], "'")
    if (nKey > 1) {
      for (j in 2:nKey) {
        strCond = paste0(strCond, " & concData$", key[j],
                         "=='", IDs[i, j], "'")
        strHeader = paste0(strHeader, ", ", key[j], "=",
                           IDs[i, j])
      }
    }
    strCond = paste0(strCond, ",]")
    tData = eval(parse(text = strCond))
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
      Res = c(Res, txtRes)
      ncar::AddPage(Header1 = strHeader)
      ncar::TextM(txtRes, StartRow = 1, Header1 = strHeader)
      scrnmat = matrix(0, 3, 4)
      scrnmat[1, ] = c(0, 1, 0, 1)
      scrnmat[2, ] = c(0.1, 0.9, 0.5, 0.95)
      scrnmat[3, ] = c(0.1, 0.9, 0.05, 0.5)
      ScrNo = split.screen(scrnmat)
      screen(ScrNo[1])
      par(adj = 0)
      ncar::Text1(1, 1, strHeader, Cex = 1)
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
  ncar::ClosePDF()

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

#' @export
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
    print(total_doses)
  }

  return(total_doses)
}

#-------------------------------------------------------------------------------
#' @name search_id_col
#' @title Search for Likely ID Columns
#'
#' @description
#' Searches through several common column names that could be used to create the
#' "ID" column in the dataset, and push that to the first column of the dataset
#'
#' @param orig_df The dataframe used for searching
#' @param names_of_id_cols Likely column names, in order of search priority
#'
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @export
#-------------------------------------------------------------------------------

search_id_col <- function(orig_df,
                          names_of_id_cols = c("SUBJIDN", "SUBJID", "USUBJID", "PTNO")) {

  df <- orig_df

  for(i in seq_along(names_of_id_cols)) {
    if('ID' %in% names(df)) {
      return(df)
    } else {
      if(names_of_id_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(ID = !!dplyr::sym(names_of_id_cols[i])) %>%
          dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("ID column has been created from '", names_of_id_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop

  return(orig_df) # if can't find any
}

#-------------------------------------------------------------------------------
#' @name search_time_col
#' @title Search for Likely TIME Columns
#'
#' @description
#' Searches through several common column names that could be used to create the
#' "TIME" column in the dataset
#'
#' @param orig_df The dataframe used for searching
#' @param names_of_time_cols Likely column names, in order of search priority
#'
#' @returns a dataframe
#' @importFrom dplyr mutate select
#' @export
#-------------------------------------------------------------------------------

search_time_col <- function(orig_df,
                            names_of_time_cols = c("TAFD", "TSFD", "ATFD", "ATSD")) {

  df <- orig_df

  for(i in seq_along(names_of_time_cols)) {
    if('TIME' %in% names(df)) {
      return(df)
    } else {
      if(names_of_time_cols[i] %in% names(df)) {
        df <- df %>% dplyr::mutate(TIME = !!dplyr::sym(names_of_time_cols[i])) #%>%
        #dplyr::select(ID, dplyr::everything())
        shiny::showNotification(paste0("TIME column has been created from '", names_of_time_cols[i], "' column."), type = "message", duration = 10)
      }
    }
  } # end of loop

  return(orig_df) # if can't find any
}

#-------------------------------------------------------------------------------
#' @name expand_addl_ii
#' @title Expand ADDL and II dosing rows
#'
#' @description
#' Expands ADDL and II dosing rows. If there are no ADDL and II columns, return
#' original dataframe unchanged
#'
#' @param data The dataframe used for expansion
#' @param x_axis The x_axis variable, usually TIME or TAFD etc
#' @param dose_col The dose variable, usually AMT or DOSE
#' @param debug Show debugging messages
#'
#' @returns a dataframe
#' @importFrom dplyr filter mutate arrange rename rowwise ungroup
#' @importFrom tidyr unnest
#' @export
#-------------------------------------------------------------------------------

expand_addl_ii <- function(data, x_axis, dose_col, debug = FALSE) {

  data$DOSETIME <- as.numeric(as.character(data[[x_axis]])) # Still create DOSETIME if no ADDL II is found

  # Check if necessary columns are present
  if(all(c("ADDL", "II", "EVID", x_axis, dose_col) %in% colnames(data))) {

    data$ADDL <- as.integer(data$ADDL) # number of additional doses must be whole numbers
    data$II   <- as.numeric(data$II)

    # Split data into rows with ADDL > 0 and others
    addl_rows <- data %>% dplyr::filter(ADDL > 0 & !is.na(II))
    non_addl_rows <- data %>%
      dplyr::filter(!(ADDL > 0 & !is.na(II)))

    # message(paste0("nrow(addl_rows): ", nrow(addl_rows)))
    # message(paste0("nrow(non_addl_rows): ", nrow(non_addl_rows)))

    # Process addl_rows to expand
    if(nrow(addl_rows) > 0) { # Edge case where entire dataset has ADDL == 0
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

  if("RATE" %in% colnames(df)) { # NAMT, SAMT, min_yvar is previously calculated

    df$RATE <- as.numeric(as.character(data$RATE))

    df <- df %>%
      select(ID, facet_label, DOSETIME, !!dplyr::sym(x_axis), !!dplyr::sym(dose_col), NAMT, SAMT, min_yvar, RATE)
  } else {
    df <- df %>%
      select(ID, facet_label, DOSETIME, !!dplyr::sym(x_axis), !!dplyr::sym(dose_col), NAMT, SAMT, min_yvar)
  }

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
#' @export
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
#' @export
#-------------------------------------------------------------------------------

exposures_table <- function(input_simulated_table,
                            output_conc,
                            start_time = NULL,
                            end_time   = NULL,
                            carry_out  = "ID",
                            debug      = FALSE
) {

  input_simulated_table$YVARNAME <- input_simulated_table[[output_conc]]

  base_columns      <- c("ID", "YVARNAME", "TIME")
  columns_to_select <- c(base_columns, carry_out)

  input_simulated_table <- input_simulated_table %>%
    dplyr::select(dplyr::any_of(columns_to_select))

  # if (debug) {
  #   message(start_time)
  #   message(end_time)
  #   tmp <- input_simulated_table %>% dplyr::filter(TIME >= start_time)
  #   message(dplyr::glimpse(tmp))
  #   tmp2 <- input_simulated_table %>% dplyr::filter(TIME <= start_time)
  #   message(dplyr::glimpse(tmp2))
  # }

  metrics_table <- input_simulated_table %>% dplyr::filter(TIME >= start_time, TIME <= end_time) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(Cmin  = min(YVARNAME, na.rm = TRUE)[1],
                  Cmax  = max(YVARNAME, na.rm = TRUE)[1], ### First element if multiple values found
                  Cavg  = mean(YVARNAME, na.rm = TRUE),
                  Clast = dplyr::last(YVARNAME, na_rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Calculate TMAX in a separate table
  tmax_table <- metrics_table %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(Tmin = TIME[which.min(YVARNAME)[1]],
                     Tmax = TIME[which.max(YVARNAME)[1]]) %>% # First element if multiple values found
    dplyr::ungroup()

  metrics_table <- left_join(metrics_table, tmax_table, by = "ID") %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(YLAG          = dplyr::lag(YVARNAME),
                  XLAG          = dplyr::lag(TIME),
                  dYVAR         = (YVARNAME + YLAG) * (TIME - XLAG) * 0.5, # Area for trapezoid
                  dYVAR         = dplyr::if_else(is.na(dYVAR), 0, dYVAR),
                  AUC           = sum(dYVAR)) %>%
    dplyr::ungroup()

  list_of_exposures <- c("ID", "Cmin", "Cmax", "Cavg", "Clast", "AUC", "Tmax", "Tmin")

  metrics_table_id <- metrics_table %>%
    dplyr::select(dplyr::any_of(c(list_of_exposures, carry_out))) %>%
    dplyr::distinct(ID, .keep_all = TRUE)

  return(metrics_table_id)
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
#' @importFrom forcats fct_inorder
#' @export
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
  input_dataset$MODEL <- as.factor(input_dataset$MODEL) %>%
    forcats::fct_inorder()

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
#' @name calculate_quantiles
#'
#' @title Function to split a continuous X-variable for a number of quantiles
#'
#' @param df              Name of dataframe
#' @param xvar            Name of x-axis variable to split by
#' @param num_quantiles   Number of discrete quantiles
#'
#' @returns a dataframe containing the quantile limits
#' @importFrom dplyr select mutate distinct starts_with
#' @export
#-------------------------------------------------------------------------------

calculate_quantiles <- function(df, xvar, num_quantiles) {

  probs <- seq(1/as.numeric(num_quantiles), 1, length.out = as.numeric(num_quantiles))

  for (i in seq_along(probs)) {
    df <- df %>%
      dplyr::mutate(!!paste0("Q", i) := quantile(.data[[xvar]], probs = probs[i], na.rm = TRUE))
  }

  df %>%
    dplyr::select(starts_with("Q")) %>%
    dplyr::distinct()
}

#-------------------------------------------------------------------------------
#' @name quantile_ranges_name
#'
#' @title Function that stores the names and ranges of quantiles as a
#' character string to be used in Figure footnotes etc (Not used)
#'
#' @param quantiles_df    Name of dataframe containing columns corresponding to number of quantiles
#' @param df_orig         Name of original dataframe containing the continuous x-axis variable
#' @param xvar            Name of x-axis variable to split by
#'
#' @returns a character string containing the quantile limits
#' @export
#-------------------------------------------------------------------------------

quantile_ranges_name <- function(quantiles_df, df_orig, xvar) {
  range_name <- ""
  for(i in seq_along(quantiles_df)) {
    if(i == 1) { # Special case for first quantile
      range_name <- paste0(range_name, "Q", i, " = [", round(min(df_orig[[xvar]], na.rm = TRUE)), " - ", round(quantiles_df[[names(quantiles_df)[i]]]), "]")
    } else {
      range_name <- paste0(range_name, ", Q", i, " = (", round(quantiles_df[[names(quantiles_df)[i-1]]]), " - ", round(quantiles_df[[names(quantiles_df)[i]]]), "]")
    }
  }
  return(range_name)
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
#' @importFrom purrr map2
#' @importFrom dplyr case_when filter mutate sym syms summarise
#' @importFrom forcats fct_relevel
#' @returns a character string containing the quantile limits
#' @export
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

  conditions <- purrr::map2(
    quantiles_df,
    names(quantiles_df),
    ~rlang::expr(!!dplyr::sym(xvar) <= !!.x ~ !!.y)
  )

  conditions <- c(rlang::expr(is.na(!!dplyr::sym(xvar)) ~ "NA"), conditions)

  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(!!!conditions)) # triple-bang for list

  # Rename NAs
  df <- df %>%
    dplyr::mutate(Quantile = dplyr::case_when(is.na(Quantile) ~ "NA", TRUE ~ Quantile))

  # Reorder factor levels to place "NA" first
  df <- df %>%
    dplyr::mutate(Quantile = forcats::fct_relevel(Quantile, "NA", names(quantiles_df)))

  return(df)
}

#-------------------------------------------------------------------------------
#' @name create_facet_label
#'
#' @title Function that combines multiple columns to create a new one that contains
#' the facet labels
#'
#' @param df              Name of input dataframe
#' @param sort_by         A list containing columns to create label by. Uses first row only
#'
#' @importFrom dplyr group_by slice arrange syms rowwise mutate c_across all_of ungroup select
#' @importFrom dplyr left_join filter
#' @returns a dataframe with the new column called "facet_label"
#' @export
#-------------------------------------------------------------------------------

create_facet_label <- function(df, sort_by) {

  if(length(sort_by) == 1 && sort_by == "ID") { # Edge case
    df <- df %>%
      dplyr::mutate(facet_label = paste0("ID: ", ID))
  } else {

    # Remove "ID" from sort_by if it exists
    sort_by <- setdiff(sort_by, "ID")

    df <- df %>% dplyr::arrange(!!!dplyr::syms(sort_by))

    # Group by ID, then slice to keep only the first row of each group
    df_first <- df %>%
      dplyr::distinct(ID, .keep_all = TRUE)

    # Create the facet_label using the first row values of each ID group
    df_first <- df_first %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        facet_label = paste0("ID: ", ID, ", ", sapply(unlist(sort_by), function(col_name) {
          col_value <- dplyr::c_across(dplyr::all_of(col_name))
          paste0(col_name, ": ", col_value)
        })
        %>% paste(collapse = ", ")
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(ID, facet_label)

    df <- dplyr::left_join(df, df_first, by = "ID")
  }
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
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

#-------------------------------------------------------------------------------
#' @name calculate_tick_size
#'
#' @title Calculates a nice tick size for tornado plots
#'
#' @param abs_y                 Range of y-axis values (absolute value)
#' @param max_ticks             Maximum number of ticks
#' @param nice_values           Ticks should be multiples of these values
#'
#' @returns a numeric of optimal tick size
#' @export
#-------------------------------------------------------------------------------

calculate_tick_size <- function(abs_y, max_ticks = 12, nice_values = c(1,2,5,10)) {
  approx_tick_size <- abs_y / max_ticks

  # Find the nearest "nice" tick size (1, 2, 5, or 10 times a power of 10)
  magnitude      <- 10^floor(log10(approx_tick_size))  # Power of 10
  possible_ticks <- nice_values * magnitude      # Generate possible tick sizes
  tick_size <- min(possible_ticks[possible_ticks >= approx_tick_size]) # Return the minimum possible_ticks that exceeds approx_tick_size
  return(tick_size)
}

#-------------------------------------------------------------------------------
#' @name tornado_plot
#'
#' @title Draws a tornado plot
#'
#' @param df                    Input df containing parameter name, lower bound name, and upper bound name
#' @param param_name            Column name for parameters
#' @param lower_name            Column name for lower bounds
#' @param upper_name            Column name for upper bounds
#' @param lower_color           Fill color for lower bounds in bars
#' @param upper_color           Fill color for upper bounds in bars
#' @param reference_value       Untransformed reference value
#' @param metric_name           Exposure metric name to be used for plot label
#' @param plot_title            Plot title
#' @param display_as            A choice between "Ratio", "Percentage", and "Value"
#' @param filter_rows           Filter by X number of rows
#' @param display_text          When TRUE, displays the size as a text labl at the end of each bar
#' @param xlabname              Custom name for X-axis (technically the Y-axis due to coord_flip)
#' @param bioeq_lines           When TRUE, plots the 80% / 125% lines relative to reference value
#'
#' @importFrom dplyr mutate select arrange slice_tail case_when if_else rename sym
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggplot geom_rect theme_bw geom_hline scale_x_continuous scale_fill_manual coord_flip
#' @importFrom ggplot2 scale_y_continuous aes labs ggtitle
#' @importFrom scales percent pretty_breaks
#' @returns a ggplot object
#' @export
#-------------------------------------------------------------------------------

tornado_plot <- function(df,
                         reference_value,
                         param_name    = ".paramname",
                         lower_name    = "Lower",
                         upper_name    = "Upper",
                         lower_color   = "#FC8D62", # "#1B9E77",
                         upper_color   = "#66C2A5", # "#7570B3",
                         metric_name   = "",
                         plot_title    = "",
                         display_as    = "Percentage",
                         filter_rows   = 20,
                         display_text  = FALSE,
                         xlabname      = "",
                         bioeq_lines   = FALSE) {

  # width of columns in plot (value between 0 and 1)
  width <- 0.95

  df <- df %>%
    dplyr::rename(Parameter = !!dplyr::sym(param_name),
                  Lower     = !!dplyr::sym(lower_name),
                  Upper     = !!dplyr::sym(upper_name))

  # get data frame in shape for ggplot and geom_rect
  df2 <- df %>%
    dplyr::mutate(delta_upper = Upper - reference_value,
                  delta_lower = Lower - reference_value,
                  del = abs(delta_upper) +  abs(delta_lower)) %>%
    tidyr::pivot_longer(cols = c(Lower, Upper), names_to = "Level", values_to = "output_pretransform") %>%
    dplyr::arrange(del) %>%
    dplyr::select(Parameter, Level, output_pretransform)

  if(filter_rows != "" & is.numeric(filter_rows)) {
    if(filter_rows > 0) {
      df2 <- df2 %>%
        dplyr::slice_tail(n = filter_rows * 2) # multiply by 2 because it is upper + lower
    }
  }

  df2 <- df2 %>%
    dplyr::mutate(Parameter = forcats::fct_inorder(Parameter))

  if(display_as == "Ratio") {
    df2 <- df2 %>%
      dplyr::mutate(output = output_pretransform / reference_value)
    ref_line   <- 1
    bioeq_high <- 1.25
    bioeq_low  <- 0.80
    label_name <- paste0(metric_name, " (Ratio to Reference)")
  }

  if(display_as == "Percentage") {
    df2 <- df2 %>%
      dplyr::mutate(output = (output_pretransform - reference_value) / reference_value)
    ref_line   <- 0
    bioeq_high <- 0.25
    bioeq_low  <- -0.2
    label_name <- paste0(metric_name, " (% Change From Reference)")
  }

  if(display_as == "Value") {
    df2 <- df2 %>%
      dplyr::mutate(output = output_pretransform)
    ref_line   <- reference_value
    bioeq_high <- reference_value * 1.25
    bioeq_low  <- reference_value * 0.8
    label_name <- paste0(metric_name, " Value")
  }

  # Calculate text labels and whether to plot them
  if(display_text) {
    # Add positions for text labels
    df2 <- df2 %>%
      dplyr::mutate(percent_change = (output_pretransform - reference_value) / reference_value * 100,
                    label_text     = dplyr::case_when(
                      percent_change > -1 & percent_change < 1             ~ "", # Don't plot text if it is sufficient close to Reference, i.e. within 1%
                      display_as == "Percentage"                           ~ as.character(round(output * 100, 0)),
                      TRUE                                                 ~ as.character(round(output, 2))
                    )
      ) # Rounded values for display

    if(display_as == "Percentage") {
      df2 <- df2 %>% dplyr::mutate(label_text = dplyr::if_else(label_text == "", "", paste0(label_text, "%")))
    }
  }

  df2 <- df2 %>%
    dplyr::mutate(ymin=pmin(output, ref_line),
                  ymax=pmax(output, ref_line),
                  xmin=as.numeric(Parameter)-width/2,
                  xmax=as.numeric(Parameter)+width/2,
                  tooltip = paste0("Parameter: ", Parameter,
                                   "<br>Value: ", round(output, 2)))  # Tooltip content for plotly

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = df2, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=Level, text = tooltip)) + # Add tooltip content for plotly
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = ref_line) +
    ggplot2::scale_x_continuous(breaks = seq_along(unique(df2$Parameter)),
                                labels = unique(df2$Parameter)) +
    ggplot2::scale_fill_manual(values = c(Lower = lower_color, Upper = upper_color) ) +
    ggplot2::coord_flip()

  if(bioeq_lines) {
    p <- p +
      ggplot2::geom_hline(yintercept = bioeq_high, linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_hline(yintercept = bioeq_low,  linetype = "dashed", alpha = 0.5)
  }

  if(display_as == "Ratio" | display_as == "Percentage") {
    max_y <- max(df2$ymax, na.rm = TRUE)
    min_y <- min(df2$ymin, na.rm = TRUE)

    if(is.finite(max_y) & is.finite(min_y)) {

      if(bioeq_lines) {

        if(display_as == "Percentage") {
          abs_y <- pmax(abs(max_y) + abs(min_y), bioeq_high) # including high bioeq line in case where fold-changes are miniscule
        }
        if(display_as == "Ratio") {
          abs_y <- pmax((abs(max_y - 1) + abs(min_y - 1)), bioeq_high - 1 )
        }
      } else {
        if(display_as == "Percentage") {
          abs_y <- pmax(abs(max_y) + abs(min_y))
        }
        if(display_as == "Ratio") {
          abs_y <- pmax((abs(max_y - 1) + abs(min_y - 1)))
        }
      }

      tick_size <- calculate_tick_size(abs_y = abs_y, max_ticks = 12, nice_values = c(1,2,5,10))

      if(display_as == "Percentage") {

        # Align min_y to the nearest multiple of tick_size
        aligned_min_y <- floor(min_y / tick_size) * tick_size
        aligned_max_y <- ceiling(max_y / tick_size) * tick_size

        if(bioeq_lines) {
          define_limits <- seq(pmin(aligned_min_y, bioeq_low), pmax(aligned_max_y, bioeq_high), tick_size)
        } else {
          define_limits <- seq(aligned_min_y, aligned_max_y, tick_size)
        }
        p <- p + ggplot2::scale_y_continuous(breaks = define_limits, labels = function(x) paste0(x * 100, "%"))

      } else {

        if(bioeq_lines) {
          define_limits <- seq(0, pmax(max_y + tick_size, bioeq_high), tick_size)
        } else {
          define_limits <- seq(0, max_y + tick_size, tick_size)
        }

        p <- p + ggplot2::scale_y_continuous(breaks = define_limits)
      }
    } # end of finite y check
  } # end of display as percentage or ratio check

  if(display_as == "Value") {
    p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  }

  # Add text labels at both ends of each bar
  if(display_text) {
    df2$Parameter <- as.numeric(df2$Parameter)
    p <- p +
      ggplot2::geom_text(data = df2,
                         ggplot2::aes(x = Parameter,
                                      y = output,
                                      label = label_text),
                         #size = 4, # Adjust text size
                         hjust = ifelse(df2$output_pretransform > reference_value, -0.2, 1.2), # Adjust vertical alignment
                         vjust = 0.5) # Adjust horizontal alignment
  }

  if(xlabname != "") {label_name <- xlabname}

  p <- p +
    ggplot2::labs(x = NULL, y = label_name) +
    ggplot2::ggtitle(plot_title)

  return(p)
}

#-------------------------------------------------------------------------------
#' @name split_reference_value
#'
#' @title Split reference values evenly between lower and upper bound for batch run table
#'
#' @param reference_value       The reference value to be used for splitting
#' @param lower_bound           The lower bound to be used for splitting
#' @param upper_bound           The upper bound to be used for splitting
#'
#' @returns a list of splits excluding reference value
#' @export
#-------------------------------------------------------------------------------
split_reference_value <- function(reference_value, lower_bound, upper_bound) {
  # Generate 4 evenly spaced values between Lower and Reference (excluding Reference and the bound)
  lower_splits <- seq(lower_bound, reference_value, length.out = 5)[c(-1, -5)]
  
  # Generate 4 evenly spaced values between Reference and Upper (excluding Reference and the bound)
  upper_splits <- seq(reference_value, upper_bound, length.out = 5)[c(-1, -5)]
  
  # Return splits as a list
  return(list(lower_splits = lower_splits, upper_splits = upper_splits))
}

#-------------------------------------------------------------------------------
#' @name spider_plot
#'
#' @title Draws a spider plot
#'
#' @param df                    Input df containing parameter name, ratio (fold-change), and metric
#' @param param_name            Column name for parameters
#' @param ratio_name            Column name for ratio
#' @param metric_name           Column name for metric
#' @param reference_value       Untransformed reference value
#' @param plot_title            Plot title
#' @param display_as            A choice between "Ratio", "Percentage", and "Value"
#' @param filter_rows           Filter by X number of rows
#' @param bioeq_lines           When TRUE, plots the 80% / 125% lines relative to reference value
#' @param ylabname              Custom name for y-axis
#' @param normalize_x_axis      Treat x-axis as a factor
#'
#' @importFrom dplyr mutate select arrange slice_tail case_when if_else rename sym
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_inorder
#' @importFrom ggplot2 ggplot geom_rect theme_bw geom_hline scale_x_continuous scale_fill_manual coord_flip
#' @importFrom ggplot2 scale_y_continuous aes labs ggtitle
#' @importFrom scales percent pretty_breaks
#' @returns a ggplot object
#' @export
#-------------------------------------------------------------------------------

spider_plot <-  function(df,
                         reference_value,
                         param_name    = ".paramname",
                         ratio_name    = ".ratio",
                         metric_name   = "Cmax",
                         plot_title    = "",
                         display_as    = "Value",
                         filter_rows   = 20,
                         ylabname      = "",
                         normalize_x_axis = FALSE,
                         bioeq_lines   = FALSE) {
  
  df <- df %>%
    dplyr::rename(Parameter = !!dplyr::sym(param_name),
                  Ratio     = !!dplyr::sym(ratio_name),
                  Metric    = !!dplyr::sym(metric_name))
  
  # get data frame in shape for ggplot and geom_rect
  df2 <- df %>%
    dplyr::mutate(output_pretransform = Metric) #%>%
  #dplyr::arrange(Metric)
  
  df2 <- df2 %>%
    dplyr::group_by(Parameter) %>%
    dplyr::mutate(del = abs(max(Metric)) +  abs(min(Metric))) %>%
    dplyr::arrange(desc(del)) %>%
    dplyr::ungroup()
  
  df2 <- df2 %>%
    dplyr::mutate(Parameter = forcats::fct_inorder(Parameter))
  
  param_levels <- levels(df2$Parameter)
  
  
  if(filter_rows != "" & is.numeric(filter_rows)) {
    if(filter_rows > 0) {
      params_to_retain <- param_levels[1:filter_rows]
      df2 <- df2 %>%
        dplyr::filter(Parameter %in% params_to_retain)
    }
  }
  
  
  if(display_as == "Ratio") {
    df2 <- df2 %>%
      dplyr::mutate(output = output_pretransform / reference_value)
    ref_line   <- 1
    bioeq_high <- 1.25
    bioeq_low  <- 0.80
    label_name <- paste0(metric_name, " (Ratio to Reference)")
  }
  
  if(display_as == "Percentage") {
    df2 <- df2 %>%
      dplyr::mutate(output = (output_pretransform - reference_value) / reference_value)
    ref_line   <- 0
    bioeq_high <- 0.25
    bioeq_low  <- -0.2
    label_name <- paste0(metric_name, " (% Change From Reference)")
  }
  
  if(display_as == "Value") {
    df2 <- df2 %>%
      dplyr::mutate(output = output_pretransform)
    ref_line   <- reference_value
    bioeq_high <- reference_value * 1.25
    bioeq_low  <- reference_value * 0.8
    label_name <- paste0(metric_name, " Value")
  }
  
  if(normalize_x_axis) {
    df2$RatioN <- round(df2$Ratio, 3)
    df2$RatioN <- factor(df2$RatioN, levels = sort(unique(df2$RatioN)))
  }
  
  if(normalize_x_axis) {
    p <- ggplot2::ggplot(data = df2, ggplot2::aes(x = RatioN, y = output, color = Parameter, group = Parameter))
  } else {
    p <- ggplot2::ggplot(data = df2, ggplot2::aes(x = Ratio, y = output, color = Parameter))
  }
  
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::geom_point() + 
    ggplot2::geom_line()
  
  if(bioeq_lines) {
    p <- p +
      ggplot2::geom_hline(yintercept = bioeq_high, linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_hline(yintercept = bioeq_low,  linetype = "dashed", alpha = 0.5)
  }
  
  if(display_as == "Ratio" | display_as == "Percentage") {
    max_y <- max(df2$output, na.rm = TRUE)
    min_y <- min(df2$output, na.rm = TRUE)
    
    if(is.finite(max_y) & is.finite(min_y)) {
      
      if(bioeq_lines) {
        
        if(display_as == "Percentage") {
          abs_y <- pmax(abs(max_y) + abs(min_y), bioeq_high) # including high bioeq line in case where fold-changes are miniscule
        }
        if(display_as == "Ratio") {
          abs_y <- pmax((abs(max_y - 1) + abs(min_y - 1)), bioeq_high - 1 )
        }
      } else {
        if(display_as == "Percentage") {
          abs_y <- pmax(abs(max_y) + abs(min_y))
        }
        if(display_as == "Ratio") {
          abs_y <- pmax((abs(max_y - 1) + abs(min_y - 1)))
        }
      }
      
      tick_size <- calculate_tick_size(abs_y = abs_y, max_ticks = 10, nice_values = c(1,2,5,10))
      
      if(display_as == "Percentage") {
        
        # Align min_y to the nearest multiple of tick_size
        aligned_min_y <- floor(min_y / tick_size) * tick_size
        aligned_max_y <- ceiling(max_y / tick_size) * tick_size
        
        if(bioeq_lines) {
          define_limits <- seq(pmin(aligned_min_y, bioeq_low), pmax(aligned_max_y, bioeq_high), tick_size)
        } else {
          define_limits <- seq(aligned_min_y, aligned_max_y, tick_size)
        }
        p <- p + ggplot2::scale_y_continuous(breaks = define_limits, labels = function(x) paste0(x * 100, "%"))
        
      } else {
        
        if(bioeq_lines) {
          define_limits <- seq(0, pmax(max_y + tick_size, bioeq_high), tick_size)
        } else {
          define_limits <- seq(0, max_y + tick_size, tick_size)
        }
        
        p <- p + ggplot2::scale_y_continuous(breaks = define_limits)
      }
    } # end of finite y check
  } # end of display as percentage or ratio check
  
  if(display_as == "Value") {
    p <- p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
  }
  
  if(!normalize_x_axis) {
    number_of_breaks <- length(unique(round(df2$Ratio, 3)))
    #message("num of breaks: ", number_of_breaks)
    if(number_of_breaks <= 9) { # standard number of breaks is 9
      p <- p + ggplot2::scale_x_continuous(breaks = unique(round(df2$Ratio, 3)), labels = unique(round(df2$Ratio, 3)))
    } else {
      p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))  
    }
  }
  
  if(ylabname != "") {label_name <- ylabname}
  
  p <- p +
    ggplot2::labs(x = "Parameter Fold Change", y = label_name) +
    ggplot2::ggtitle(plot_title)
  
  return(p)
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
#' @export
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
#' @name safely_showNotification
#'
#' @title A safe version of shiny notifications, mostly for used in testing
#' @param ... args to be passed
#'
#' @returns A shiny showNotification or print to console
#' @importFrom shiny isRunning showNotification
#' @seealso `parse`
#' @export
#-------------------------------------------------------------------------------

safely_showNotification <- function(...) {
  if (shiny::isRunning()) {
    shiny::showNotification(...)
  } else {
    # Extract the 'ui' argument for a console-friendly message
    args <- list(...)
    msg <- if (!is.null(args$ui)) args$ui else args[[1]]
    type <- args$type %||% "message"
    message(paste0("[", toupper(type), "] ", msg))
  }
}

#-------------------------------------------------------------------------------
#' @name safely_withProgress
#'
#' @title A safe version of shiny withProgress, mostly for used in testing
#' @param ... args to be passed
#'
#' @returns A shiny withProgress bar or print to console
#' @importFrom shiny isRunning withProgress
#' @seealso `parse`
#' @export
#-------------------------------------------------------------------------------

safely_withProgress <- function(message, value = 0, expr) {
  if (shiny::isRunning()) {
    shiny::withProgress(message = message, value = value, expr)
  } else {
    message(paste0("[PROGRESS] ", message))
    force(expr)
  }
}

#-------------------------------------------------------------------------------
#' @name safely_incProgress
#'
#' @title A safe version of shiny incProgress, mostly for used in testing
#' @param ... args to be passed
#'
#' @returns A shiny withProgress bar or print to console
#' @importFrom shiny isRunning incProgress
#' @seealso `parse`
#' @export
#-------------------------------------------------------------------------------

safely_incProgress <- function(amount, detail = NULL) {
  if (shiny::isRunning()) {
    shiny::incProgress(amount, detail = detail)
  } else if (!is.null(detail)) {
    message(paste0("[PROGRESS] ", detail))
  }
}

#-------------------------------------------------------------------------------
#' @name translate_model_code
#'
#' @title Upload, and translate a PDF file to model code via external API
#'
#' @description
#' Sends a file to a Dify Workflow or LLM API for translation into model code.
#' The file should be pre-validated and optionally pre-combined using
#' \code{combine_uploaded_files} before being passed to this function.
#'
#' @param ready_path Path to the file to be processed. For multi-file uploads,
#'   this should be the combined output of \code{combine_uploaded_files}.
#' @param file_name Original filename (used for display in notifications and
#'   for MIME type detection). If \code{NULL}, derived from \code{ready_path}.
#' @param service Choice of "PROD" (BI-only), "EXP" (BI-only), "Gemini", "OpenAI", "AWS Bedrock",
#'  "Claude", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Apollo" (BI-only), "Azure OpenAI"
#' @param api_key API Key, recommended to store it as env var called "ANTHROPIC_API_KEY" etc
#' @param api_upload API URL for uploading of files (Dify requires a 2-step process)
#' @param api_chat API URL for chat messages, required when using OpenAI-compatible API
#' @param user_id user id for the request (BI-only)
#' @param model_gemini Model to be used when calling Gemini API
#' @param model_openai Model to be used when calling OpenAI API
#' @param model_anthropic Model to be used when calling Anthropic API
#' @param model_openrouter Model to be used when calling OpenRouter
#' @param model_openai_compatible Model to be used when calling OpenAI-compatible API
#' @param model_deepseek Model to be used when calling DeepSeek
#' @param model_apollo Model to be used when calling Apollo (BI only)
#' @param model_azure Model to be used when calling Azure OpenAI
#' @param model_aws Model to be used when calling AWS Bedrock
#' @param display_info Set to TRUE to show how much time/tokens in Shiny UI when job is finished
#' @param temperature Goes from 0 to 1, where 0 is deterministic
#' @param seed seed number for LLMs
#' @param locally_parse_file if TRUE, extracts text locally and includes in prompt instead of uploading file
#' @param model_lang Either "mrgsolve" or "nonmem" (changes the prompt)
#' @param deep_pdfscan Use Vision to extract image data (BI only)
#' @param force_parse Force parsing (BI only)
#' @param mrgsolve_system_prompt String for mrgsolve system prompt
#' @param mrgsolve_long_user_prompt String for mrgsolve long user prompt
#' @param mrgsolve_short_user_prompt String for mrgsolve short user prompt
#' @param nonmem_system_prompt String for nonmem system prompt
#' @param nonmem_long_user_prompt String for nonmem long user prompt
#' @param nonmem_short_user_prompt String for nonmem short user prompt
#' @param rxode2_system_prompt String for rxode2 system prompt
#' @param rxode2_long_user_prompt String for rxode2 long user prompt
#' @param rxode2_short_user_prompt String for rxode2 short user prompt
#' @param internal_version Logical. Only relevant for BI
#' @param debug Displays debug messages
#'
#' @returns a named list with \code{answer}, \code{conversation_id}, and
#'   \code{chat_obj}, or \code{NULL} on failure.
#' @importFrom shiny setProgress incProgress showNotification
#' @importFrom httr2 request req_headers req_body_multipart req_retry req_perform
#' @importFrom httr2 resp_body_json last_response resp_body_string req_body_json
#' @importFrom httr2 req_auth_bearer_token with_verbosity
#' @importFrom curl form_file
#' @importFrom mime guess_type
#' @importFrom pdftools pdf_text
#' @importFrom stats setNames
#' @export
#-------------------------------------------------------------------------------

translate_model_code <- function(ready_path,
                                 file_name = NULL,
                                 service,
                                 api_key,
                                 api_upload = NULL,
                                 api_chat = NULL,
                                 user_id = "mrgsolve_translator",
                                 model_gemini = "gemini-3-flash-preview",
                                 model_openai = "gpt-5-mini",
                                 model_anthropic = "claude-haiku-4-5-20251001",
                                 model_openrouter = "arcee-ai/trinity-large-preview:free",
                                 model_openai_compatible = "gpt-5-mini",
                                 model_deepseek = "deepseek-reasoner",
                                 model_apollo = "gpt-5.2",
                                 model_azure = "gpt-5.2",
                                 model_aws = "anthropic.claude-sonnet-4-6",
                                 display_info = TRUE,
                                 temperature = 0.1,
                                 seed = 42,
                                 locally_parse_file = FALSE,
                                 model_lang = "mrgsolve",
                                 deep_pdfscan = FALSE,
                                 force_parse = FALSE,
                                 mrgsolve_system_prompt,
                                 mrgsolve_long_user_prompt,
                                 mrgsolve_short_user_prompt,
                                 nonmem_system_prompt,
                                 nonmem_long_user_prompt,
                                 nonmem_short_user_prompt,
                                 rxode2_system_prompt,
                                 rxode2_long_user_prompt,
                                 rxode2_short_user_prompt,                                 
                                 internal_version = TRUE,
                                 debug = TRUE
) {

  file_path <- ready_path
  file_name <- file_name %||% basename(ready_path)

  is_text_file <- grepl("\\.(txt|mod|ctl|cpp|lst)$", file_name, ignore.case = TRUE)
  
  if(is_text_file) {
    detected_type <- "text/plain"
    new_path <- paste0(file_path, ".txt")
    file.copy(file_path, new_path)
    file_path <- new_path
    locally_parse_file <- TRUE
    safely_showNotification(paste0("Text file detected, will be parsed locally to bypass file upload."))
  } else {
    detected_type <- mime::guess_type(file_name)
  }

  model_name <- switch(service,
                       "PROD"              = "[PROD model]",
                       "PMx Co-Modeler"    = "[Apollo model]",
                       "EXP"               = "[EXP model]",
                       "Fast"              = "[Apollo model]",
                       "Gemini"            = model_gemini,
                       "OpenAI"            = model_openai,
                       "Claude"            = model_anthropic,
                       "OpenRouter"        = model_openrouter,
                       "OpenAI-Compatible" = model_openai_compatible,
                       "DeepSeek"          = model_deepseek,
                       "Apollo"            = model_apollo,
                       "Test Provider"     = model_apollo,
                       "Azure OpenAI"      = model_azure,
                       "AWS Bedrock"       = model_aws)
  
  models_no_temperature <- c("gpt-5-mini", "gpt-5-nano", "gpt-5", "o1", "o3-mini", "o4")
  
  if(debug) {
    print(paste0("translate_model_code: 0) model_name is: ", model_name))
  }
  
  if(model_name %in% models_no_temperature) {
    safely_showNotification(paste0(model_name, " does not support temperature setting."), type = "warning")
    optimal_params <- ellmer::params(seed = seed)
  } else {
    optimal_params <- ellmer::params(temperature = temperature, seed = seed)
  }
  
  if(debug) {
    print("translate_model_code: 1) API key check")
  }
  
  if(model_lang == "mrgsolve") {
    system_prompt     <- mrgsolve_system_prompt
    long_user_prompt  <- mrgsolve_long_user_prompt
    short_user_prompt <- mrgsolve_short_user_prompt
  }
  
  if(model_lang == "nonmem") {
    system_prompt     <- nonmem_system_prompt
    long_user_prompt  <- nonmem_long_user_prompt
    short_user_prompt <- nonmem_short_user_prompt
  }
  
  if(model_lang == "rxode2") {
    system_prompt     <- rxode2_system_prompt
    long_user_prompt  <- rxode2_long_user_prompt
    short_user_prompt <- rxode2_short_user_prompt    
  }
  
  if(service == "PROD" | service == "EXP" | service == "PMx Co-Modeler" | service == "Fast") {
    instruction_prompt <- short_user_prompt
  } else {
    instruction_prompt <- long_user_prompt
  }
  
  # ── Service-level format guard (still relevant for single-file non-PDF/text uploads) ──
  if (service %in% c("DeepSeek", "Apollo", "Test Provider")) {
    safely_showNotification(
      paste0(service, " currently only supports local parsing."),
      type = "warning"
    )
    locally_parse_file <- TRUE
  }
  
  if (service %in% c("OpenAI", "OpenRouter", "OpenAI-Compatible", "DeepSeek", "Apollo", "Test Provider") &&
      !detected_type %in% c("application/pdf", "text/plain")) {
    safely_showNotification(
      paste0(service, " currently only supports PDF or plain text format."),
      type = "error"
    )
    return(NULL)
  }
  
  if(debug) {
    print("translate_model_code: 2) result tryCatch")
  }
  
  # --- Initialize output variables outside tryCatch for clarity ---
  usage_info      <- list(input = 0, output = 0, total = 0, model = "unknown")
  conversation_id <- ""
  chat_obj        <- NULL
  elapsed         <- NA
  tokens_per_sec  <- "N/A"
  
  result <- tryCatch({
    
    safely_withProgress(message = 'Processing...', value = 0, {
      
      # --- Local file parsing ---
      if(locally_parse_file) {
        
        safely_incProgress(0.1, detail = paste("Parsing", file_name, "locally..."))
        
        instruction_prompt <- paste0(
          instruction_prompt,
          "\n\n--- FILE CONTENT START ---\n",
          extract_file_content(file_path, file_name, detected_type, instruction_prompt, debug),
          "\n--- FILE CONTENT END ---"
        )
        
      } else {
        safely_incProgress(0.1, detail = paste("Uploading", file_name, "..."))
      }
      
      start_time <- Sys.time()
      safely_incProgress(0.3, detail = paste0("Generating ", model_lang, " code using ", model_name, " (please be patient, takes ~1 min)..."))

      branch_result <- if (service == "PROD" | service == "EXP" | service == "PMx Co-Modeler" | service == "Fast") {
        
        if(debug) print("translate_model_code: 3) Dify branch")
        run_dify_chat(
          conversation_id    = NULL,
          instruction_prompt = instruction_prompt,
          api_key            = api_key,
          api_upload         = api_upload,
          api_chat           = api_chat,
          user_id            = user_id,
          file_path          = file_path,
          detected_type      = detected_type,
          locally_parse_file = locally_parse_file,
          deep_pdfscan       = deep_pdfscan,
          force_parse        = force_parse,
          model_name         = model_name,
          debug              = debug
        )
      } else {
        
        if(debug) print("translate_model_code: 3) Ellmer branch")
        run_ellmer_chat(
          chat_obj           = NULL,
          service            = service,
          model_name         = model_name,
          system_prompt      = system_prompt,
          optimal_params     = optimal_params,
          api_chat           = api_chat,
          instruction_prompt = instruction_prompt,
          file_path          = file_path,
          detected_type      = detected_type,
          locally_parse_file = locally_parse_file,
          internal_version   = internal_version,
          debug              = debug
        )
      } # end BRANCH B
      
      if(debug) {branch_model <<- branch_result}

      elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
      
      if(debug) print(paste0("translate_model_code: raw answer: ", branch_result$answer)) # Raw answer before cleaning
      
      # Return EVERYTHING needed after the tryCatch as a named list
      list(
        answer          = branch_result$answer,
        conversation_id = branch_result$conversation_id %||% "",
        chat_obj        = branch_result$chat_obj,
        elapsed         = elapsed,
        usage_info      = branch_result$usage_info
      )
      
    }) # end of withProgress
    
  }, error = function(e) {
    resp <- httr2::last_response()
    msg  <- if(!is.null(resp)) {
      tryCatch(httr2::resp_body_json(resp)$message, error = function(i) httr2::resp_body_string(resp))
    } else {
      e$message
    }
    if(debug) message("--- TRANSLATION ERROR ---\n", msg)
    safely_showNotification(paste("Translation Failed:", msg), type = "error", duration = NULL)
    return(NULL)
  })
  
  # Unpack cleanly after tryCatch
  if(!is.null(result)) {
    
    elapsed        <- result$elapsed
    usage_info     <- result$usage_info
    tokens_per_sec <- if(is.numeric(usage_info$total) && usage_info$total > 0) {
      round(usage_info$total / elapsed, 1)
    } else { "N/A" }
    
    if(display_info) {
      safely_showNotification(
        ui = shiny::tagList(
          shiny::tags$div(
            style = "font-size: 14px;",
            shiny::tags$b("Translation Complete"),
            shiny::tags$hr(style = "margin: 5px 0;"),
            shiny::tags$p(paste0("⏱ Time: ", elapsed, " seconds")),
            shiny::tags$p(paste0("🤖 Model: ", usage_info$model)),
            shiny::tags$p(paste0("🎟 Tokens: ", usage_info$total, " (", tokens_per_sec, " t/s)")),
            shiny::tags$p(style = "font-size: 11px; color: #666;",
                          paste0("In: ", usage_info$input, " | Out: ", usage_info$output))
          )
        ),
        type     = "message",
        duration = 25
      )
    }
    
    list_of_output <- list(
      answer          = result$answer,
      conversation_id = result$conversation_id,
      chat_obj        = result$chat_obj
    )
    
    if(debug) llm_result <<- list_of_output
    
    list_of_output$answer <- clean_llm_response(list_of_output$answer)
    
    return(list_of_output)
    
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
#' @name refine_model_code
#'
#' @title Provide a model code string and its compile error message to be refined
#'
#' @description
#' Sends a string containing model code and error message to a Dify Workflow or
#' LLM API for correction.
#'
#' @param model_code Input model code
#' @param error_message Input compilation error message
#' @param context Argument to hold chat_obj or conversation_id to keep same chat session
#' @param reuse_context Set to TRUE to re-use same conversation_id or chat_obj, more costly in terms of time and tokens
#' @param service Choice of "PROD", "EXP", "Gemini", "OpenAI", "Claude", "OpenRouter",
#'  "OpenAI-Compatible", "DeepSeek", "Apollo", "Azure OpenAI", "AWS Bedrock"
#' @param api_key API Key, recommended to store it as env var called "ANTHROPIC_API_KEY" etc (Dify only)
#' @param api_upload API URL for uploading of files (Dify requires a 2-step process)
#' @param api_chat API URL for chat messages, required for OpenAI-compatible
#' @param user_id user id for the request
#' @param model_gemini Model to be used when calling Gemini API
#' @param model_openai Model to be used when calling OpenAI API
#' @param model_anthropic Model to be used when calling Anthropic API
#' @param model_openrouter Model to be used when calling OpenRouter
#' @param model_openai_compatible Model to be used when calling OpenAI-compatible API
#' @param model_deepseek Model to be used when calling DeepSeek
#' @param model_apollo Model to be used when calling Apollo LLM
#' @param model_azure Model to be used when calling Azure OpenAI
#' @param model_aws Model to be used when calling AWS Bedrock
#' @param progress_bar Starting point of Shiny progress bar, relevant for 2nd try
#' @param max_retries Required to calculate a more accurate progress bar
#' @param display_info Set to TRUE to show how much time/tokens in Shiny UI when job is finished
#' @param temperature Goes from 0 to 1, where 0 is deterministic, if not reusing context
#' @param seed seed number for LLMs, if not reusing context
#' @param attempt current attempt number
#' @param deep_pdfscan Uses Vision to extract image data (BI only)
#' @param force_parse Force reparsing (BI only)
#' @param system_prompt Character. System prompt
#' @param long_user_prompt Character. Long user prompt
#' @param short_user_prompt Character. Short user prompt
#' @param internal_version Logical. Only relevant for BI
#' @param feedback_success Logical. Only relevant for BI, and if reuse_context = TRUE
#' @param debug Displays debug messages
#'
#' @returns a named list of "answer", "conversation_id", "chat_obj"
#' @importFrom stats setNames
#' @export
#-------------------------------------------------------------------------------

refine_model_code <- function(model_code,
                              error_message,
                              context = NULL, # New argument to hold chat_obj or conversation_id
                              reuse_context = FALSE,
                              service,
                              api_key,
                              api_upload = NULL,
                              api_chat = NULL,
                              user_id = "mrgsolve_translator",
                              model_gemini = "gemini-3-flash-preview",
                              model_openai = "gpt-5-mini",
                              model_anthropic = "claude-haiku-4-5-20251001",
                              model_openrouter = "arcee-ai/trinity-large-preview:free",
                              model_openai_compatible = "gpt-5-mini",
                              model_deepseek = "deepseek-reasoner",
                              model_apollo = "gpt-5.2",
                              model_azure = "gpt-5.2",
                              model_aws = "anthropic.claude-sonnet-4-6",
                              progress_bar = 0.4,
                              max_retries = 2,
                              display_info = TRUE,
                              temperature = 0,
                              seed = 42,
                              attempt = 1,
                              deep_pdfscan = FALSE,
                              force_parse = FALSE,
                              system_prompt,
                              long_user_prompt,
                              short_user_prompt,
                              internal_version,
                              feedback_success = FALSE,
                              debug = TRUE
) {
  
  model_name <- switch(service,
                       "PROD"              = "[PROD model]",
                       "PMx Co-Modeler"    = "[Apollo model]",
                       "EXP"               = "[EXP model]",
                       "Fast"              = "[Apollo model]",
                       "Gemini"            = model_gemini,
                       "OpenAI"            = model_openai,
                       "Claude"            = model_anthropic,
                       "OpenRouter"        = model_openrouter,
                       "OpenAI-Compatible" = model_openai_compatible,
                       "DeepSeek"          = model_deepseek,
                       "Apollo"            = model_apollo,
                       "Test Provider"     = model_apollo,
                       "Azure OpenAI"      = model_azure,
                       "AWS Bedrock"       = model_aws)
  
  models_no_temperature <- c("gpt-5-mini", "gpt-5-nano", "o1", "o3-mini")
  
  if(model_name %in% models_no_temperature) {
    safely_showNotification(paste0(model_name, " does not support temperature setting."), type = "warning")
    optimal_params <- ellmer::params(seed = seed)
  } else {
    optimal_params <- ellmer::params(temperature = temperature, seed = seed)
  }
  
  # Calculate the width of one retry segment (e.g., 0.2 if max_retries is 3)
  if(!feedback_success) {
    segment_width <- (1 - 0.4) / max_retries 
  } else {
    segment_width <- 0 # Setting progress bar to 0 when relaying success messages
  }

  if(service == "PROD" | service == "EXP" | service == "Fast" | service == "PMx Co-Modeler") {
    instruction_prompt <- short_user_prompt
  } else {
    instruction_prompt <- long_user_prompt
  }
  
  # Ensure error_message is a single string, not a vector of lines (i.e. if error comes from out$stderr)
  if (is.character(error_message) && length(error_message) > 1) {
    error_message <- paste(error_message, collapse = "\n")
  }
  
  retry_prompt <- paste0(
    instruction_prompt,
    "\n\n",
    "<model_code>\n",
    model_code, "\n",
    "</model_code>\n\n",
    "<error_message>\n",
    error_message, "\n",
    "</error_message>"
  )
  
  if(reuse_context) {
    safely_showNotification("Keeping same conversation to improve results...")
    if(feedback_success) {
      retry_prompt <- paste0(
        "Model compiled successfully!"
      )
      safely_showNotification(paste0(retry_prompt, " Feeding back to ", service, "."))
    }
  }
  
  if(debug) {print(paste0("refine_model_code: 1) ", retry_prompt))}
  
  result <- tryCatch({
    
    if(feedback_success) {
      process_msg <- "Relaying success..."
    } else {
      process_msg <- "Processing..."
    }
    
    safely_withProgress(message = process_msg, value = progress_bar, {
      
      if(!feedback_success) {
        safely_incProgress(segment_width * 0.33, detail = paste("Re-iterating after failed compilation..."))
      }
      
      start_time <- Sys.time()
      
      # --- BRANCH A: DIFY (Manual httr2 Logic) ---
      branch_result <- if(service == "PROD" | service == "EXP" | service == "Fast" | service == "PMx Co-Modeler") {
        run_dify_chat(
          conversation_id    = if(reuse_context) context$conversation_id else NULL,
          instruction_prompt = retry_prompt,
          api_key            = api_key,
          api_chat           = api_chat,
          user_id            = user_id,
          locally_parse_file = TRUE,
          model_name         = model_name,
          deep_pdfscan       = FALSE,
          force_parse        = FALSE,
          debug              = debug
        )
      } else {
        run_ellmer_chat(
          chat_obj           = if(reuse_context) context$chat_obj else NULL,
          system_prompt      = system_prompt,
          instruction_prompt = retry_prompt,
          api_chat           = api_chat,
          locally_parse_file = TRUE,
          optimal_params     = optimal_params,
          service            = service,
          model_name         = model_name,
          internal_version   = internal_version,
          debug              = debug
        )
      }
      
      if(debug) {branch_refine <<- branch_result}
      
      if(!feedback_success) {
        safely_incProgress(segment_width * 0.33, detail = paste("Cleaning response..."))
      }
      
      # Return everything needed after tryCatch as a named list
      list(
        answer          = branch_result$answer,
        conversation_id = branch_result$conversation_id %||% "",
        chat_obj        = branch_result$chat_obj,
        elapsed         = round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1),
        usage_info      = branch_result$usage_info
      )
      
    }) # end of withProgress
    
  }, error = function(e) {
    # Single unified error handler
    resp <- httr2::last_response()
    
    # Try to extract detailed error message
    msg <- if (!is.null(resp)) {
      error_json <- tryCatch(httr2::resp_body_json(resp), error = function(i) NULL)
      error_json$message %||% httr2::resp_body_string(resp)
    } else {
      e$message
    }
    
    if(debug) message("--- REFINEMENT ERROR ---\n", msg)
    safely_showNotification(paste("Refinement Failed:", msg), type = "error", duration = NULL)
    return(NULL)  # Graceful failure
  })
  
  # Unpack and display notification outside tryCatch
  if(!is.null(result)) {
    
    elapsed        <- result$elapsed
    usage_info     <- result$usage_info
    tokens_per_sec <- if(is.numeric(usage_info$total) && usage_info$total > 0) {
      round(usage_info$total / elapsed, 1)
    } else { "N/A" }
    
    if(debug) print(paste0("refine_model_code: 4) conversation_id: ", result$conversation_id))
    
    if(display_info) {
      safely_showNotification(
        ui = shiny::tagList(
          shiny::tags$div(
            style = "font-size: 14px;",
            shiny::tags$b(paste0("Retry Complete (#", attempt, ")")),
            shiny::tags$hr(style = "margin: 5px 0;"),
            shiny::tags$p(paste0("⏱ Time: ", elapsed, " seconds")),
            shiny::tags$p(paste0("🤖 Model: ", usage_info$model)),
            shiny::tags$p(paste0("🎟 Tokens: ", usage_info$total, " (", tokens_per_sec, " t/s)")),
            shiny::tags$p(style = "font-size: 11px; color: #666;",
                          paste0("In: ", usage_info$input, " | Out: ", usage_info$output))
          )
        ),
        type     = "message",
        duration = 25
      )
    }
  
    list_of_output <- list(
      answer          = result$answer,
      conversation_id = result$conversation_id,
      chat_obj        = result$chat_obj
    )
    
    # Only assign globally during development
    if(debug && !feedback_success) llm_refine <<- list_of_output
    if(debug && feedback_success) llm_success <<- list_of_output
    
    list_of_output$answer <- clean_llm_response(list_of_output$answer)
    
    if(feedback_success) {
      return(model_code) ## Not that it matters, since we're calling this function for its side effect when relaying success
    } else {
      return(list_of_output)      
    }
    
  } else {
    return(NULL)
  }
}

#-------------------------------------------------------------------------------
#' @name clean_llm_response
#'
#' @title Clean and Escape LLM Markdown Responses
#'
#' @description
#' This function strips markdown code fences (e.g., ```cpp, ```mrgsolve) and 
#' any text appearing before or after the code block. It also escapes 
#' backslashes and quotes to ensure the string can be safely wrapped in 
#' double quotes for downstream processing.
#'
#' @param final_answer A character string containing the raw LLM response.
#'
#' @return A character string stripped of markdown wrappers, with internal 
#'   quotes/backslashes escaped, and padded with a single newline at the 
#'   start and end.
#' @importFrom stringr str_replace_all str_replace fixed
#' @export
#------------------------------------------------------------------------------- 

clean_llm_response <- function(final_answer) {
  
  orig_answer <- final_answer
  
  # 1. Clean the Markdown (Keep these as regex)
  # Removes everything including and before the first code fence (```), and then
  # remove everything including and after the last code fence
  final_answer <- stringr::str_replace(final_answer, "(?s)^.*?```[a-zA-Z]*\\s*\\n?", "")
  final_answer <- stringr::str_replace(final_answer, "(?s)\\n?\\s*```[\\s\\S]*$", "")

  
  # 2. Perform all escapes in one pass using a named vector
  # Uses fixed() for maximum performance on large strings
  replaces <- c("\\\\" = "\\\\",  
                "\"" = "\\\"", 
                "'" = "\\'")
  
  final_answer <- stringr::str_replace_all(final_answer, stringr::fixed(replaces))
  
  # 3. Final Polish: Trim whitespace and add intentional newlines
  final_answer <- paste0("\n", trimws(final_answer), "\n")
  
  if(final_answer == "\n\n") {
    return(orig_answer) # Failsafe, return original answer if it's empty after cleaning
  } else {
    return(final_answer)    
  }
} 

#-------------------------------------------------------------------------------
#' @name extract_file_content
#'
#' @title Extract text content from a local file (pdf or text) and append to a prompt
#'
#' @description
#' Reads a local PDF or plain text file and appends the extracted content to the
#' provided instruction prompt, wrapped in delimiters. For PDFs, text is
#' extracted using \code{pdftools::pdf_text()}. For plain text files (including
#' \code{.txt}, \code{.mod}, and \code{.ctl}), lines are read with
#' \code{readLines()}. The resulting prompt is returned and intended to be passed
#' directly as the user message to the LLM.
#'
#' @param file_path Path to the local file to extract text from
#' @param file_name Original filename including extension, used for debug
#'   messaging only
#' @param detected_type MIME type of the file. Use \code{"application/pdf"} for
#'   PDF files and \code{"text/plain"} for all plain text formats
#' @param instruction_prompt Base instruction prompt string to which the
#'   extracted file content will be appended
#' @param debug If \code{TRUE}, prints the extracted character count to the
#'   console
#'
#' @returns A single character string containing \code{instruction_prompt}
#'   followed by the extracted file content wrapped in
#'   \code{--- FILE CONTENT START ---} and \code{--- FILE CONTENT END ---}
#'   delimiters
#'
#' @importFrom pdftools pdf_text
#' @export
#-------------------------------------------------------------------------------

extract_file_content <- function(file_path,
                                 file_name,
                                 detected_type,
                                 instruction_prompt,
                                 debug = TRUE) {
  
  if(debug) print("extract_file_content: parsing file locally")
  
  extracted_text <- if(detected_type == "application/pdf") {
    paste(pdftools::pdf_text(file_path), collapse = "\n\n")
  } else {
    paste(readLines(file_path, warn = FALSE), collapse = "\n")
  }
  
  if(debug) print(paste0("Extracted text length: ", nchar(extracted_text), " characters"))
  
  return(extracted_text)
  # paste0(
  #   instruction_prompt,
  #   "\n\n--- FILE CONTENT START ---\n",
  #   extracted_text,
  #   "\n--- FILE CONTENT END ---"
  # )
}

#-------------------------------------------------------------------------------
#' @name run_dify_chat
#'
#' @title Send a chat request to a Dify Workflow via httr2 (BI-only)
#'
#' @description
#' Handles the full Dify API interaction: optionally uploads a file first,
#' then sends the instruction prompt as a blocking workflow request and returns
#' the answer along with token usage metadata.
#'
#' @param instruction_prompt Full prompt string to send as the query, including
#'   any extracted file content if parsing locally
#' @param api_key Dify API key
#' @param api_upload API URL for the Dify file upload endpoint. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param api_chat API URL for the Dify chat/workflow endpoint
#' @param user_id User identifier string passed to the Dify API
#' @param file_path Path to the file on disk. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param detected_type MIME type of the file (e.g. \code{"application/pdf"}).
#'   Only used when \code{locally_parse_file = FALSE}
#' @param locally_parse_file If \code{TRUE}, skips file upload and sends text
#'   content directly in the prompt
#' @param conversation_id Default NULL, required for context reuse
#' @param deep_pdfscan Default FALSE, uses Vision to extract images
#' @param force_parse Default FALSE, forces reparsing
#' @param model_name Model name string used as a fallback label in usage_info
#'   if the API does not return a model identifier
#' @param debug If \code{TRUE}, prints progress messages to the console
#'
#' @returns A named list with the following elements:
#' \describe{
#'   \item{answer}{Character string containing the raw model response}
#'   \item{conversation_id}{Dify conversation ID string for the request}
#'   \item{usage_info}{Named list with \code{input}, \code{output}, \code{total}
#'     token counts and \code{model} label}
#' }
#' @export
#-------------------------------------------------------------------------------

run_dify_chat <- function(instruction_prompt,
                          api_key,
                          api_upload         = NULL,
                          api_chat,
                          user_id,
                          file_path          = NULL, 
                          detected_type      = NULL,
                          locally_parse_file = TRUE,
                          conversation_id    = NULL,   # NEW: for context reuse
                          deep_pdfscan       = FALSE,
                          force_parse        = FALSE,
                          model_name,
                          debug = TRUE) {
  
  chat_body <- if(locally_parse_file) {
    
    if(debug) print("run_dify_chat: locally_parse_file == TRUE")
    
    if(!is.null(conversation_id)) {
      
      if(debug) print("run_dify_chat: conversation_id is not NULL")
      
      list(
        query = instruction_prompt,
        response_mode = "blocking",
        user = user_id,
        conversation_id = conversation_id, # USE CONTEXT HERE
        inputs = setNames(list(), character(0)) # Force an empty dictionary {} instead of an empty array []
      )
    } else {
      list(
        query = instruction_prompt,
        response_mode = "blocking",
        user = user_id,
        inputs = setNames(list(), character(0)) # Force an empty dictionary {} instead of an empty array []
      )
    }
  } else {
    upload_data <- httr2::request(api_upload) %>%
      httr2::req_headers(Authorization = paste("Bearer", api_key)) %>%
      httr2::req_body_multipart(
        user = user_id,
        file = curl::form_file(file_path, type = detected_type)
      ) %>%
      httr2::req_retry(max_tries = 3) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    
    if(debug) print("run_dify_chat: upload_data formed")
    
    list(
      inputs = list(
        deep_pdfscan = tolower(as.character(deep_pdfscan)),
        force_parse = tolower(as.character(force_parse)),
        fileInput = list(list(
          transfer_method = "local_file",
          upload_file_id  = upload_data$id,
          type            = "document"
        ))
      ),
      query         = instruction_prompt,
      response_mode = "blocking",
      user          = user_id,
      files         = list(list(
        type            = "document",
        transfer_method = "local_file",
        upload_file_id  = upload_data$id
      ))
    )
  }
  
  if(debug) print("run_dify_chat: sending request")
  
  chat_resp <- httr2::request(api_chat) %>%
    httr2::req_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) %>%
    httr2::req_body_json(chat_body) %>%
    httr2::req_retry(max_tries = 2) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  
  if(debug) dify_chat_response <<- chat_resp
  
  list(
    answer          = chat_resp$answer,
    conversation_id = chat_resp$conversation_id,
    usage_info      = list(
      input  = chat_resp$metadata$usage$prompt_tokens    %||% "N/A",
      output = chat_resp$metadata$usage$completion_tokens %||% "N/A",
      total  = chat_resp$metadata$usage$total_tokens      %||% "N/A",
      model  = chat_resp$metadata$usage$model             %||% model_name
    )
  )
}

#-------------------------------------------------------------------------------
#' @name run_ellmer_chat
#'
#' @title Send a chat request via an ellmer provider
#'
#' @description
#' Initialises the appropriate ellmer chat object for the given service and
#' dispatches the prompt, either as text-only or alongside an uploaded file.
#' Supports Gemini, OpenAI, Claude, OpenRouter, and OpenAI-compatible endpoints.
#'
#' @param service Provider to use. One of \code{"Gemini"}, \code{"OpenAI"},
#'   \code{"Claude"}, \code{"OpenRouter"}, \code{"OpenAI-Compatible"}, \code{"DeepSeek"},
#'    \code{"Apollo"}, \code{"Azure OpenAI"}, \code{"AWS Bedrock"}
#' @param model_name Model identifier string passed to the ellmer chat constructor
#' @param system_prompt System prompt string defining model behaviour and output
#'   format constraints
#' @param optimal_params An \code{ellmer::params()} object controlling
#'   temperature, seed, and other supported sampling parameters
#' @param api_chat Base URL for the chat endpoint. Only required when
#'   \code{service = "openai_compatible"}
#' @param instruction_prompt Full user prompt string, including any extracted
#'   file content if parsing locally
#' @param file_path Path to the file on disk. Only used when
#'   \code{locally_parse_file = FALSE}
#' @param detected_type MIME type of the file (e.g. \code{"application/pdf"}).
#'   Only used when \code{locally_parse_file = FALSE}
#' @param locally_parse_file If \code{TRUE}, sends text content in the prompt
#'   directly rather than uploading the file to the provider
#' @param chat_obj the ellmer chat object for context re-use, default NULL
#' @param internal_version Logical. Changes base_url path, Only relevant for BI
#' @param debug If \code{TRUE}, prints progress messages and the chat object
#'   summary to the console
#'
#' @returns A named list with the following elements:
#' \describe{
#'   \item{answer}{Character string containing the raw model response}
#'   \item{chat_obj}{The ellmer chat object, which can be used for token
#'     inspection or follow-up turns}
#'   \item{usage_info}{Named list with \code{input}, \code{output}, \code{total}
#'     token counts and \code{model} label}
#' }
#'
#' @export
#-------------------------------------------------------------------------------
run_ellmer_chat <- function(service,
                            model_name,
                            system_prompt,
                            optimal_params,
                            api_chat            = NULL,
                            instruction_prompt,
                            file_path           = NULL,
                            detected_type       = NULL,
                            locally_parse_file  = TRUE,
                            chat_obj            = NULL,   # NEW: for context reuse
                            internal_version    = TRUE,
                            debug = TRUE) {
  
  apollo_url <- ifelse(internal_version, Sys.getenv("APOLLO_BASE_URL"), Sys.getenv("APOLLO_BASE_URL_EXT"))
  
  # Reuse existing chat object if provided, otherwise create a new one
  chat_obj <- if(!is.null(chat_obj)) {
    chat_obj
  } else {
    switch(service,
           "Gemini"            = ellmer::chat_google_gemini(model = model_name, system = system_prompt, params = optimal_params),
           "OpenAI"            = ellmer::chat_openai(model = model_name, system = system_prompt, params = optimal_params),
           "Claude"            = if(!locally_parse_file) {
             ellmer::chat_claude(model = model_name, system = system_prompt, params = optimal_params,
                                 beta_headers = "files-api-2025-04-14")
           } else {
             ellmer::chat_claude(model = model_name, system = system_prompt, params = optimal_params)
           },
           "OpenRouter"        = ellmer::chat_openrouter(model = model_name, system = system_prompt, params = optimal_params),
           "OpenAI-Compatible" = ellmer::chat_openai_compatible(base_url = api_chat, model = model_name, system = system_prompt, params = optimal_params),
           "DeepSeek"          = ellmer::chat_deepseek(model = model_name, system = system_prompt, params = optimal_params),
           "Apollo"            = if(internal_version) {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)
           } else {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token_ext, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)             
           },
           "Test Provider"      = if(internal_version) {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)
           } else {
             ellmer::chat_openai_compatible(base_url = paste0(apollo_url, "/llm-api"),
                                            credentials = get_apollo_token_ext, # zero-argument function
                                            model = model_name, system = system_prompt, params = optimal_params)             
           },
           "Azure OpenAI"      = ellmer::chat_azure_openai(model = model_name, system = system_prompt, params = optimal_params),
           "AWS Bedrock"       = ellmer::chat_aws_bedrock(model = model_name, system = system_prompt, params = optimal_params)
    )
  }
  
  final_answer <- if(locally_parse_file) {
    if(debug) print(paste0("run_ellmer_chat: ", service, " text-only"))
    chat_obj$chat(instruction_prompt, echo = FALSE)
    
  } else {
    if(debug) print(paste0("run_ellmer_chat: ", service, " with file upload"))
    
    upload_file <- switch(service,
                          "Gemini"            = ellmer::google_upload(path = file_path, mime_type = detected_type),
                          "Claude"            = ellmer::claude_file_upload(path = file_path, beta_headers = "files-api-2025-04-14"),
                          "OpenAI"            = ellmer::content_pdf_file(file_path),
                          "OpenRouter"        = ellmer::content_pdf_file(file_path),
                          "OpenAI-Compatible" = ellmer::content_pdf_file(file_path),
                          "DeepSeek"          = ellmer::content_pdf_file(file_path),  # DeepSeek does not support file upload, the logic to guard against this happens in translate_model_code
                          "Apollo"            = ellmer::content_pdf_file(file_path),  # Does not support file upload
                          "Test Provider"     = ellmer::content_pdf_file(file_path),  # Does not support file upload
                          "Azure OpenAI"      = ellmer::content_pdf_file(file_path),
                          "AWS Bedrock"       = ellmer::content_pdf_file(file_path),
    )
    
    if(debug && service %in% c("Gemini", "Claude")) {
      print(paste0("run_ellmer_chat: upload_file - ", upload_file@uri, ", type: ", upload_file@mime_type))
    }
    chat_obj$chat(instruction_prompt, upload_file, echo = FALSE)
  }
  
  if(debug) { print("run_ellmer_chat: chat_obj:"); print(chat_obj) }
  
  if(debug) ellmer_chat_response <<- final_answer
  
  usage_df <- chat_obj$get_tokens()
  
  list(
    answer     = final_answer,
    chat_obj   = chat_obj,
    usage_info = list(
      input  = sum(usage_df$input),
      output = sum(usage_df$output),
      total  = sum(usage_df$input) + sum(usage_df$output),
      model  = chat_obj$get_model()
    )
  )
}

#' @name prepare_uploaded_files
#' 
#' @title Validate and Prepare Uploaded Files for LLM Processing
#'
#' @description
#' Validates one or more files from a Shiny \code{\link[shiny]{fileInput}}
#' widget against allowed single- and multi-file extension lists, then either
#' returns the single file path directly or combines multiple files into one
#' via \code{\link{combine_uploaded_files}}. On validation failure, a Shiny
#' error notification is shown and \code{NULL} is returned.
#'
#' @param files A data frame produced by a Shiny \code{fileInput} widget,
#'   containing at minimum the columns \code{name} (original filename) and
#'   \code{datapath} (server-side temporary path).
#' @param llm_service LLM provider name. More extensions are accepted by Dify-workflows.
#' @param model_lang Character. "mrgsolve", "nonmem", "rxode2"
#' @param use_nonmem2mrgsolve Logical. Whether to use nonmem2mrgsolve when translating to mrgsolve, if package is present
#' @param use_nonmem2rx Logical. Whether to use nonmem2rx when translating to rxode2, if package is present
#' @param single_file_types Character vector of permitted file extensions when
#'   exactly one file is uploaded, e.g. \code{c(".pdf", ".docx", ".csv")}.
#'   Extensions should include the leading dot.
#' @param multi_file_types Character vector of permitted file extensions when
#'   more than one file is uploaded, e.g. \code{c(".pdf", ".txt", ".mod",
#'   ".ctl")}. Must be a subset of \code{single_file_types}. Extensions should
#'   include the leading dot.
#'
#' @return A length-one character string giving the path to a ready-to-use
#'   file, or \code{NULL} if validation failed (in which case a Shiny error
#'   notification has already been shown to the user). For single-file uploads
#'   this is \code{files$datapath[1]}; for multi-file uploads this is the
#'   temporary combined file produced by \code{\link{combine_uploaded_files}}.
#'   Returns a list if either criteria for nonmem2mrgsolve or nonmem2rx has been met.
#'
#' @details
#' Extensions \code{.ctl} and \code{.mod} are treated as equivalent to
#' \code{.txt} when checking for mixed-type uploads, since all three are
#' plain-text formats commonly used in pharmacometric workflows. This
#' normalisation only affects the mixed-type guard; the original extensions
#' are preserved in any user-facing notification messages.
#'
#' Validation is performed in this order:
#' \enumerate{
#'   \item For a single file: check that its extension is in
#'     \code{single_file_types}.
#'   \item For multiple files: check that every extension is in
#'     \code{multi_file_types}.
#'   \item For multiple files: check that all files share the same normalised
#'     extension (i.e. no mixing of e.g. \code{.pdf} and \code{.txt}).
#'   \item For multiple files: delegate to
#'     \code{\link{combine_uploaded_files}}.
#' }
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny server function:
#' observe({
#'   req(input$pdffile_model_1)
#'
#'   ready_path <- prepare_uploaded_files(
#'     files             = input$pdffile_model_1,
#'     single_file_types = llm_accept_single_types,
#'     multi_file_types  = llm_accept_multi_types
#'   )
#'
#'   req(!is.null(ready_path))
#'   # pass ready_path to translate_model_code() or send_to_llm()
#' })
#' }
#'
#' @seealso
#' \code{\link{combine_uploaded_files}} for the multi-file combining logic,
#' \code{\link[shiny]{fileInput}} for the UI element that produces \code{files},
#' \code{\link[shiny]{showNotification}} for the notification system.
#'
#' @importFrom shiny showNotification req
#' @importFrom tools file_ext
#'
#' @export
prepare_uploaded_files <- function(files,
                                   llm_service,
                                   model_lang,
                                   use_nonmem2mrgsolve,
                                   use_nonmem2rx,
                                   single_file_types,
                                   multi_file_types) {
  
  # Determine accepted file types based on llm choice
  accept_types <- if (llm_service %in% c("PMx Co-Modeler", "EXP")) {
    single_file_types  # Dify supports text too
  } else {
    multi_file_types  # default for other providers (PDF or text-equivalent)
  }
  
  if(model_lang == "mrgsolve" && use_nonmem2mrgsolve && requireNamespace("nonmem2mrgsolve", quietly = TRUE)) {
    accept_types <- c(accept_types, ".ext")
  }
  
  if(model_lang == "rxode2" && use_nonmem2rx && requireNamespace("nonmem2rx", quietly = TRUE)) {
    accept_types <- c(accept_types, ".lst")
  }
  
  # ── Helper: normalise interchangeable plain-text extensions ──────────────────
  normalize_ext <- function(ext) {
    ifelse(ext %in% c(".ctl", ".mod", ".cpp", ".lst"), ".txt", ext)
  }
  
  exts            <- tolower(paste0(".", tools::file_ext(files$name)))
  normalized_exts <- normalize_ext(exts)
  
  # ── Single file ───────────────────────────────────────────────────────────────
  if (nrow(files) == 1) {
    
    if (!exts %in% single_file_types) {
      safely_showNotification(
        paste0("Unsupported file type: '", exts, "'. ",
               "Supported types are: ",
               paste(accept_types, collapse = ", ")),
        type     = "error",
        duration = NULL
      )
      return(invisible(NULL))
    }
    
    # ── nonmem2rx path: single .ctl/.mod/.lst when targeting rxode2 ─────────────
    if (model_lang == "rxode2" && use_nonmem2rx && requireNamespace("nonmem2rx", quietly = TRUE) &&
        exts %in% c(".ctl", ".mod", ".lst")) {
      
      return(list(
        path = files$datapath[1],
        ext  = exts,
        name = files$name[1]
      ))
    }
    
    return(files$datapath[1])
  }
  
  # ── NONMEM pair: one .ctl/.mod + one .ext → list for nonmem2mrgsolve ─────────
  # Must be checked before the general multi-file logic so .ext doesn't get
  # rejected by the multi_file_types guard.
  if (nrow(files) == 2 && model_lang == "mrgsolve" && use_nonmem2mrgsolve && requireNamespace("nonmem2mrgsolve", quietly = TRUE)) {
    
    is_model_ext <- exts %in% c(".ctl", ".mod")
    is_ext_ext   <- exts == ".ext"
    
    if (sum(is_model_ext) == 1 && sum(is_ext_ext) == 1) {
      
      model_path <- files$datapath[is_model_ext]
      ext_path   <- files$datapath[is_ext_ext]
      
      return(list(
        model = model_path,
        ext   = ext_path
      ))
    }
  }
  
  # ── Multiple files ────────────────────────────────────────────────────────────
  
  # Guard: all extensions must be in the multi-file supported list
  unsupported <- exts[!exts %in% multi_file_types]
  if (length(unsupported) > 0) {
    safely_showNotification(
      paste0("When uploading multiple files, only ",
             paste(multi_file_types, collapse = ", "),
             " are supported. Unsupported file(s): ",
             paste(unique(unsupported), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # Guard: all files must share the same normalised extension
  if (length(unique(normalized_exts)) > 1) {
    safely_showNotification(
      paste0("All files must be the same type when uploading multiple files. ",
             "Found: ", paste(unique(exts), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # Combine and return
  combine_uploaded_files(files$datapath, files$name)
}


#' @name combine_uploaded_files
#' 
#' @title Combine Multiple Uploaded Files into a Single File
#'
#' @description
#' Merges a list of locally stored files (from a Shiny \code{fileInput} with
#' \code{multiple = TRUE}) into a single temporary file, ready for downstream
#' processing such as sending to an LLM. Supports PDF (merged page-by-page)
#' and plain text types including \code{.txt}, \code{.mod}, \code{.ctl}, and \code{.cpp}
#' (concatenated). On error, a Shiny notification is shown to the user and
#' \code{NULL} is returned rather than stopping the session.
#'
#' @param file_paths Character vector of temporary file paths, drawn from
#'   \code{input$<inputId>$datapath} after a Shiny \code{fileInput} upload.
#' @param file_names Character vector of original file names, drawn from
#'   \code{input$<inputId>$name}. Used to determine file type via extension,
#'   since browser-reported MIME types are unreliable for types such as
#'   \code{.mod} and \code{.ctl}.
#'
#' @return A length-one character string: the path to a temporary file
#'   containing the combined content, or \code{NULL} if an error occurred (in
#'   which case a Shiny notification is shown). The file lives in
#'   \code{tempdir()} and will be cleaned up at the end of the R session.
#'
#' @details
#' File type is determined from the extension of \code{file_names} rather than
#' the browser-reported MIME type, as browsers often report generic
#' \code{"application/octet-stream"} for domain-specific extensions such as
#' \code{.ctl} and \code{.mod}.
#'
#' This function assumes all files share the same extension. If they do not,
#' a Shiny error notification is shown and \code{NULL} is returned. It is
#' expected that this validation is performed by the caller (see the Shiny
#' server example) before calling this function.
#'
#' Combination strategy by extension:
#' \describe{
#'   \item{\code{.pdf}}{Pages are merged in order using
#'     \code{\link[pdftools]{pdf_combine}}. Requires the \pkg{pdftools} package.}
#'   \item{\code{.txt}, \code{.mod}, \code{.ctl}}{Files are read with
#'     \code{\link{readLines}} and concatenated with a newline separator.
#'     Output is written as \code{.txt}.}
#'   \item{All other extensions}{A Shiny error notification is shown and
#'     \code{NULL} is returned.}
#' }
#'
#' @examples
#' \dontrun{
#' # Inside a Shiny server function:
#' observe({
#'   req(input$pdffile_model_1)
#'   files <- input$pdffile_model_1
#'
#'   combined_path <- if (nrow(files) > 1) {
#'     combine_uploaded_files(files$datapath, files$name)
#'   } else {
#'     files$datapath[1]
#'   }
#'
#'   req(!is.null(combined_path))
#'   send_to_llm(combined_path)
#' })
#' }
#'
#' @seealso
#' \code{\link[shiny]{fileInput}} for the UI element that produces the input,
#' \code{\link[shiny]{showNotification}} for the notification system,
#' \code{\link[pdftools]{pdf_combine}} for the underlying PDF merge.
#'
#' @importFrom pdftools pdf_combine
#' @importFrom shiny showNotification
#'
#' @export
combine_uploaded_files <- function(file_paths, file_names) {
  
  # ── Validate inputs ──────────────────────────────────────────────────────────
  if (length(file_paths) != length(file_names)) {
    shiny::showNotification(
      "Internal error: file_paths and file_names must be the same length.",
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  # ── Resolve extensions from original file names ──────────────────────────────
  exts <- tolower(paste0(".", tools::file_ext(file_names)))
  
  # Normalize interchangeable plain-text extensions before uniqueness check
  normalize_ext <- function(ext) {
    ifelse(ext %in% c(".ctl", ".mod", ".cpp", ".lst"), ".txt", ext)
  }
  normalized_exts <- normalize_ext(exts)
  
  if (length(unique(normalized_exts)) > 1) {
    shiny::showNotification(
      paste0("All uploaded files must share the same extension. ",
             "Found: ", paste(unique(exts), collapse = ", ")),
      type     = "error",
      duration = NULL
    )
    return(invisible(NULL))
  }
  
  ext <- normalized_exts[1]
  
  # ── PDF ──────────────────────────────────────────────────────────────────────
  if (ext == ".pdf") {
    output_path <- tempfile(fileext = ".pdf")
    pdftools::pdf_combine(file_paths, output = output_path)
    return(output_path)
  }
  
  # ── Plain text: .txt, .mod, .ctl, .cpp ──────────────────────────────────────
  if (ext %in% c(".txt", ".mod", ".ctl", ".cpp", ".lst")) {
    output_path <- tempfile(fileext = ".txt")
    combined_text <- sapply(file_paths, readLines, warn = FALSE) %>%
      unlist() %>%
      paste(collapse = "\n")
    writeLines(combined_text, output_path)
    return(output_path)
  }
  
  # ── Fallback: unsupported type ───────────────────────────────────────────────
  shiny::showNotification(
    paste0("Unsupported file type for multi-file combining: '", ext, "'. ",
           "Supported types are: .pdf, .txt, .mod, .ctl, .cpp"),
    type     = "error",
    duration = NULL
  )
  return(invisible(NULL))
}


#' @name get_api_key
#' 
#' @title Retrieves the stored API key from a provider in .Renviron File
#'
#' @description
#' Retrieves the API key for a specified LLM service from environment variables.
#' Set environment variables with \code{usethis::edit_r_environ()}.
#'
#' @param llm_service Name of LLM provider
#' 
#' @return A character string containing the API key, or \code{""} if the
#'   environment variable is not set.
#'
#' @export
get_api_key <- function(llm_service) {
  switch(llm_service, # edit with usethis::edit_r_environ()
         "PROD"              = Sys.getenv("DIFY_API_KEY"),
         "PMx Co-Modeler"    = Sys.getenv("DIFY_API_KEY"),
         "EXP"               = Sys.getenv("DIFY_API_KEY_2"),
         "Fast"              = Sys.getenv("DIFY_API_KEY_2"),
         "Claude"            = Sys.getenv("ANTHROPIC_API_KEY"),
         "Gemini"            = Sys.getenv("GEMINI_API_KEY"),
         "OpenAI"            = Sys.getenv("OPENAI_API_KEY"),
         "OpenRouter"        = Sys.getenv("OPENROUTER_API_KEY"),
         "OpenAI-Compatible" = Sys.getenv("OPENAI_COMPATIBLE_API_KEY"),
         "DeepSeek"          = Sys.getenv("DEEPSEEK_API_KEY"),
         "Apollo"            = Sys.getenv("APOLLO_CLIENT_SECRET"),
         "Test Provider"     = Sys.getenv("APOLLO_CLIENT_SECRET"),
         "Azure OpenAI"      = Sys.getenv("AZURE_OPENAI_API_KEY"),
         "AWS Bedrock"       = "dummy"
  )
}

#' @name get_api_key_name
#' 
#' @title Returns the designated API key name from a provider in .Renviron File
#'
#' @description
#' Retrieves the name of the API key for a specified LLM service from environment variables.
#' Set environment variables with \code{usethis::edit_r_environ()}.
#'
#' @param llm_service Name of LLM provider
#' 
#' @return A character string containing the API key name
#'
#' @export
get_api_key_name <- function(llm_service) {
  switch(llm_service,
         "PROD"              = "DIFY_API_KEY",
         "PMx Co-Modeler"    = "DIFY_API_KEY",
         "EXP"               = "DIFY_API_KEY_2",
         "Fast"              = "DIFY_API_KEY_2",
         "Claude"            = "ANTHROPIC_API_KEY",
         "Gemini"            = "GEMINI_API_KEY",
         "OpenAI"            = "OPENAI_API_KEY",
         "OpenRouter"        = "OPENROUTER_API_KEY",
         "OpenAI-Compatible" = "OPENAI_COMPATIBLE_API_KEY",
         "DeepSeek"          = "DEEPSEEK_API_KEY",
         "Apollo"            = "APOLLO_CLIENT_SECRET", # Also needs APOLLO_CLIENT_ID, APOLLO_TOKEN_URL, APOLLO_BASE_URL
         "Test Provider"     = "APOLLO_CLIENT_SECRET", # Also needs APOLLO_CLIENT_ID, APOLLO_TOKEN_URL, APOLLO_BASE_URL
         "Azure OpenAI"      = "AZURE_OPENAI_API_KEY",
         "AWS Bedrock"       = "dummy"
  )
}

#' Get Apollo OAuth Token
#'
#' Requests a short-lived OAuth access token from the Axway gateway using
#' client credentials. Requires \code{APOLLO_CLIENT_ID}, \code{APOLLO_CLIENT_SECRET},
#' and \code{APOLLO_TOKEN_URL} to be set in \code{.Renviron}.
#' Use \code{usethis::edit_r_environ()} to set them. Only relevant for BI.

#'
#' @return A character string containing the Bearer access token.
#'
#' @examples
#' \dontrun{
#' token <- get_apollo_token()
#' }
#' 
#' @importFrom httr2 request req_body_form req_perform resp_body_json
#' 
#' @export
get_apollo_token <- function() {

  response <- httr2::request(Sys.getenv("APOLLO_TOKEN_URL")) %>%
    httr2::req_body_form(
      client_id     = Sys.getenv("APOLLO_CLIENT_ID"),
      client_secret = Sys.getenv("APOLLO_CLIENT_SECRET"),
      grant_type    = "client_credentials"
    ) %>%
    httr2::req_perform()
  
  httr2::resp_body_json(response)$access_token
}

#' Get Apollo OAuth Token (External)
#'
#' Requests a short-lived OAuth access token from the Axway gateway using
#' client credentials. Requires \code{APOLLO_CLIENT_ID}, \code{APOLLO_CLIENT_SECRET},
#' and \code{APOLLO_TOKEN_URL} to be set in \code{.Renviron}.
#' Use \code{usethis::edit_r_environ()} to set them. Only relevant for BI.

#'
#' @return A character string containing the Bearer access token.
#'
#' @examples
#' \dontrun{
#' token <- get_apollo_token()
#' }
#' 
#' @importFrom httr2 request req_body_form req_perform resp_body_json
#' 
#' @export
get_apollo_token_ext <- function() {
  
  response <- httr2::request(Sys.getenv("APOLLO_TOKEN_URL_EXT")) %>%
    httr2::req_body_form(
      client_id     = Sys.getenv("APOLLO_CLIENT_ID"),
      client_secret = Sys.getenv("APOLLO_CLIENT_SECRET"),
      grant_type    = "client_credentials"
    ) %>%
    httr2::req_perform()
  
  httr2::resp_body_json(response)$access_token
}

#' Get current MVP session state
#'
#' Captures all current UI inputs and reactive values into a serializable list
#' for saving via \code{saveRDS()}. Complementary to
#' \code{\link{restore_session_state}}.
#'
#' @param input The Shiny \code{input} object from the current server session.
#' @param rv A \code{\link[shiny]{reactiveValues}} object containing
#'   server-side state not bound to UI inputs.
#' @param uploaded_data The \code{uploaded_data} reactive. Wrapped in
#'   \code{tryCatch} so \code{NULL} is stored gracefully if no file is loaded.
#'
#' @return A named list containing \code{version}, \code{saved_at}, and
#'   sublists \code{model}, \code{data}, \code{dosing}, \code{sim},
#'   \code{psa}, and \code{variability}.
#'
#' @seealso \code{\link{restore_session_state}}
#'
#' @export
get_session_state <- function(input, rv, uploaded_data) {
  list(
    version = "0.4.1",  # tag with app version for forward-compat checks
    saved_at = Sys.time(),
    
    # --- Model code & settings ---
    model = list(
      model_input     = input$model_input,
      model_input2    = input$model_input2,
      model_select    = input$model_select,
      model_select2   = input$model_select2
    ),
    
    # --- Data ---
    data = list(
      uploaded_data      = tryCatch(uploaded_data(), error = function(e) NULL),
      create_cmt_col     = input$create_cmt_col,
      create_id_col      = input$create_id_col,
      create_time_col    = input$create_time_col,
      BLQ_filter         = input$BLQ_filter,
      EVID_filter        = input$EVID_filter,
      distinct_by_ID     = input$distinct_by_ID,
      turn_all_numeric   = input$turn_all_numeric,
      column             = input$column,
      codes              = input$codes,
      enableAutocomplete = input$enableAutocomplete,
      ########################################
      transpose_data_info= input$transpose_data_info,
      subject_colname    = input$subject_colname,
      additional_keys    = input$additional_keys,
      time_colname       = input$time_colname,
      desc_time_unit     = input$desc_time_unit,
      conc_colname       = input$conc_colname,
      desc_conc_unit     = input$desc_conc_unit,
      dose_colname       = input$dose_colname,
      desc_dose_unit     = input$desc_dose_unit,
      adm_route          = input$adm_route,
      dur_inf            = input$dur_inf,
      down_method        = input$down_method,
      mw_value           = input$mw_value,
      nca_sigfigs        = input$nca_sigfigs,
      transpose_nca      = input$transpose_nca,
      ########################################
      y_axis             = input$y_axis,
      x_axis             = input$x_axis,
      color              = input$color,
      median_line_by     = input$median_line_by,
      facet_by           = input$facet_by,
      filter_cmt_data    = input$filter_cmt_data,
      log_y_axis_data    = input$log_y_axis_data,
      log_x_axis_data    = input$log_x_axis_data,
      insert_smoother    = input$insert_smoother,
      median_line_data   = input$median_line_data,
      insert_lm_eqn      = input$insert_lm_eqn,
      do_boxplot         = input$do_boxplot, # updatePrettySwitch
      plot_title_data    = input$plot_title_data,
      select_label_size  = input$select_label_size,
      quantize_x         = input$quantize_x,
      do_data_plotly     = input$do_data_plotly,
      ########################################
      filter_by_id       = input$filter_by_id,
      page_number        = input$page_number,
      number_of_rows     = input$number_of_rows,
      number_of_cols     = input$number_of_cols,
      highlight_var      = input$highlight_var,
      highlight_var_values=input$highlight_var_values,
      lloq_colname       = input$lloq_colname,
      ind_dose_colname   = input$ind_dose_colname,
      dose_units         = input$dose_units,
      insert_dosing      = input$insert_dosing,
      fixed_scale        = input$fixed_scale,
      do_data_ind_plotly = input$do_data_ind_plotly,
      plot_title_ind     = input$plot_title_ind,
      sort_by_ind        = input$sort_by_ind,
      strat_by_ind       = input$strat_by_ind,
      outlier_threshold  = input$outlier_threshold,
      ########################################
      var_corr           = input$var_corr,
      color_corr         = input$color_corr,
      ########################################
      var_hist           = input$var_hist,
      bin_size           = input$bin_size
    ),
    
    # --- Dosing ---
    dosing = list(
      cmt1_model_1       = input$cmt1_model_1,
      amt1               = input$amt1,
      delay_time1        = input$delay_time1,
      total1             = input$total1,
      ii1                = input$ii1,
      tinf1              = input$tinf1,
      ########################################
      cmt2_model_1       = input$cmt2_model_1,
      amt2               = input$amt2,
      delay_time2        = input$delay_time2,
      total2             = input$total2,
      ii2                = input$ii2,
      tinf2              = input$tinf2,
      ########################################
      cmt3_model_1       = input$cmt3_model_1,
      amt3               = input$amt3,
      delay_time3        = input$delay_time3,
      total3             = input$total3,
      ii3                = input$ii3,
      tinf3              = input$tinf3,   
      ########################################
      cmt4_model_1       = input$cmt4_model_1,
      amt4               = input$amt4,
      delay_time4        = input$delay_time4,
      total4             = input$total4,
      ii4                = input$ii4,
      tinf4              = input$tinf4,  
      ########################################
      cmt5_model_1       = input$cmt5_model_1,
      amt5               = input$amt5,
      delay_time5        = input$delay_time5,
      total5             = input$total5,
      ii5                = input$ii5,
      tinf5              = input$tinf5,
      ########################################
      mw_checkbox        = input$mw_checkbox,
      mw                 = input$mw,
      multi_factor       = input$multi_factor,
      wt_based_dosing_checkbox = input$wt_based_dosing_checkbox,
      wt_based_dosing_name     = input$wt_based_dosing_name,
      model_dur_checkbox       = input$model_dur_checkbox,
      model_rate_checkbox      = input$model_rate_checkbox,
      ########################################
      ########################################
      cmt1_model_2       = input$cmt1_model_2,
      amt1_2               = input$amt1_2,
      delay_time1_2        = input$delay_time1_2,
      total1_2             = input$total1_2,
      ii1_2                = input$ii1_2,
      tinf1_2              = input$tinf1_2,
      ########################################
      cmt2_model_2       = input$cmt2_model_2,
      amt2_2               = input$amt2_2,
      delay_time2_2        = input$delay_time2_2,
      total2_2             = input$total2_2,
      ii2_2                = input$ii2_2,
      tinf2_2              = input$tinf2_2,
      ########################################
      cmt3_model_2       = input$cmt3_model_2,
      amt3_2               = input$amt3_2,
      delay_time3_2        = input$delay_time3_2,
      total3_2             = input$total3_2,
      ii3_2                = input$ii3_2,
      tinf3_2              = input$tinf3_2,   
      ########################################
      cmt4_model_2       = input$cmt4_model_2,
      amt4_2               = input$amt4_2,
      delay_time4_2        = input$delay_time4_2,
      total4_2             = input$total4_2,
      ii4_2                = input$ii4_2,
      tinf4_2              = input$tinf4_2,  
      ########################################
      cmt5_model_2       = input$cmt5_model_2,
      amt5_2               = input$amt5_2,
      delay_time5_2        = input$delay_time5_2,
      total5_2             = input$total5_2,
      ii5_2                = input$ii5_2,
      tinf5_2              = input$tinf5_2,
      ########################################
      mw_checkbox_2        = input$mw_checkbox_2,
      mw_2                 = input$mw_2,
      multi_factor_2       = input$multi_factor_2,
      wt_based_dosing_checkbox_2 = input$wt_based_dosing_checkbox_2,
      wt_based_dosing_name_2     = input$wt_based_dosing_name_2,
      model_dur_checkbox_2       = input$model_dur_checkbox_2,
      model_rate_checkbox_2      = input$model_rate_checkbox_2
      # 
    ),
    
    # --- Simulation options ---
    sim = list(
      time_unit              = input$time_unit,
      x_axis_label           = input$x_axis_label,
      yaxis_name             = input$yaxis_name,
      yaxis_name_model_2     = input$yaxis_name_model_2,
      y_axis_label           = input$y_axis_label,
      plot_title_sim         = input$plot_title_sim,
      log_y_axis             = input$log_y_axis,
      log_x_axis             = input$log_x_axis,
      geom_point_sim_option  = input$geom_point_sim_option,
      show_model_1           = input$show_model_1,
      show_model_2           = input$show_model_2,
      do_sim_plotly          = input$do_sim_plotly,
      ##############################################
      tgrid_max              = input$tgrid_max,
      delta                  = input$delta,
      custom_sampling_time_text = input$custom_sampling_time_text,
      custom_sampling_time_cb   = input$custom_sampling_time_cb,
      add_time_zero             = input$add_time_zero,
      ##############################################
      nonmem_y_axis          = input$nonmem_y_axis,
      filter_cmt             = input$filter_cmt,
      color_data_by          = input$color_data_by,
      stat_sum_data_by       = input$stat_sum_data_by,
      combine_nmdata         = input$combine_nmdata,
      stat_sum_data_option   = input$stat_sum_data_option,
      geom_point_data_option = input$geom_point_data_option
      
    ),
    
    # --- PSA state ---
    psa = list(
      param_selector_model_1        = input$param_selector_model_1,
      digits_model_1                = input$digits_model_1,
      dp_checkbox_model_1           = input$dp_checkbox_model_1,
      min_nca_obs_time_model_1      = input$min_nca_obs_time_model_1,
      max_nca_obs_time_model_1      = input$max_nca_obs_time_model_1,      
      log_y_axis_model_1            = input$log_y_axis_model_1,
      geom_point_sim_option_model_1 = input$geom_point_sim_option_model_1,
      log_x_axis_model_1            = input$log_x_axis_model_1,      
      geom_point_data_option_model_1 = input$geom_point_data_option_model_1,
      geom_vline_option_model_1      = input$geom_vline_option_model_1,
      combine_nmdata_1_model_1       = input$combine_nmdata_1_model_1,
      geom_ribbon_option_model_1     = input$geom_ribbon_option_model_1,
      stat_sum_data_option_model_1   = input$stat_sum_data_option_model_1,
      plot_title_psa_model_1         = input$plot_title_psa_model_1,
      do_psa_plotly_model_1          = input$do_psa_plotly_model_1,
      ##############################################
      param_selector_model_2        = input$param_selector_model_2,
      digits_model_2                = input$digits_model_2,
      dp_checkbox_model_2           = input$dp_checkbox_model_2,
      min_nca_obs_time_model_2      = input$min_nca_obs_time_model_2,
      max_nca_obs_time_model_2      = input$max_nca_obs_time_model_2,   
      log_y_axis_model_2            = input$log_y_axis_model_2,
      geom_point_sim_option_model_2 = input$geom_point_sim_option_model_2,
      log_x_axis_model_2            = input$log_x_axis_model_2,      
      geom_point_data_option_model_2 = input$geom_point_data_option_model_2,
      geom_vline_option_model_2      = input$geom_vline_option_model_2,
      combine_nmdata_1_model_2       = input$combine_nmdata_1_model_2,
      geom_ribbon_option_model_2     = input$geom_ribbon_option_model_2,
      stat_sum_data_option_model_2   = input$stat_sum_data_option_model_2,
      plot_title_psa_model_2         = input$plot_title_psa_model_2,
      do_psa_plotly_model_2          = input$do_psa_plotly_model_2,
      ##############################################
      min_tor_obs_time_model_1       = input$min_tor_obs_time_model_1,
      max_tor_obs_time_model_1       = input$max_tor_obs_time_model_1, 
      tor_lower_model_1              = input$tor_lower_model_1,
      tor_upper_model_1              = input$tor_upper_model_1,
      tor_fix_model_1                = input$tor_fix_model_1,
      tor_show_digits_model_1        = input$tor_show_digits_model_1,
      tor_do_gradient_model_1        = input$tor_do_gradient_model_1,
      tor_var_model_1                = input$tor_var_model_1,
      select_tor_metric_model_1      = input$select_tor_metric_model_1,
      tor_display_as_model_1         = input$tor_display_as_model_1,
      trim_tor_model_1               = input$trim_tor_model_1,
      show_bioeq_model_1             = input$show_bioeq_model_1,
      plot_title_tor_model_1         = input$plot_title_tor_model_1,
      xlab_tor_model_1               = input$xlab_tor_model_1,
      tor_display_text_model_1       = input$tor_display_text_model_1,
      spi_normalize_model_1          = input$spi_normalize_model_1,
      do_tor_plotly_model_1          = input$do_tor_plotly_model_1,
      ##############################################
      min_tor_obs_time_model_2       = input$min_tor_obs_time_model_2,
      max_tor_obs_time_model_2       = input$max_tor_obs_time_model_2, 
      tor_lower_model_2              = input$tor_lower_model_2,
      tor_upper_model_2              = input$tor_upper_model_2,
      tor_fix_model_2                = input$tor_fix_model_2,
      tor_show_digits_model_2        = input$tor_show_digits_model_2,
      tor_do_gradient_model_2        = input$tor_do_gradient_model_2,
      tor_var_model_2                = input$tor_var_model_2,
      select_tor_metric_model_2      = input$select_tor_metric_model_2,
      tor_display_as_model_2         = input$tor_display_as_model_2,
      trim_tor_model_2               = input$trim_tor_model_2,
      show_bioeq_model_2             = input$show_bioeq_model_2,
      plot_title_tor_model_2         = input$plot_title_tor_model_2,
      xlab_tor_model_2               = input$xlab_tor_model_2,
      tor_display_text_model_2       = input$tor_display_text_model_2,
      spi_normalize_model_2          = input$spi_normalize_model_2,
      do_tor_plotly_model_2          = input$do_tor_plotly_model_2      
      
    ),
    
    # --- Variability matrices ---
    variability = list(
      db_model_1             = input$db_model_1,
      n_subj_model_1         = input$n_subj_model_1,
      seed_number_model_1    = input$seed_number_model_1,
      age_db_model_1         = input$age_db_model_1,
      wt_db_model_1          = input$wt_db_model_1,
      males_db_model_1       = input$males_db_model_1,
      bmi_db_model_1         = input$bmi_db_model_1,
      custom_cov_1_model_1   = input$custom_cov_1_model_1,
      custom_cov_1_dist_model_1 = input$custom_cov_1_dist_model_1,
      custom_cov_2_model_1   = input$custom_cov_2_model_1,
      custom_cov_2_dist_model_1 = input$custom_cov_2_dist_model_1,
      custom_cov_3_model_1   = input$custom_cov_3_model_1,
      custom_cov_3_dist_model_1 = input$custom_cov_3_dist_model_1,      
      ##############################################
      db_model_2             = input$db_model_2,
      n_subj_model_2         = input$n_subj_model_2,
      seed_number_model_2    = input$seed_number_model_2,
      age_db_model_2         = input$age_db_model_2,
      wt_db_model_2          = input$wt_db_model_2,
      males_db_model_2       = input$males_db_model_2,
      bmi_db_model_2         = input$bmi_db_model_2,
      custom_cov_1_model_2   = input$custom_cov_1_model_2,
      custom_cov_1_dist_model_2 = input$custom_cov_1_dist_model_2,
      custom_cov_2_model_2   = input$custom_cov_2_model_2,
      custom_cov_2_dist_model_2 = input$custom_cov_2_dist_model_2,
      custom_cov_3_model_2   = input$custom_cov_3_model_2,
      custom_cov_3_dist_model_2 = input$custom_cov_3_dist_model_2,
      ##############################################
      upper_quartile         = input$upper_quartile,
      lower_quartile         = input$lower_quartile,
      show_ind_profiles      = input$show_ind_profiles,
      do_iiv_plotly          = input$do_iiv_plotly,
      show_iiv_model_1       = input$show_iiv_model_1,
      log_y_axis_iiv         = input$log_y_axis_iiv,
      show_iiv_model_2       = input$show_iiv_model_2,
      log_x_axis_iiv         = input$log_x_axis_iiv,
      combine_nmdata_iiv     = input$combine_nmdata_iiv,
      stat_sum_data_option_iiv = input$stat_sum_data_option_iiv,
      geom_point_data_option_iiv = input$geom_point_data_option_iiv,
      plot_title_iiv         = input$plot_title_iiv,
      y_value_threshold      = input$y_value_threshold,
      x_value_threshold      = input$x_value_threshold,
      show_y_intercept_threshold = input$show_y_intercept_threshold,
      show_x_intercept_threshold = input$show_x_intercept_threshold,
      ##############################################
      select_exp             = input$select_exp,
      min_exp_obs_time_model = input$min_exp_obs_time_model,
      max_exp_obs_time_model = input$max_exp_obs_time_model,      
      exp_yaxis_label        = input$exp_yaxis_label,
      exp_model_1_name       = input$exp_model_1_name,
      exp_model_2_name       = input$exp_model_2_name,
      exp_show_model_1       = input$exp_show_model_1,
      exp_show_model_2       = input$exp_show_model_2,
      plot_title_exp_model   = input$plot_title_exp_model,
      exp_display_stats      = input$exp_display_stats,
      do_exp_plotly          = input$do_exp_plotly
      
    )
  )
}

#' Restore a saved MVP session state
#'
#' Restores a previously saved session state by updating all UI inputs and
#' reactive values to match the saved configuration. Intended to be called
#' inside an \code{observeEvent()} triggered by a session file upload, and
#' again after model compilation completes for inputs that require a compiled
#' model to exist first.
#'
#' @param state A named list produced by \code{\link{get_session_state}},
#'   typically loaded via \code{readRDS()}. Must contain a \code{version} field
#'   for compatibility checking.
#' @param input The Shiny \code{input} object from the current server session.
#' @param session The Shiny \code{session} object passed to all
#'   \code{updateXxx()} calls.
#' @param rv A \code{\link[shiny]{reactiveValues}} object containing
#'   server-side state not bound to UI inputs. Fields are overwritten directly.
#' @param uploaded_data_override A \code{\link[shiny]{reactiveVal}} used to
#'   inject the restored dataset into \code{uploaded_data()} without requiring
#'   a file upload.
#' @param show_note Logical. Whether to display a version compatibility
#'   notification on restore. Default \code{TRUE}.
#' @param selectize_choices Workaround for restoring nonmem_y_axis and color_data_by   
#'
#' @return \code{NULL} invisibly. Called for its side effects.
#'
#' @details
#' Restoration covers: dataset cleaning options, NCA settings, plot options,
#' dosing regimens for both models, simulation settings, PSA options, and
#' variability settings. Inputs are restored via the appropriate
#' \code{updateXxx()} function for each widget type.
#'
#' A version compatibility notification is shown if \code{show_note = TRUE}
#' and a \code{version} field is present in \code{state}, but restoration
#' proceeds regardless of version match.
#' 
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinyWidgets updatePrettySwitch updatePickerInput
#'
#' @seealso \code{\link{get_session_state}} for the complementary save function.
#'
#' @export
restore_session_state <- function(state, input, session, rv, uploaded_data_override, show_note,
                                  selectize_choices = NULL) {
  
  # --- Version compatibility check ---
  if(show_note) {
    if (!is.null(state$version)) {
      showNotification(
        paste0("Session was saved with v", state$version, ". ",
               "Some settings may not restore correctly if you are on a different version."),
        type = "message", duration = 10
      )
    }
  }
  
  uploaded_data_override(state$data$uploaded_data)
  shinyAce::updateAceEditor(session, "codes",          value    = state$data$codes)
  
  # --- Model code & settings ---
  updateSelectInput(session, "model_select",         selected = '--------------------------------------------') # Dummy choice to not update model code
  updateSelectInput(session, "model_select2",        selected = '--------------------------------------------') # Dummy choice to not update model code
  shinyAce::updateAceEditor(session, "model_input",  value    = state$model$model_input)
  shinyAce::updateAceEditor(session, "model_input2", value    = state$model$model_input2)
  
  # --- Data: Dataset cleaning ---
  updateCheckboxInput(session, "create_cmt_col",      value    = state$data$create_cmt_col)
  updateCheckboxInput(session, "create_id_col",        value    = state$data$create_id_col)
  updateCheckboxInput(session, "create_time_col",      value    = state$data$create_time_col)
  updateCheckboxInput(session, "BLQ_filter",           value    = state$data$BLQ_filter)
  updateCheckboxInput(session, "EVID_filter",          value    = state$data$EVID_filter)
  updateCheckboxInput(session, "distinct_by_ID",       value    = state$data$distinct_by_ID)
  updateCheckboxInput(session, "turn_all_numeric",     value    = state$data$turn_all_numeric)
  updateSelectizeInput(session, "column",              selected = state$data$column)
  updateCheckboxInput(session, "enableAutocomplete",   value    = state$data$enableAutocomplete)
  
  # --- Data: Summary statistics ---
  updateCheckboxInput(session, "transpose_data_info",  value    = state$data$transpose_data_info)
  
  # --- Data: NCA options ---
  updateSelectizeInput(session, "subject_colname",     selected = state$data$subject_colname)
  updateSelectizeInput(session, "additional_keys",     selected = state$data$additional_keys)
  updateSelectizeInput(session, "time_colname",        selected = state$data$time_colname)
  updateTextInput(    session, "desc_time_unit",        value    = state$data$desc_time_unit)
  updateSelectizeInput(session, "conc_colname",        selected = state$data$conc_colname)
  updateTextInput(    session, "desc_conc_unit",        value    = state$data$desc_conc_unit)
  updateSelectizeInput(session, "dose_colname",        selected = state$data$dose_colname)
  updateTextInput(    session, "desc_dose_unit",        value    = state$data$desc_dose_unit)
  updateSelectizeInput(session, "adm_route",           selected = state$data$adm_route)
  updateNumericInput( session, "dur_inf",               value    = state$data$dur_inf)
  updateSelectizeInput(session, "down_method",         selected = state$data$down_method)
  updateNumericInput( session, "mw_value",              value    = state$data$mw_value)
  updateSelectizeInput(session, "nca_sigfigs",         selected = state$data$nca_sigfigs)
  updateCheckboxInput(session, "transpose_nca",        value    = state$data$transpose_nca)
  
  # --- Data: General plot options ---
  updateSelectizeInput(session, "y_axis",              selected = state$data$y_axis)
  updateSelectizeInput(session, "x_axis",              selected = state$data$x_axis)
  updateSelectizeInput(session, "color",               selected = state$data$color)
  updateSelectizeInput(session, "median_line_by",      selected = state$data$median_line_by)
  updateSelectizeInput(session, "facet_by",            selected = state$data$facet_by)
  updateSelectizeInput(session, "filter_cmt_data",     selected = state$data$filter_cmt_data)
  updateCheckboxInput(session, "log_y_axis_data",      value    = state$data$log_y_axis_data)
  updateCheckboxInput(session, "log_x_axis_data",      value    = state$data$log_x_axis_data)
  updateCheckboxInput(session, "insert_smoother",      value    = state$data$insert_smoother)
  updateCheckboxInput(session, "median_line_data",     value    = state$data$median_line_data)
  updateCheckboxInput(session, "insert_lm_eqn",        value    = state$data$insert_lm_eqn)
  shinyWidgets::updatePrettySwitch(session, "do_boxplot", value = state$data$do_boxplot)
  updateTextInput(    session, "plot_title_data",       value    = state$data$plot_title_data)
  updateSelectInput(  session, "select_label_size",    selected = state$data$select_label_size)
  updateSelectInput(  session, "quantize_x",           selected = state$data$quantize_x)
  updateCheckboxInput(session, "do_data_plotly",       value    = state$data$do_data_plotly)
  
  # --- Data: Individual plot options ---
  updateSelectizeInput(session, "filter_by_id",        selected = state$data$filter_by_id)
  updateSelectInput(  session, "page_number",          selected = state$data$page_number)
  updateSelectInput(  session, "number_of_rows",       selected = state$data$number_of_rows)
  updateSelectInput(  session, "number_of_cols",       selected = state$data$number_of_cols)
  updateSelectizeInput(session, "highlight_var",       selected = state$data$highlight_var)
  updateSelectizeInput(session, "highlight_var_values",selected = state$data$highlight_var_values)
  updateSelectizeInput(session, "lloq_colname",        selected = state$data$lloq_colname)
  updateSelectizeInput(session, "ind_dose_colname",    selected = state$data$ind_dose_colname)
  updateTextInput(    session, "dose_units",            value    = state$data$dose_units)
  updateCheckboxInput(session, "insert_dosing",        value    = state$data$insert_dosing)
  updateCheckboxInput(session, "fixed_scale",          value    = state$data$fixed_scale)
  updateCheckboxInput(session, "do_data_ind_plotly",   value    = state$data$do_data_ind_plotly)
  updateTextInput(    session, "plot_title_ind",        value    = state$data$plot_title_ind)
  updateSelectizeInput(session, "sort_by_ind",         selected = state$data$sort_by_ind)
  updateSelectizeInput(session, "strat_by_ind",        selected = state$data$strat_by_ind)
  updateSelectizeInput(session, "outlier_threshold",   selected = state$data$outlier_threshold)
  
  # --- Data: Correlation plot options ---
  updateSelectizeInput(session, "var_corr",            selected = state$data$var_corr)
  updateSelectizeInput(session, "color_corr",          selected = state$data$color_corr)
  
  # --- Data: Histogram options ---
  updateSelectizeInput(session, "var_hist",            selected = state$data$var_hist)
  updateNumericInput( session, "bin_size",              value    = state$data$bin_size)
  
  # --- Dosing: Model 1 ---
  updateSelectInput( session, "cmt1_model_1",   selected = state$dosing$cmt1_model_1)
  updateNumericInput(session, "amt1",            value    = state$dosing$amt1)
  updateNumericInput(session, "delay_time1",     value    = state$dosing$delay_time1)
  updateNumericInput(session, "total1",          value    = state$dosing$total1)
  updateNumericInput(session, "ii1",             value    = state$dosing$ii1)
  updateNumericInput(session, "tinf1",           value    = state$dosing$tinf1)
  
  updateSelectInput( session, "cmt2_model_1",   selected = state$dosing$cmt2_model_1)
  updateNumericInput(session, "amt2",            value    = state$dosing$amt2)
  updateNumericInput(session, "delay_time2",     value    = state$dosing$delay_time2)
  updateNumericInput(session, "total2",          value    = state$dosing$total2)
  updateNumericInput(session, "ii2",             value    = state$dosing$ii2)
  updateNumericInput(session, "tinf2",           value    = state$dosing$tinf2)
  
  updateSelectInput( session, "cmt3_model_1",   selected = state$dosing$cmt3_model_1)
  updateNumericInput(session, "amt3",            value    = state$dosing$amt3)
  updateNumericInput(session, "delay_time3",     value    = state$dosing$delay_time3)
  updateNumericInput(session, "total3",          value    = state$dosing$total3)
  updateNumericInput(session, "ii3",             value    = state$dosing$ii3)
  updateNumericInput(session, "tinf3",           value    = state$dosing$tinf3)
  
  updateSelectInput( session, "cmt4_model_1",   selected = state$dosing$cmt4_model_1)
  updateNumericInput(session, "amt4",            value    = state$dosing$amt4)
  updateNumericInput(session, "delay_time4",     value    = state$dosing$delay_time4)
  updateNumericInput(session, "total4",          value    = state$dosing$total4)
  updateNumericInput(session, "ii4",             value    = state$dosing$ii4)
  updateNumericInput(session, "tinf4",           value    = state$dosing$tinf4)
  
  updateSelectInput( session, "cmt5_model_1",   selected = state$dosing$cmt5_model_1)
  updateNumericInput(session, "amt5",            value    = state$dosing$amt5)
  updateNumericInput(session, "delay_time5",     value    = state$dosing$delay_time5)
  updateNumericInput(session, "total5",          value    = state$dosing$total5)
  updateNumericInput(session, "ii5",             value    = state$dosing$ii5)
  updateNumericInput(session, "tinf5",           value    = state$dosing$tinf5)
  
  updateCheckboxInput(session, "mw_checkbox",                value    = state$dosing$mw_checkbox)
  updateNumericInput( session, "mw",                         value    = state$dosing$mw)
  updateNumericInput( session, "multi_factor",               value    = state$dosing$multi_factor)
  updateCheckboxInput(session, "wt_based_dosing_checkbox",   value    = state$dosing$wt_based_dosing_checkbox)
  updateTextInput(    session, "wt_based_dosing_name",       value    = state$dosing$wt_based_dosing_name)
  updateCheckboxInput(session, "model_dur_checkbox",         value    = state$dosing$model_dur_checkbox)
  updateCheckboxInput(session, "model_rate_checkbox",        value    = state$dosing$model_rate_checkbox)
  
  # --- Dosing: Model 2 ---
  updateSelectInput( session, "cmt1_model_2",   selected = state$dosing$cmt1_model_2)
  updateNumericInput(session, "amt1_2",          value    = state$dosing$amt1_2)
  updateNumericInput(session, "delay_time1_2",   value    = state$dosing$delay_time1_2)
  updateNumericInput(session, "total1_2",        value    = state$dosing$total1_2)
  updateNumericInput(session, "ii1_2",           value    = state$dosing$ii1_2)
  updateNumericInput(session, "tinf1_2",         value    = state$dosing$tinf1_2)
  
  updateSelectInput( session, "cmt2_model_2",   selected = state$dosing$cmt2_model_2)
  updateNumericInput(session, "amt2_2",          value    = state$dosing$amt2_2)
  updateNumericInput(session, "delay_time2_2",   value    = state$dosing$delay_time2_2)
  updateNumericInput(session, "total2_2",        value    = state$dosing$total2_2)
  updateNumericInput(session, "ii2_2",           value    = state$dosing$ii2_2)
  updateNumericInput(session, "tinf2_2",         value    = state$dosing$tinf2_2)
  
  updateSelectInput( session, "cmt3_model_2",   selected = state$dosing$cmt3_model_2)
  updateNumericInput(session, "amt3_2",          value    = state$dosing$amt3_2)
  updateNumericInput(session, "delay_time3_2",   value    = state$dosing$delay_time3_2)
  updateNumericInput(session, "total3_2",        value    = state$dosing$total3_2)
  updateNumericInput(session, "ii3_2",           value    = state$dosing$ii3_2)
  updateNumericInput(session, "tinf3_2",         value    = state$dosing$tinf3_2)
  
  updateSelectInput( session, "cmt4_model_2",   selected = state$dosing$cmt4_model_2)
  updateNumericInput(session, "amt4_2",          value    = state$dosing$amt4_2)
  updateNumericInput(session, "delay_time4_2",   value    = state$dosing$delay_time4_2)
  updateNumericInput(session, "total4_2",        value    = state$dosing$total4_2)
  updateNumericInput(session, "ii4_2",           value    = state$dosing$ii4_2)
  updateNumericInput(session, "tinf4_2",         value    = state$dosing$tinf4_2)
  
  updateSelectInput( session, "cmt5_model_2",   selected = state$dosing$cmt5_model_2)
  updateNumericInput(session, "amt5_2",          value    = state$dosing$amt5_2)
  updateNumericInput(session, "delay_time5_2",   value    = state$dosing$delay_time5_2)
  updateNumericInput(session, "total5_2",        value    = state$dosing$total5_2)
  updateNumericInput(session, "ii5_2",           value    = state$dosing$ii5_2)
  updateNumericInput(session, "tinf5_2",         value    = state$dosing$tinf5_2)
  
  updateCheckboxInput(session, "mw_checkbox_2",                value    = state$dosing$mw_checkbox_2)
  updateNumericInput( session, "mw_2",                         value    = state$dosing$mw_2)
  updateNumericInput( session, "multi_factor_2",               value    = state$dosing$multi_factor_2)
  updateCheckboxInput(session, "wt_based_dosing_checkbox_2",   value    = state$dosing$wt_based_dosing_checkbox_2)
  updateTextInput(    session, "wt_based_dosing_name_2",       value    = state$dosing$wt_based_dosing_name_2)
  updateCheckboxInput(session, "model_dur_checkbox_2",         value    = state$dosing$model_dur_checkbox_2)
  updateCheckboxInput(session, "model_rate_checkbox_2",        value    = state$dosing$model_rate_checkbox_2)
  
  # --- Simulation options ---
  updateSelectInput( session, "time_unit",                 selected = state$sim$time_unit)
  updateTextInput(   session, "x_axis_label",              value    = state$sim$x_axis_label)
  updateSelectInput( session, "yaxis_name",                selected = state$sim$yaxis_name)
  updateSelectInput( session, "yaxis_name_model_2",        selected = state$sim$yaxis_name_model_2)
  updateTextInput(   session, "y_axis_label",              value    = state$sim$y_axis_label)
  updateTextInput(   session, "plot_title_sim",            value    = state$sim$plot_title_sim)
  updateCheckboxInput(session, "log_y_axis",               value    = state$sim$log_y_axis)
  updateCheckboxInput(session, "log_x_axis",               value    = state$sim$log_x_axis)
  updateCheckboxInput(session, "geom_point_sim_option",    value    = state$sim$geom_point_sim_option)
  updateCheckboxInput(session, "show_model_1",             value    = state$sim$show_model_1)
  updateCheckboxInput(session, "show_model_2",             value    = state$sim$show_model_2)
  updateCheckboxInput(session, "do_sim_plotly",            value    = state$sim$do_sim_plotly)
  updateNumericInput( session, "tgrid_max",                value    = state$sim$tgrid_max)
  updateNumericInput( session, "delta",                    value    = state$sim$delta)
  updateTextInput(   session, "custom_sampling_time_text", value    = state$sim$custom_sampling_time_text)
  updateCheckboxInput(session, "custom_sampling_time_cb",  value    = state$sim$custom_sampling_time_cb)
  updateCheckboxInput(session, "add_time_zero",            value    = state$sim$add_time_zero)
  
  # --- Simulation options (Dataset) ---
  updateSelectizeInput(session, "filter_cmt",              selected = state$sim$filter_cmt)
  
  if (!is.null(selectize_choices)) {
    updateSelectizeInput(session, "nonmem_y_axis", choices = selectize_choices, selected = state$sim$nonmem_y_axis)
    updateSelectizeInput(session, "color_data_by", choices = selectize_choices, selected = state$sim$color_data_by)
  } else {
    updateSelectizeInput(session, "nonmem_y_axis", selected = state$sim$nonmem_y_axis)
    updateSelectizeInput(session, "color_data_by", selected = state$sim$color_data_by)
  }
  
  updateCheckboxInput(session, "stat_sum_data_by",         value    = state$sim$stat_sum_data_by)
  updateCheckboxInput(session, "combine_nmdata",           value    = state$sim$combine_nmdata)
  updateCheckboxInput(session, "stat_sum_data_option",     value    = state$sim$stat_sum_data_option)
  updateCheckboxInput(session, "geom_point_data_option",   value    = state$sim$geom_point_data_option)
  
  # --- PSA ---
  updateSelectInput( session, "digits_model_1",                  selected = state$psa$digits_model_1)
  updateCheckboxInput(session, "dp_checkbox_model_1",            value    = state$psa$dp_checkbox_model_1)
  shinyWidgets::updatePickerInput( session,  "min_nca_obs_time_model_1",       selected = state$psa$min_nca_obs_time_model_1)
  shinyWidgets::updatePickerInput( session,  "max_nca_obs_time_model_1",       selected = state$psa$max_nca_obs_time_model_1)
  updateCheckboxInput(session, "log_y_axis_model_1",             value    = state$psa$log_y_axis_model_1)
  updateCheckboxInput(session, "geom_point_sim_option_model_1",  value    = state$psa$geom_point_sim_option_model_1)
  updateCheckboxInput(session, "log_x_axis_model_1",             value    = state$psa$log_x_axis_model_1)
  updateCheckboxInput(session, "geom_point_data_option_model_1", value    = state$psa$geom_point_data_option_model_1)
  updateCheckboxInput(session, "geom_vline_option_model_1",      value    = state$psa$geom_vline_option_model_1)
  updateCheckboxInput(session, "combine_nmdata_1_model_1",       value    = state$psa$combine_nmdata_1_model_1)
  updateCheckboxInput(session, "geom_ribbon_option_model_1",     value    = state$psa$geom_ribbon_option_model_1)
  updateCheckboxInput(session, "stat_sum_data_option_model_1",   value    = state$psa$stat_sum_data_option_model_1)
  updateTextInput(    session, "plot_title_psa_model_1",         value    = state$psa$plot_title_psa_model_1)
  updateCheckboxInput(session, "do_psa_plotly_model_1",          value    = state$psa$do_psa_plotly_model_1)
  
  updateSelectInput( session, "digits_model_2",                  selected = state$psa$digits_model_2)
  updateCheckboxInput(session, "dp_checkbox_model_2",            value    = state$psa$dp_checkbox_model_2)
  shinyWidgets::updatePickerInput( session,  "min_nca_obs_time_model_2",       selected = state$psa$min_nca_obs_time_model_2)
  shinyWidgets::updatePickerInput( session,  "max_nca_obs_time_model_2",       selected = state$psa$max_nca_obs_time_model_2)
  updateCheckboxInput(session, "log_y_axis_model_2",             value    = state$psa$log_y_axis_model_2)
  updateCheckboxInput(session, "geom_point_sim_option_model_2",  value    = state$psa$geom_point_sim_option_model_2)
  updateCheckboxInput(session, "log_x_axis_model_2",             value    = state$psa$log_x_axis_model_2)
  updateCheckboxInput(session, "geom_point_data_option_model_2", value    = state$psa$geom_point_data_option_model_2)
  updateCheckboxInput(session, "geom_vline_option_model_2",      value    = state$psa$geom_vline_option_model_2)
  updateCheckboxInput(session, "combine_nmdata_1_model_2",       value    = state$psa$combine_nmdata_1_model_2)
  updateCheckboxInput(session, "geom_ribbon_option_model_2",     value    = state$psa$geom_ribbon_option_model_2)
  updateCheckboxInput(session, "stat_sum_data_option_model_2",   value    = state$psa$stat_sum_data_option_model_2)
  updateTextInput(    session, "plot_title_psa_model_2",         value    = state$psa$plot_title_psa_model_2)
  updateCheckboxInput(session, "do_psa_plotly_model_2",          value    = state$psa$do_psa_plotly_model_2)
  
  # --- Tornado - Model 1 ---
  updateNumericInput(  session, "tor_lower_model_1",          value    = state$psa$tor_lower_model_1)
  updateNumericInput(  session, "tor_upper_model_1",          value    = state$psa$tor_upper_model_1)
  updateCheckboxInput( session, "tor_fix_model_1",            value    = state$psa$tor_fix_model_1)
  updateCheckboxInput( session, "tor_show_digits_model_1",    value    = state$psa$tor_show_digits_model_1)
  updateCheckboxInput( session, "tor_do_gradient_model_1",    value    = state$psa$tor_do_gradient_model_1)
  updateSelectizeInput(session, "tor_var_model_1",            selected = state$psa$tor_var_model_1)
  updateSelectInput(   session, "select_tor_metric_model_1",  selected = state$psa$select_tor_metric_model_1)
  updateSelectInput(   session, "tor_display_as_model_1",     selected = state$psa$tor_display_as_model_1)
  updateCheckboxInput( session, "trim_tor_model_1",           value    = state$psa$trim_tor_model_1)
  updateCheckboxInput( session, "show_bioeq_model_1",         value    = state$psa$show_bioeq_model_1)
  updateTextInput(     session, "plot_title_tor_model_1",     value    = state$psa$plot_title_tor_model_1)
  updateTextInput(     session, "xlab_tor_model_1",           value    = state$psa$xlab_tor_model_1)
  updateCheckboxInput( session, "tor_display_text_model_1",   value    = state$psa$tor_display_text_model_1)
  updateCheckboxInput( session, "spi_normalize_model_1",      value    = state$psa$spi_normalize_model_1)
  updateCheckboxInput( session, "do_tor_plotly_model_1",      value    = state$psa$do_tor_plotly_model_1)
  
  # --- Tornado - Model 2 ---
  updateNumericInput(  session, "tor_lower_model_2",          value    = state$psa$tor_lower_model_2)
  updateNumericInput(  session, "tor_upper_model_2",          value    = state$psa$tor_upper_model_2)
  updateCheckboxInput( session, "tor_fix_model_2",            value    = state$psa$tor_fix_model_2)
  updateCheckboxInput( session, "tor_show_digits_model_2",    value    = state$psa$tor_show_digits_model_2)
  updateCheckboxInput( session, "tor_do_gradient_model_2",    value    = state$psa$tor_do_gradient_model_2)
  updateSelectizeInput(session, "tor_var_model_2",            selected = state$psa$tor_var_model_2)
  updateSelectInput(   session, "select_tor_metric_model_2",  selected = state$psa$select_tor_metric_model_2)
  updateSelectInput(   session, "tor_display_as_model_2",     selected = state$psa$tor_display_as_model_2)
  updateCheckboxInput( session, "trim_tor_model_2",           value    = state$psa$trim_tor_model_2)
  updateCheckboxInput( session, "show_bioeq_model_2",         value    = state$psa$show_bioeq_model_2)
  updateTextInput(     session, "plot_title_tor_model_2",     value    = state$psa$plot_title_tor_model_2)
  updateTextInput(     session, "xlab_tor_model_2",           value    = state$psa$xlab_tor_model_2)
  updateCheckboxInput( session, "tor_display_text_model_2",   value    = state$psa$tor_display_text_model_2)
  updateCheckboxInput( session, "spi_normalize_model_2",      value    = state$psa$spi_normalize_model_2)
  updateCheckboxInput( session, "do_tor_plotly_model_2",      value    = state$psa$do_tor_plotly_model_2)
  
  # --- Variability ---
  updateSelectInput( session, "db_model_1",          selected = state$variability$db_model_1)
  updateNumericInput(session, "n_subj_model_1",       value    = state$variability$n_subj_model_1)
  updateNumericInput(session, "seed_number_model_1",  value    = state$variability$seed_number_model_1)
  updateSliderInput( session, "age_db_model_1",       value    = state$variability$age_db_model_1)
  updateSliderInput( session, "wt_db_model_1",        value    = state$variability$wt_db_model_1)
  updateSliderInput( session, "males_db_model_1",     value    = state$variability$males_db_model_1)
  updateSliderInput( session, "bmi_db_model_1",       value    = state$variability$bmi_db_model_1)
  updateTextInput(   session, "custom_cov_1_model_1", value    = state$variability$custom_cov_1_model_1)
  updateTextInput(   session, "custom_cov_2_model_1", value    = state$variability$custom_cov_2_model_1)
  updateTextInput(   session, "custom_cov_3_model_1", value    = state$variability$custom_cov_3_model_1)
  updateSelectInput( session, "custom_cov_1_dist_model_1", selected = state$variability$custom_cov_1_dist_model_1)
  updateSelectInput( session, "custom_cov_2_dist_model_1", selected = state$variability$custom_cov_2_dist_model_1)
  updateSelectInput( session, "custom_cov_3_dist_model_1", selected = state$variability$custom_cov_3_dist_model_1)
  
  # --- Variability: Model 2 ---
  updateSelectInput( session, "db_model_2",          selected = state$variability$db_model_2)
  updateNumericInput(session, "n_subj_model_2",       value    = state$variability$n_subj_model_2)
  updateNumericInput(session, "seed_number_model_2",  value    = state$variability$seed_number_model_2)
  updateSliderInput( session, "age_db_model_2",       value    = state$variability$age_db_model_2)
  updateSliderInput( session, "wt_db_model_2",        value    = state$variability$wt_db_model_2)
  updateSliderInput( session, "males_db_model_2",     value    = state$variability$males_db_model_2)
  updateSliderInput( session, "bmi_db_model_2",       value    = state$variability$bmi_db_model_2)
  updateTextInput(   session, "custom_cov_1_model_2", value    = state$variability$custom_cov_1_model_2)
  updateTextInput(   session, "custom_cov_2_model_2", value    = state$variability$custom_cov_2_model_2)
  updateTextInput(   session, "custom_cov_3_model_2", value    = state$variability$custom_cov_3_model_2)
  updateSelectInput( session, "custom_cov_1_dist_model_2", selected = state$variability$custom_cov_1_dist_model_2)
  updateSelectInput( session, "custom_cov_2_dist_model_2", selected = state$variability$custom_cov_2_dist_model_2)
  updateSelectInput( session, "custom_cov_3_dist_model_2", selected = state$variability$custom_cov_3_dist_model_2)
  
  # --- Variability: IIV plot options ---
  updateNumericInput( session, "upper_quartile",              value    = state$variability$upper_quartile)
  updateNumericInput( session, "lower_quartile",              value    = state$variability$lower_quartile)
  updateCheckboxInput(session, "show_ind_profiles",           value    = state$variability$show_ind_profiles)
  updateCheckboxInput(session, "do_iiv_plotly",               value    = state$variability$do_iiv_plotly)
  updateCheckboxInput(session, "show_iiv_model_1",            value    = state$variability$show_iiv_model_1)
  updateCheckboxInput(session, "log_y_axis_iiv",              value    = state$variability$log_y_axis_iiv)
  updateCheckboxInput(session, "show_iiv_model_2",            value    = state$variability$show_iiv_model_2)
  updateCheckboxInput(session, "log_x_axis_iiv",              value    = state$variability$log_x_axis_iiv)
  updateCheckboxInput(session, "combine_nmdata_iiv",          value    = state$variability$combine_nmdata_iiv)
  updateSelectInput(  session, "stat_sum_data_option_iiv",    selected = state$variability$stat_sum_data_option_iiv)
  updateSelectInput(  session, "geom_point_data_option_iiv",  selected = state$variability$geom_point_data_option_iiv)
  updateTextInput(    session, "plot_title_iiv",              value    = state$variability$plot_title_iiv)
  updateNumericInput( session, "y_value_threshold",           value    = state$variability$y_value_threshold)
  shinyWidgets::updatePickerInput( session,  "x_value_threshold",           selected = state$variability$x_value_threshold)
  updateCheckboxInput(session, "show_y_intercept_threshold",  value    = state$variability$show_y_intercept_threshold)
  updateCheckboxInput(session, "show_x_intercept_threshold",  value    = state$variability$show_x_intercept_threshold)
  
  # --- Variability: Exposure box plot options ---
  shinyWidgets::updatePickerInput( session,  "min_exp_obs_time_model",      selected = state$variability$min_exp_obs_time_model)
  shinyWidgets::updatePickerInput( session,  "max_exp_obs_time_model",      selected = state$variability$max_exp_obs_time_model)
  updateSelectInput(  session, "select_exp",                  selected = state$variability$select_exp)
  updateTextInput(    session, "exp_yaxis_label",             value    = state$variability$exp_yaxis_label)
  updateTextInput(    session, "exp_model_1_name",            value    = state$variability$exp_model_1_name)
  updateTextInput(    session, "exp_model_2_name",            value    = state$variability$exp_model_2_name)
  updateCheckboxInput(session, "exp_show_model_1",            value    = state$variability$exp_show_model_1)
  updateCheckboxInput(session, "exp_show_model_2",            value    = state$variability$exp_show_model_2)
  updateTextInput(    session, "plot_title_exp_model",        value    = state$variability$plot_title_exp_model)
  updateCheckboxInput(session, "exp_display_stats",           value    = state$variability$exp_display_stats)
  updateCheckboxInput(session, "do_exp_plotly",               value    = state$variability$do_exp_plotly)
  
  invisible(NULL)
}