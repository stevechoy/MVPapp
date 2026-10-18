# Plot helpers: axes, log breaks, regression labels, watermarks

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
#' @noRd
#---------------------------------------------------------------------------

lm_eqn <- function(df, facet_name, x, y) {
  if (!is.null(facet_name) && facet_name[1] != "") {
    df <- dplyr::group_by(df, !!!dplyr::syms(facet_name))
  }

  # One linear fit per group (one lm() call each)
  line_stats <- function(xv, yv) {
    if (identical(x, y)) { # lm() of a variable on itself gives an NA slope
      return(data.frame(rsq = 1, slope = 1, intercept = 0))
    }
    if (all(is.na(yv))) {  # nothing to fit in this group
      return(data.frame(rsq = NA_real_, slope = NA_real_, intercept = NA_real_))
    }
    fit <- stats::lm(yv ~ xv)
    data.frame(rsq       = summary(fit)$r.squared,
               slope     = unname(stats::coef(fit)[2]),
               intercept = unname(stats::coef(fit)[1]))
  }

  dplyr::summarise(df, line_stats(.data[[x]], .data[[y]])) %>%
    dplyr::mutate(slope_direction = dplyr::if_else(slope >= 0, " + ", " - "),
                  label = dplyr::case_when(
                    is.na(intercept) ~ "", # no data in this group at all
                    !is.na(slope) ~
                      paste0("y = ",
                             format(intercept, digits = 2),
                             slope_direction,
                             format(abs(slope), digits = 2),
                             "\u00B7x, R\u00B2 = ",
                             format(rsq, digits = 3)),
                    is.na(slope) ~ # only one x value: the intercept is the mean
                      paste0("y = ", format(intercept, digits = 2), " [mean]")
                  ))
}

#-------------------------------------------------------------------------------
#' @name generate_log_breaks
#' @title Log axis breaks
#'
#' @param base_values the base value(s) to apply the exponent to
#' @param start the starting exponent
#' @param end the last exponent
#'
#' @returns a numeric
#' @noRd
#-------------------------------------------------------------------------------

generate_log_breaks <- function(base_values, start, end) {
  # every base value times each power of 10, e.g. 1, 3, 10, 30, 100, ...
  as.vector(outer(base_values, 10^seq(start, end)))
}

#' @noRd
logbreaks_y <- signif(generate_log_breaks(c(1, 3), -10, 10), digits = 2)

#' @noRd
logbreaks_x <- logbreaks_y

# Grid lines at 1, 2, ..., 9 times each power of 10: the same positions as the
# log tick marks from annotation_logticks(), so grid lines and ticks line up
#' @noRd
log_minor_breaks <- generate_log_breaks(1:9, -10, 10)

#-------------------------------------------------------------------------------
#' Largest finite value of a column across several data frames (NULLs ignored)
#'
#' Used to pick axis breaks without the cost of `ggplot_build()`.
#' @noRd
max_x_of <- function(xvar, ...) {
  vals <- unlist(lapply(list(...), function(d) {
    if (is.data.frame(d) && xvar %in% names(d) && is.numeric(d[[xvar]])) d[[xvar]] else NULL
  }), use.names = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) NA_real_ else max(vals)
}

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
#' @noRd
#-------------------------------------------------------------------------------

smart_x_axis <- function(p1,
                         max_x  = NULL,
                         xlabel,
                         debug  = FALSE) {
  
  if (is.null(max_x)) {
    # Fallback only: ggplot_build() is expensive, so callers should pass max_x
    # (see max_x_of()). The x.range upper bound is expanded by ~5% by default,
    # so we reverse that
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
                        NULL  # unrecognised xlabel - fall through to pretty_breaks below
  )
  
  if (debug) message("x_tick_size: ", x_tick_size)
  
  p1 + if (is.null(x_tick_size) || ceiling(max_x / x_tick_size) >= 25) {
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  } else {
    ggplot2::scale_x_continuous(breaks = seq(0, max_x + x_tick_size, by = x_tick_size))
  }
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
#-------------------------------------------------------------------------------

create_facet_label <- function(df, sort_by) {

  if (length(setdiff(sort_by, "ID")) == 0) { # Edge case: nothing but ID to label by
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
    # (vectorised over subjects; previously built row by row, ~100x slower)
    label_parts <- lapply(sort_by, function(col_name) paste0(col_name, ": ", df_first[[col_name]]))
    df_first <- data.frame(
      ID          = df_first$ID,
      facet_label = if (nrow(df_first) == 0) character(0) else
        paste0("ID: ", df_first$ID, ", ", do.call(paste, c(label_parts, sep = ", "))),
      stringsAsFactors = FALSE
    )

    df <- dplyr::left_join(df, df_first, by = "ID")
  }
  return(df)
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
#' @noRd
#-------------------------------------------------------------------------------

calculate_tick_size <- function(abs_y, max_ticks = 12, nice_values = c(1,2,5,10)) {
  # No parameter changed the metric (range 0), or the range is NA/Inf:
  # any positive tick size works, and 0 would make seq() fail downstream
  if(length(abs_y) != 1 || !is.finite(abs_y) || abs_y <= 0) {
    return(1)
  }
  approx_tick_size <- abs_y / max_ticks

  # Find the nearest "nice" tick size (1, 2, 5, or 10 times a power of 10)
  magnitude      <- 10^floor(log10(approx_tick_size))  # Power of 10
  possible_ticks <- nice_values * magnitude      # Generate possible tick sizes
  tick_size <- min(possible_ticks[possible_ticks >= approx_tick_size]) # Return the minimum possible_ticks that exceeds approx_tick_size
  return(tick_size)
}
