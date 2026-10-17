# Plot helpers: axes, log breaks, regression labels, watermarks

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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
#-------------------------------------------------------------------------------

generate_log_breaks <- function(base_values, start, end) {
  powers      <- seq(from = start, to = end)
  breaks      <- rep(0, length(base_values) * length(powers))

  for (i in seq_along(powers)) {
    breaks[((i - 1) * length(base_values) + 1):(i * length(base_values))] <- base_values * 10^powers[i]
  }

  return(breaks)
}

#' @noRd
logbreaks_y <- generate_log_breaks(c(1,3), -10, 10) %>% signif(digits = 2)

#' @noRd
logbreaks_x <- logbreaks_y

#' @noRd
logbreaks_y_log10 <- generate_log_breaks(c(1), -10, 10) %>% signif(digits = 2)

#' @noRd
logbreaks_x_log10 <- logbreaks_y_log10

#' @noRd
logbreaks_y_minor <- generate_log_breaks(c(1:9), -10, 10) %>% signif(digits = 2)

#' @noRd
logbreaks_x_minor <- logbreaks_y_minor

#' @noRd
log10_axis_label <- rep("", length(logbreaks_y_minor))
#' @noRd
log10_axis_label[seq(1, length(logbreaks_y_minor), 9)] <- as.character(logbreaks_y_minor)[seq(1, length(logbreaks_y_minor), 9)] # every 9th tick is labelled

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
#' @noRd
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
#' @importFrom gridExtra arrangeGrob grid.arrange marrangeGrob
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
  approx_tick_size <- abs_y / max_ticks

  # Find the nearest "nice" tick size (1, 2, 5, or 10 times a power of 10)
  magnitude      <- 10^floor(log10(approx_tick_size))  # Power of 10
  possible_ticks <- nice_values * magnitude      # Generate possible tick sizes
  tick_size <- min(possible_ticks[possible_ticks >= approx_tick_size]) # Return the minimum possible_ticks that exceeds approx_tick_size
  return(tick_size)
}
