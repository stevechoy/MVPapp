# Parameter sensitivity analysis (tornado and spider plots)

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
#' @importFrom ggplot2 ggplot geom_rect theme_bw geom_hline scale_x_continuous scale_fill_manual coord_flip
#' @importFrom ggplot2 scale_y_continuous aes labs ggtitle
#' @importFrom scales percent pretty_breaks
#' @returns a ggplot object
#' @noRd
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

  width <- 0.95 # bar width (between 0 and 1)
  scale <- sensitivity_scale(display_as, reference_value, metric_name)

  # One row per parameter and bound, the most sensitive parameters last (= on top)
  df2 <- df %>%
    dplyr::rename(Parameter = !!dplyr::sym(param_name),
                  Lower     = !!dplyr::sym(lower_name),
                  Upper     = !!dplyr::sym(upper_name)) %>%
    dplyr::mutate(del = abs(Upper - reference_value) + abs(Lower - reference_value)) %>%
    tidyr::pivot_longer(cols = c(Lower, Upper), names_to = "Level", values_to = "output_pretransform") %>%
    dplyr::arrange(!is.na(del), del) %>% # a failed bound run (del = NA) ranks as least sensitive, not most
    dplyr::select(Parameter, Level, output_pretransform)

  if (is.numeric(filter_rows) && filter_rows > 0) {
    df2 <- dplyr::slice_tail(df2, n = filter_rows * 2) # 2 rows (lower, upper) per parameter
  }

  df2 <- df2 %>%
    dplyr::mutate(Parameter = fct_inorder(Parameter),
                  output    = scale$transform(output_pretransform))

  if (display_text) {
    # No label within 1% of the reference
    percent_change <- (df2$output_pretransform - reference_value) / reference_value * 100
    df2$label_text <- dplyr::case_when(
      percent_change > -1 & percent_change < 1 ~ "",
      scale$display_as == "Percentage"         ~ paste0(round(df2$output * 100, 0), "%"),
      TRUE                                     ~ as.character(round(df2$output, 2)))
  }

  # Bars from the reference line to each value
  df2 <- df2 %>%
    dplyr::mutate(ymin = pmin(output, scale$ref_line),
                  ymax = pmax(output, scale$ref_line),
                  xmin = as.numeric(Parameter) - width / 2,
                  xmax = as.numeric(Parameter) + width / 2,
                  tooltip = paste0("Parameter: ", Parameter, "<br>Value: ", round(output, 2))) # plotly tooltip

  # `text` is not a ggplot2 aesthetic; plotly uses it for the tooltip, so only
  # ggplot2's "Ignoring unknown aesthetics" warning is muffled
  rect_layer <- withCallingHandlers(
    ggplot2::geom_rect(data = df2, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=Level, text = tooltip)),
    warning = function(w) {
      if (grepl("unknown aesthetics", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    })

  p <- ggplot2::ggplot() +
    rect_layer +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = scale$ref_line) +
    ggplot2::scale_x_continuous(breaks = seq_along(unique(df2$Parameter)),
                                labels = unique(df2$Parameter)) +
    ggplot2::scale_fill_manual(values = c(Lower = lower_color, Upper = upper_color) ) +
    ggplot2::coord_flip()

  p <- add_sensitivity_y_axis(p, c(df2$ymin, df2$ymax), scale, bioeq_lines, max_ticks = 12, value_breaks = 10)

  # Values at the outer end of each bar
  if (display_text) {
    df2$Parameter <- as.numeric(df2$Parameter)
    p <- p +
      ggplot2::geom_text(data = df2,
                         ggplot2::aes(x = Parameter, y = output, label = label_text),
                         hjust = ifelse(df2$output_pretransform > reference_value, -0.2, 1.2),
                         vjust = 0.5)
  }

  p +
    ggplot2::labs(x = NULL, y = if (xlabname != "") xlabname else scale$label) +
    ggplot2::ggtitle(plot_title)
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
#' @noRd
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
#' @importFrom ggplot2 ggplot geom_rect theme_bw geom_hline scale_x_continuous scale_fill_manual coord_flip
#' @importFrom ggplot2 scale_y_continuous aes labs ggtitle
#' @importFrom scales percent pretty_breaks
#' @returns a ggplot object
#' @noRd
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
  
  scale <- sensitivity_scale(display_as, reference_value, metric_name)
  
  # Parameters ordered by sensitivity (spread of the metric), most sensitive first
  df2 <- df %>%
    dplyr::rename(Parameter = !!dplyr::sym(param_name),
                  Ratio     = !!dplyr::sym(ratio_name),
                  Metric    = !!dplyr::sym(metric_name)) %>%
    dplyr::group_by(Parameter) %>%
    dplyr::mutate(del = max(Metric) - min(Metric)) %>%
    dplyr::arrange(dplyr::desc(del)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Parameter = fct_inorder(Parameter),
                  output    = scale$transform(Metric))
  
  if (is.numeric(filter_rows) && filter_rows > 0) {
    df2 <- dplyr::filter(df2, Parameter %in% levels(df2$Parameter)[1:filter_rows])
  }
  
  p <- if (normalize_x_axis) {
    # fold changes as evenly spaced categories
    df2$RatioN <- factor(round(df2$Ratio, 3), levels = sort(unique(round(df2$Ratio, 3))))
    ggplot2::ggplot(data = df2, ggplot2::aes(x = RatioN, y = output, color = Parameter, group = Parameter))
  } else {
    ggplot2::ggplot(data = df2, ggplot2::aes(x = Ratio, y = output, color = Parameter))
  }
  
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::geom_point() + 
    ggplot2::geom_line()
  
  p <- add_sensitivity_y_axis(p, df2$output, scale, bioeq_lines, max_ticks = 10, value_breaks = 8)
  
  if (!normalize_x_axis) {
    # label each fold change when there are few of them
    ratios <- unique(round(df2$Ratio, 3))
    p <- p + if (length(ratios) <= 9) {
      ggplot2::scale_x_continuous(breaks = ratios, labels = ratios)
    } else {
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
    }
  }
  
  p +
    ggplot2::labs(x = "Parameter Fold Change", y = if (ylabname != "") ylabname else scale$label) +
    ggplot2::ggtitle(plot_title)
}

#-------------------------------------------------------------------------------
#' @name sensitivity_scale
#'
#' @title How a sensitivity metric is shown
#'
#' @description
#' "Ratio" (to the reference), "Percentage" (change from the reference) or
#' "Value". Ratio and Percentage are undefined for a reference of 0 (or not
#' finite); "Value" is then used, with a notification.
#'
#' @param display_as       "Ratio", "Percentage" or "Value"
#' @param reference_value  metric of the reference run
#' @param metric_name      metric name, for the axis label
#'
#' @returns a list: `display_as`, `transform` (function of the metric values),
#'   `ref_line` (reference on the transformed scale), `bioeq` (80-125% limits on
#'   that scale) and `label` (axis label)
#' @noRd
#-------------------------------------------------------------------------------
sensitivity_scale <- function(display_as, reference_value, metric_name) {
  if (display_as %in% c("Ratio", "Percentage") && (!is.finite(reference_value) || reference_value == 0)) {
    safely_showNotification(
      paste0("The reference value of the metric is 0 or not finite, so '", display_as, "' is undefined. Showing 'Value' instead."),
      type = "warning", duration = 10, id = "tornado_zero_ref")
    display_as <- "Value"
  }
  switch(display_as,
         "Ratio"      = list(display_as = display_as,
                             transform  = function(v) v / reference_value,
                             ref_line   = 1,
                             bioeq      = c(0.80, 1.25),
                             label      = paste0(metric_name, " (Ratio to Reference)")),
         "Percentage" = list(display_as = display_as,
                             transform  = function(v) (v - reference_value) / reference_value,
                             ref_line   = 0,
                             bioeq      = c(-0.2, 0.25),
                             label      = paste0(metric_name, " (% Change From Reference)")),
         "Value"      = list(display_as = display_as,
                             transform  = function(v) v,
                             ref_line   = reference_value,
                             bioeq      = reference_value * c(0.8, 1.25),
                             label      = paste0(metric_name, " Value")))
}

#-------------------------------------------------------------------------------
#' @name add_sensitivity_y_axis
#'
#' @title Breaks of the metric axis of the tornado and spider plots, plus the
#'   optional 80-125% (bioequivalence) lines
#'
#' @param p             ggplot object
#' @param y             plotted values (on the transformed scale)
#' @param scale         from sensitivity_scale()
#' @param bioeq_lines   draw the 80-125% lines (and keep them in view)
#' @param max_ticks     approximate maximum number of breaks (Ratio / Percentage)
#' @param value_breaks  number of breaks for "Value"
#'
#' @returns the ggplot object
#' @noRd
#-------------------------------------------------------------------------------
add_sensitivity_y_axis <- function(p, y, scale, bioeq_lines, max_ticks, value_breaks) {
  if (bioeq_lines) {
    p <- p + ggplot2::geom_hline(yintercept = scale$bioeq, linetype = "dashed", alpha = 0.5)
  }
  if (scale$display_as == "Value") {
    return(p + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = value_breaks)))
  }

  max_y <- max(y, na.rm = TRUE)
  min_y <- min(y, na.rm = TRUE)
  if (!is.finite(max_y) || !is.finite(min_y)) return(p)

  # Tick size from the spread around the reference line, which also keeps the
  # upper bioequivalence line in view when shown (fold changes can be tiny)
  spread <- abs(max_y - scale$ref_line) + abs(min_y - scale$ref_line)
  if (bioeq_lines) spread <- max(spread, scale$bioeq[2] - scale$ref_line)
  tick <- calculate_tick_size(abs_y = spread, max_ticks = max_ticks, nice_values = c(1, 2, 5, 10))

  if (scale$display_as == "Percentage") {
    # breaks on multiples of the tick size
    lo <- floor(min_y / tick) * tick
    hi <- ceiling(max_y / tick) * tick
    if (bioeq_lines) {
      lo <- min(lo, scale$bioeq[1])
      hi <- max(hi, scale$bioeq[2])
    }
    p + ggplot2::scale_y_continuous(breaks = seq(lo, hi, tick), labels = function(x) paste0(x * 100, "%"))
  } else {
    # Ratio: from 0
    hi <- max_y + tick
    if (bioeq_lines) hi <- max(hi, scale$bioeq[2])
    p + ggplot2::scale_y_continuous(breaks = seq(0, hi, tick))
  }
}
