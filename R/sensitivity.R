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

  # width of columns in plot (value between 0 and 1)
  width <- 0.95

  # Ratio and % change divide by the reference metric, which is undefined at 0 (L54)
  if (display_as %in% c("Ratio", "Percentage") && (!is.finite(reference_value) || reference_value == 0)) {
    safely_showNotification(
      paste0("The reference value of the metric is 0 or not finite, so '", display_as, "' is undefined. Showing 'Value' instead."),
      type = "warning", duration = 10, id = "tornado_zero_ref")
    display_as <- "Value"
  }

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
    dplyr::arrange(!is.na(del), del) %>% # a failed bound run (del = NA) ranks as least sensitive, not most
    dplyr::select(Parameter, Level, output_pretransform)

  if(filter_rows != "" & is.numeric(filter_rows)) {
    if(filter_rows > 0) {
      df2 <- df2 %>%
        dplyr::slice_tail(n = filter_rows * 2) # multiply by 2 because it is upper + lower
    }
  }

  df2 <- df2 %>%
    dplyr::mutate(Parameter = fct_inorder(Parameter))

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
  
  # Ratio and % change divide by the reference metric, which is undefined at 0 (L54)
  if (display_as %in% c("Ratio", "Percentage") && (!is.finite(reference_value) || reference_value == 0)) {
    safely_showNotification(
      paste0("The reference value of the metric is 0 or not finite, so '", display_as, "' is undefined. Showing 'Value' instead."),
      type = "warning", duration = 10, id = "tornado_zero_ref")
    display_as <- "Value"
  }

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
    dplyr::mutate(del = max(Metric) - min(Metric)) %>% # spread = sensitivity
    dplyr::arrange(desc(del)) %>%
    dplyr::ungroup()
  
  df2 <- df2 %>%
    dplyr::mutate(Parameter = fct_inorder(Parameter))
  
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
