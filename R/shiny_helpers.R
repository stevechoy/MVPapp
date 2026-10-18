# Shiny UI and safety wrappers

#' @name today_numeric
#' @title Get today's date as YYYYMMDD as seed input
#'
#' @returns A numeric
#' @examples
#' \dontrun{
#' today_numeric()
#' }
#' @importFrom magrittr %>% %<>% %T>% %$%
#' @noRd
today_numeric  <- function() {
  format(Sys.Date(), "%Y%m%d") %>% as.numeric()
}

#-------------------------------------------------------------------------------
#' @name safely_eval
#'
#' @title Error-capturing (safely) wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `eval`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `eval`
#' @noRd
#-------------------------------------------------------------------------------

safely_eval  <- mvp_safely(eval)

#-------------------------------------------------------------------------------
#' @name safely_parse
#'
#' @title Error-capturing (safely) wrappers for various functions
#' @param ... args to be passed
#'
#' @returns A list with two elements:
#' * `result`: The result of `parse`, or `NULL` if an error occurred.
#' * `error`: The error that occurred, or `NULL` if no error occurred.
#'
#' @seealso `parse`
#' @noRd
#-------------------------------------------------------------------------------

safely_parse <- mvp_safely(parse)

#-------------------------------------------------------------------------------
#' Exported from shinyBS:::buildTooltipOrPopoverOptionsList
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
    # Exact name (e.g. "AUC_ranged") or "<prefix>_<METRIC>_ranged", so that
    # CFBPCT does not also match MEANCFBPCT
    cols <- names(input_dataset)
    hit  <- cols[cols == name_ends_with | endsWith(cols, paste0("_", name_ends_with))]
    if (length(hit) == 0) NA_real_ else unique(input_dataset[[hit[1]]])
  }
  
  # Direct function calls — no pipe overhead on scalar operations
  metric_value <- if (dp) round(metric_value, digits = sigdig) else signif(metric_value, digits = sigdig)
  
  shinydashboard::valueBox(
    value    = htmltools::tags$p(style = font_size, if (length(metric_value) != 1 || !is.finite(metric_value)) "N/A" else metric_value),
    subtitle = value_box_subtitle,
    width    = width,
    color    = color,
    href     = NULL
  )
}

#-------------------------------------------------------------------------------
#' @name create_alert
#' @title Function that generates a shinyalert
#'
#' @param ppm_name       Name of Project Pharmacometrician
#' @param ppm_email      Email of Project Pharmacometrician
#'
#' @returns a shinyalert() popup
#' @noRd
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
#' @noRd
#-------------------------------------------------------------------------------
useShinydashboardMVP <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(htmltools::tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}

#-------------------------------------------------------------------------------
#' @name general_warning_modal
#' @title Creates a general warning modal
#' @param title            Title for the modal box
#' @param text_description UI elements
#'
#' @returns a modal dialog
#' @noRd
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
#' @noRd
#-------------------------------------------------------------------------------

check_cov_name <- function(orig_name, replaced_name = "DUMMY", list_of_reserved_strings = c("AGE", "AGEMO", "SEX", "WT", "BMI", "BSA", "ID", "USUBJID")) {

  if (orig_name %in% list_of_reserved_strings) {
    new_name <- replaced_name
    shiny::showNotification(paste0("WARNING: Covariate name is reserved. Will show up as ", replaced_name, " instead."), type = "warning", duration = 10)
  } else {
    new_name <- orig_name
  }
  return(new_name)
}

#-------------------------------------------------------------------------------
#' @name safely_showNotification
#'
#' @title A safe version of shiny notifications, mostly for used in testing
#' @param ... args to be passed
#'
#' @returns A shiny showNotification or print to console
#' @importFrom shiny isRunning showNotification
#' @importFrom rlang %||%
#' @seealso `parse`
#' @noRd
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
#' @noRd
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
#' @noRd
#-------------------------------------------------------------------------------

safely_incProgress <- function(amount, detail = NULL) {
  if (shiny::isRunning()) {
    shiny::incProgress(amount, detail = detail)
  } else if (!is.null(detail)) {
    message(paste0("[PROGRESS] ", detail))
  }
}

#-------------------------------------------------------------------------------
#' @name help_popover
#' 
#' @title Help icon with a Bootstrap 3 popover
#'
#' @description 
#' Returns a question-mark icon that shows a popover on hover or click. Use it
#' where `shinyBS::bsPopover()` fails (e.g. `shinydashboard::box()` titles).
#'
#' Requires the delegated popover init script to be included once in the UI
#' (e.g. in `dashboardBody(tags$head(...))`). Without it, nothing is shown.
#'
#' @param title Popover heading.
#' @param content Popover body. HTML is rendered.
#' @param placement `"top"`, `"bottom"`, `"left"` or `"right"` (default).
#' @param trigger `"hover"` (default) or `"click"`.
#'
#' @return An `<i>` [htmltools::tag].
#'
#' @noRd
#-------------------------------------------------------------------------------
help_popover <- function(title, content, placement = "right", trigger = "hover") {
  htmltools::tags$i(
    class = "fa fa-circle-question", style = "margin-left: 4px;",
    `data-toggle` = "popover", `data-placement` = placement,
    `data-trigger` = trigger,
    title = title, `data-content` = content
  )
}

#-------------------------------------------------------------------------------
#' @name safe_seed
#'
#' @title Validate a seed typed into the app
#'
#' @description Pauses the calling reactive/observer (via `shiny::req()`) while
#'   the Seed box is empty or not a finite number, instead of erroring in
#'   `set.seed()` (which would close the session from inside an observer).
#'
#' @param x Seed value from a numericInput.
#'
#' @returns `x`, if it is a single finite number.
#' @noRd
#-------------------------------------------------------------------------------
safe_seed <- function(x) {
  shiny::req(is.numeric(x), length(x) == 1, is.finite(x))
  x
}
