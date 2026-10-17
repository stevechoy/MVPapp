required_packages <- list(
  bslib = "0.5.1",
  cowplot = NULL, 
  curl = NULL,
  data.table = "1.14.2", 
  dplyr = NULL,
  DT = NULL, 
  #ellmer = "0.4.0", # suggested, not required
  flextable = "0.6.10",
  forcats = NULL,
  GGally = NULL,
  ggplot2 = "3.4.4",
  grid = NULL, 
  gridExtra = NULL, 
  htmltools = "0.5.5",
  httr2 = NULL,
  magrittr = NULL,
  mime = NULL,
  mrgsolve = "1.5.0",
  #nonmem2rx, # suggested, not required
  #pdftools, # suggested, not required
  plotly = "4.10.1", 
  purrr = NULL,
  rhandsontable = "0.3.8",
  rlang = NULL, 
  scales = NULL,
  shiny = "1.7.4.1",
  shinyAce = NULL, 
  shinyBS = "0.61.1", 
  shinycssloaders = NULL,
  shinydashboard = "0.7.2",
  shinyFiles = NULL,
  shinyjs = "1.0", 
  shinythemes = NULL,
  shinyWidgets = "0.8.0",
  stringr = NULL, 
  tibble = NULL,
  tidyr = NULL,
  zip = NULL
)

# Find missing or outdated packages
packages_to_update <- character(0)

for (pkg in names(required_packages)) {
  min_ver <- required_packages[[pkg]]
  
  # Check if package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    packages_to_update <- c(packages_to_update, pkg)
  } else if (!is.null(min_ver)) {
    current_ver <- as.character(packageVersion(pkg))
    if (compareVersion(current_ver, min_ver) < 0) {
      packages_to_update <- c(packages_to_update, pkg)
    }
  }
}

# If not all packages are up to date, stop with instructions instead of
# installing anything automatically
if (length(packages_to_update) > 0) {
  stop(
    "The following packages are missing or outdated: ",
    paste(packages_to_update, collapse = ", "), "\n",
    "Please install/update them before launching the app, e.g.:\n",
    "install.packages(c(", paste0('"', packages_to_update, '"', collapse = ", "), "))",
    call. = FALSE
  )
}
