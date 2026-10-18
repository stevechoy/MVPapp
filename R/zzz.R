# The code-editor preambles and the template models embed the mrgsolve version.
# They are built when the package is installed, so refresh the version at load
# time to reflect the mrgsolve actually in use.
.onLoad <- function(libname, pkgname) {
  ns  <- parent.env(environment())
  ver <- tryCatch(as.character(utils::packageVersion("mrgsolve")), error = function(e) NULL)
  if (is.null(ver)) return(invisible())
  for (nm in ls(ns, all.names = TRUE)) {
    if (!bindingIsLocked(nm, ns) && !bindingIsActive(nm, ns)) {
      x <- get(nm, envir = ns, inherits = FALSE)
      if (is.character(x) && length(x) == 1L && grepl("mrgsolve [v", x, fixed = TRUE)) {
        assign(nm, sub("mrgsolve \\[v[0-9.]+\\]", paste0("mrgsolve [v", ver, "]"), x), envir = ns)
      }
    }
  }
  invisible()
}
