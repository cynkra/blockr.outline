coal <- function(...) {
  for (x in list(...)) {
    if (!is.null(x)) {
      return(x)
    }
  }
  NULL
}

pkg_file <- function(...) {
  system.file(..., package = "blockr.outline")
}

# Call `fn` passing only the named arguments its installed version
# accepts, dropping any the current signature does not know. Guards
# against version skew in an external dependency: blockr.io's
# path_input_* signatures have grown over releases (e.g. `placeholder`,
# `extensions`), and a hard call to a not-yet-present argument aborts UI
# construction. Positional arguments and every named argument pass through
# untouched when the target takes `...`.
io_call <- function(fn, ...) {
  args <- list(...)
  fmls <- names(formals(fn))
  if ("..." %in% fmls) {
    return(do.call(fn, args))
  }
  nms <- names(args)
  keep <- is.null(nms) | nms == "" | nms %in% fmls
  do.call(fn, args[keep])
}

pkg_version <- function() {
  as.character(utils::packageVersion("blockr.outline"))
}

chr_ply <- function(x, fun, ...) {
  vapply(x, fun, character(1L), ...)
}

lgl_ply <- function(x, fun, ...) {
  vapply(x, fun, logical(1L), ...)
}
