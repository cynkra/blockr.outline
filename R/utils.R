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

# The reference document a render actually styles against: the outline's own
# `template` when it has one, otherwise the app-level default from
# `getOption("blockr.outline.template")`.
#
# The option exists because the deck is a property of the DEPLOYMENT, not of
# the board. `template` is extension STATE, so it serialises with the board --
# which means a constructor argument only ever reaches boards created after it
# was added: every workflow saved before an app shipped a house template
# restores its own empty template and keeps rendering against officer's stock
# deck, with no way to fix it short of every user typing the path into the
# gear. The option applies to all of them, old and new.
#
# Resolved at RENDER time and deliberately NOT folded into the state
# reactiveVal: an empty template must keep meaning "whatever this app
# declares", never "the absolute path that happened to exist on the machine
# where this board was last saved". A template typed into the gear still wins.
effective_template <- function(x) {
  x <- coal(x, "")
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(x)
  }
  coal(getOption("blockr.outline.template", ""), "")
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
