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

pkg_version <- function() {
  as.character(utils::packageVersion("blockr.outline"))
}

chr_ply <- function(x, fun, ...) {
  vapply(x, fun, character(1L), ...)
}

lgl_ply <- function(x, fun, ...) {
  vapply(x, fun, logical(1L), ...)
}
