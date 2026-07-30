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

# A string for a double-quoted yaml scalar. Board titles are user text and
# reach the front matter in several places; an unescaped quote closes the
# scalar early and the render dies on malformed yaml.
yaml_dq <- function(x) {
  gsub("\"", "\\\\\"", x)
}

# The bundled fallback beneath the app-level option: a neutral 13.333x7.5in
# (true 16:9) deck, BUILT from officer's stock Office layouts by
# dev/make-default-template.R -- no media, no text, Arial.
#
# Exists because officer's OWN stock deck (read_pptx() with no path) is
# 10x7.5in -- 4:3 -- while every exhibit's pptx_width sizes to ~11.9in
# (gg_attach_pptx_size(), static_table()'s ft_fit_width default): widescreen
# math laid onto a 4:3 slide, overflowing the right edge on every chart and
# table, on every board that never configured a house deck.
#
# Generated rather than copied, and that is the point. The first version of
# this file was a client master with the logo deleted, which kept its font
# scheme and its footer strings inside an open package -- branding nobody
# asked for on every deployment that sets no template of its own.
default_template <- function() {
  pkg_file("templates", "widescreen-default.pptx")
}

# The reference document a render actually styles against: the outline's own
# `template` when it has one, otherwise the app-level default from
# `getOption("blockr.outline.template")`, otherwise the bundled deck above.
#
# The option exists because the deck is a property of the DEPLOYMENT, not of
# the board. `template` is extension STATE, so it serialises with the board --
# which means a constructor argument only ever reaches boards created after it
# was added: every workflow saved before an app shipped a house template
# restores its own empty template and keeps rendering against the fallback
# deck, with no way to fix it short of every user typing the path into the
# gear. The option applies to all of them, old and new.
#
# Resolved at RENDER time and deliberately NOT folded into the state
# reactiveVal: an empty template must keep meaning "whatever this app
# declares", never "the absolute path that happened to exist on the machine
# where this board was last saved". A template typed into the gear still wins.
#
# A stored path that no longer EXISTS is treated the same as an empty one, and
# that is the half this used to miss. The stored value is an absolute path from
# whichever machine last saved the board, so a board moved between deployments
# (or saved on a laptop and opened on Connect) carries one that resolves
# nowhere. Returning it anyway does not fail: render_pptx_officer() checks
# file.exists() and quietly falls back to the fallback deck -- so the house
# template is silently ignored while an app-level default sits right there
# unused. Exactly the "absolute path that happened to exist" case the paragraph
# above rules out.
effective_template <- function(x) {
  x <- coal(x, "")
  usable <- is.character(x) && length(x) == 1L && nzchar(x)

  if (usable && file.exists(x)) {
    return(x)
  }

  fallback <- coal(getOption("blockr.outline.template", default_template()), "")

  # Worth saying out loud: the render succeeds either way, so a stale path is
  # otherwise indistinguishable from having no house deck at all.
  if (usable) {
    message(
      "Board template '", x, "' does not exist here; ",
      if (nzchar(fallback)) {
        paste0("using the app default '", fallback, "'.")
      } else {
        "no app default is set, so the render uses the stock deck."
      }
    )
  }

  fallback
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
