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

# The reference document a render styles against: the app-level
# `getOption("blockr.outline.template")`, otherwise the bundled deck above.
# An app declares it once, typically from its theme
# (`blockr.theme::theme_template(thm, "pptx")`).
#
# One source, and deliberately not the board. The deck is a property of the
# DEPLOYMENT: the same workflow downloaded from the house instance and from a
# laptop should carry the house master in the first case and not pretend to in
# the second. Both extensions used to offer a `template` field in their gear,
# which made it extension STATE -- so it serialised with the board as an
# ABSOLUTE path from whichever machine last saved it, resolved nowhere on the
# next one, and quietly lost to the fallback deck. It also only ever reached
# boards created after an app shipped a house template: every workflow saved
# before that kept rendering against the stock deck, with no fix short of
# every user typing the path in by hand. The option applies to all of them,
# old and new, and is resolved at RENDER time.
#
# The `template` constructor arguments survive as ignored LEGACY arguments,
# because a board saved with the old field restores its state through the
# constructor and must not error on the way in.
effective_template <- function() {
  coal(getOption("blockr.outline.template", default_template()), "")
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
