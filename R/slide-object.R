#' A composed slide
#'
#' The value a slide block evaluates to: a layout id, the authored strings
#' (title, subtitle, footnote and the layout's own text field) and the linked
#' exhibit values, in slot order. The object is inert data -- painting it is
#' the painters' job (the HTML preview and the officer pptx writer consume
#' the same [slide_slots()] geometry), which is what keeps the block's
#' preview and its download the same slide.
#'
#' `text` is the chosen layout's one authored field; what it means depends on
#' the layout (the takeaway line under an exhibit, the bullet list of a text
#' slide, the kicker of a section divider). It survives a layout switch on
#' purpose: a takeaway rewritten as bullets is the same words in a different
#' shape.
#'
#' @param layout Layout id, one of `names(slide_layouts())`.
#' @param title,subtitle,footnote The slide chrome. Empty strings render
#'   nothing (and an empty subtitle hands its room to the body).
#' @param text The layout's text field (see above). Inline `**bold**` and
#'   `*italic*` are honoured; bullet-shaped fields read one bullet per line.
#' @param exhibits Linked block results, in slot order. More values than the
#'   layout has slots are not drawn; missing ones leave the slot empty.
#'
#' @return A `blockr_slide` object.
#'
#' @export
slide <- function(layout = "exhibit-full", title = "", subtitle = "",
                  footnote = "", text = "", exhibits = list()) {

  spec <- slide_layout_spec(layout)

  structure(
    list(
      layout = layout,
      title = as_chr1(title),
      subtitle = as_chr1(subtitle),
      footnote = as_chr1(footnote),
      text = as_chr1(text),
      exhibits = exhibits
    ),
    class = "blockr_slide"
  )
}

as_chr1 <- function(x) {
  if (is.null(x) || !length(x)) "" else paste(as.character(x), collapse = "\n")
}

#' @export
format.blockr_slide <- function(x, ...) {
  spec <- slide_layout_spec(x$layout)
  paste0(
    "<blockr_slide> ", spec$name,
    if (nzchar(x$title)) paste0(": “", x$title, "”"),
    " (", length(x$exhibits), "/", spec$inputs, " exhibits)"
  )
}

#' @export
print.blockr_slide <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# --- markdown runs ----------------------------------------------------------
#
# The authored text fields accept exactly what both painters can draw as
# runs: **bold**, *italic* and line breaks (spec req. 16). Everything else a
# markdown parser would recognise is treated as literal text, so nothing is
# accepted that would then be silently dropped from the pptx.
#
# Parsed per line into a list of runs, each `list(text=, bold=, italic=)`.
md_runs <- function(line) {

  out <- list()
  rest <- line

  # perl = TRUE throughout: POSIX TRE has no lazy quantifier, so the same
  # pattern would swallow leading tokens into the prefix as literal text.
  pat <- "^(.*?)(\\*\\*[^*]+\\*\\*|\\*[^*]+\\*)(.*)$"

  while (grepl(pat, rest, perl = TRUE)) {
    m <- regmatches(rest, regexec(pat, rest, perl = TRUE))[[1L]]
    if (nzchar(m[[2L]])) {
      out <- c(out, list(list(text = m[[2L]], bold = FALSE, italic = FALSE)))
    }
    tok <- m[[3L]]
    if (startsWith(tok, "**")) {
      out <- c(out, list(list(
        text = substr(tok, 3L, nchar(tok) - 2L), bold = TRUE, italic = FALSE
      )))
    } else {
      out <- c(out, list(list(
        text = substr(tok, 2L, nchar(tok) - 1L), bold = FALSE, italic = TRUE
      )))
    }
    rest <- m[[4L]]
  }

  if (nzchar(rest)) {
    out <- c(out, list(list(text = rest, bold = FALSE, italic = FALSE)))
  }

  out
}

# A text field as lines of runs (bullet fields: one bullet per line).
md_lines <- function(text) {
  lines <- strsplit(coal(text, ""), "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, md_runs)
}
