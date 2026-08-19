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
#' @param text The layout's Details field -- ONE field, the same content
#'   whatever the layout, so switching layouts reshapes the words rather
#'   than losing them. Lines starting with `- ` become bullets (numbered
#'   items on the agenda layout); lines without stay plain text, and a
#'   slide freely mixes both. Inline `**bold**` and `*italic*`.
#' @param labels Panel headings for the compare layout, separated by `|`
#'   (e.g. `"Before | After"`). Ignored by other layouts.
#' @param exhibits Linked block results, in slot order. More values than the
#'   layout has slots are not drawn; missing ones leave the slot empty.
#' @param paginate Single-exhibit layouts only: a table too tall for its
#'   slot is carried over further slides by blockr.viz's paginator (repeated
#'   header, the title marked `(2 of 3)`, the slide's subtitle and footnote
#'   stamped on every page), exactly like the deck's paged tables. `FALSE`
#'   truncates with a visible marker instead. Layouts with more than one
#'   content slot always truncate: a second slide would duplicate the other
#'   slots.
#'
#' @return A `blockr_slide` object.
#'
#' @export
slide <- function(layout = "exhibit-full", title = "", subtitle = "",
                  footnote = "", text = "", labels = "", exhibits = list(),
                  paginate = TRUE) {

  spec <- slide_layout_spec(layout)

  structure(
    list(
      layout = layout,
      title = as_chr1(title),
      subtitle = as_chr1(subtitle),
      footnote = as_chr1(footnote),
      text = as_chr1(text),
      labels = as_chr1(labels),
      exhibits = exhibits,
      paginate = isTRUE(paginate)
    ),
    class = "blockr_slide"
  )
}

# Pagination applies when the layout's ONLY content slot is the exhibit --
# then nothing on a continuation slide duplicates, which is the whole of
# requirement 21's objection to a composed slide paging.
slide_single_exhibit <- function(x) {
  spec <- slide_layout_spec(x$layout)
  body <- rect(0, 0, 1, 1)
  kinds <- chr_ply(spec$build(body), `[[`, "kind")
  identical(kinds, "exhibit")
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
# The Details field accepts exactly what both painters can draw as runs:
# **bold**, *italic*, line breaks, and the `- ` bullet marker (spec req.
# 16). A line without the marker is plain text -- no bullet means no
# bullet, and a slide mixes a sentence and a list freely. Everything else a
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

# The Details field as classified lines: each entry `list(runs=, bullet=)`.
# `- ` (or `* `) opens a bullet line; anything else is a plain line.
md_lines <- function(text) {
  lines <- strsplit(coal(text, ""), "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines, function(l) {
    l <- trimws(l)
    bullet <- grepl("^[-*] ", l)
    if (bullet) {
      l <- sub("^[-*] +", "", l)
    }
    list(runs = md_runs(l), bullet = bullet)
  })
}

# The compare layout's panel headings, split on `|`.
panel_labels <- function(labels, n) {
  parts <- trimws(strsplit(coal(labels, ""), "|", fixed = TRUE)[[1L]])
  out <- character(n)
  out[seq_len(min(n, length(parts)))] <- parts[seq_len(min(n, length(parts)))]
  out
}
