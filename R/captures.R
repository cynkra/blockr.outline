# The browser draws the charts a document carries ------------------------------
#
# A deck and a report are both generated FROM an open board, so the browser
# holding the charts is the one that can draw them. blockr.viz's capture
# service takes a request for a block at a box we choose and answers with a
# bitmap, mounting the chart offscreen when its panel was never opened.
#
# The reply crosses the websocket, so an export is ask-now-write-later: this
# holds the tokens between the two, and both extensions drive it the same way
# (slides.R and report.R), which is why it lives here rather than in either.

# The box a chart is drawn into, in CSS pixels at 96dpi:
#   deck    11.9in x 5.5in, the slide's content area
#   report   6.5in x 4.0in, a figure in a page with margins
#' @noRd
capture_box <- function(kind = c("deck", "report")) {
  switch(match.arg(kind), deck = c(1142, 528), report = c(624, 384))
}

# One capture exchange per extension. `request()` asks for every chart block
# on the export and returns FALSE when there is nothing to ask (no charts, no
# service), so the caller goes straight to writing the file. `value()` is
# NULL until the whole set has landed.
#' @noRd
capture_exchange <- function(kind = "deck") {

  box <- capture_box(kind)
  tokens <- shiny::reactiveVal(NULL)

  request <- function(sects) {

    if (!requireNamespace("blockr.viz", quietly = TRUE) ||
          !length(sects$ids)) {
      return(FALSE)
    }

    # A chart that cannot be captured falls back to the server-side
    # renderer, which draws a DIFFERENT picture -- so say which chart and
    # why, rather than letting a document come back quietly mixed.
    ask <- function(bid) {
      key <- tryCatch(blockr.viz::chart_capture_for(bid),
                      error = function(e) NULL)
      if (is.null(key)) {
        return(NA_character_)
      }
      tryCatch(
        blockr.viz::chart_capture_request(key, box[[1L]], box[[2L]]),
        error = function(e) {
          cat("[export] no capture for '", bid, "': ", conditionMessage(e),
              "\n", sep = "", file = stderr())
          NA_character_
        }
      )
    }

    on_export <- vapply(
      seq_along(sects$ids),
      function(i) isTRUE(sects$report[i]) && !isTRUE(sects$pending[i]),
      logical(1L)
    )

    tok <- vapply(sects$ids[on_export], ask, character(1L))
    tok <- tok[!is.na(tok)]

    if (!length(tok)) {
      return(FALSE)
    }

    tokens(tok)
    TRUE
  }

  value <- shiny::reactive({
    tok <- tokens()
    if (is.null(tok)) {
      return(NULL)
    }
    out <- lapply(tok, blockr.viz::chart_capture_collect)
    if (any(vapply(out, is.null, logical(1L)))) {
      return(NULL)
    }
    out
  })

  list(
    request = request,
    pending = function() !is.null(shiny::isolate(tokens())),
    value = value,
    clear = function() tokens(NULL)
  )
}

# Captures written to disk and hung on the projection, keyed by block id.
# `sect_output()` reads them: a chart with a picture already drawn puts the
# picture in the document instead of the code that would redraw it.
#
# prune_sections() subsets a fixed list of per-block fields and leaves
# everything else alone, so this rides along whether it is attached before or
# after the prune.
#' @noRd
sections_with_captures <- function(sects, caps) {

  if (!length(caps)) {
    return(sects)
  }

  files <- vapply(names(caps), function(id) {
    f <- tempfile(paste0("blockr-capture-", id, "-"), fileext = ".png")
    tryCatch(blockr.viz::chart_capture_file(caps[[id]], f),
             error = function(e) NA_character_)
  }, character(1L))

  sects$captures <- as.list(files[!is.na(files)])
  sects
}
