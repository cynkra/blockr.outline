# The HTML deck.
#
# The slide builder's other export, next to render_pptx_officer(): the same
# `sects` list, the same exhibits, the same slide order, written as one
# self-contained HTML file instead of a PowerPoint.
#
# WHY THIS IS NOT QUARTO + REVEALJS, which is what it replaced:
#
#   * The look was already shared and the box was not. An exhibit renders
#     through blockr.viz::html_exhibit() either way -- the same markup, CSS
#     and script the table block's own HTML download writes -- so a deck
#     slide and a downloaded table were the same artifact until the table
#     outgrew the slide, at which point reveal clipped it and the download
#     did not. Owning the box is the whole of the difference.
#   * The deck's PowerPoint render is already in-process (officer). Writing
#     this one too takes the quarto CLI and pandoc off the deployment's
#     requirements list entirely, which for an app that ships to a server
#     nobody can install software on is the difference between a working
#     download and a broken one.
#   * 3.6MB of reveal bundle for a file whose job is to be emailed.
#
# What reveal gave that this does not: fragments, speaker notes, overview
# mode, chalkboard. None of them are used by a deck of exhibits. What it did
# NOT give, and this does: printing to PDF without a separate tool.
#
# The outline extension's own report (html / pdf / docx) is untouched and
# still goes through quarto -- it is a document, and quarto is the right tool
# for a document.

# Write `sects` to a self-contained HTML deck at `file`.
#
# `title` names the deck: it is the title slide's heading and the running
# footer on every slide after it.
render_deck_html <- function(sects, file, title) {

  render_err <- function(e) {
    msg <- paste("Deck render failed:", conditionMessage(e))
    notify_render_error(msg)
    stop(msg, call. = FALSE)
  }

  if (is.null(sects)) {
    render_err(simpleError("render_deck_html() needs the sections projection."))
  }

  # Prune to the reported closure for the reason the officer path does: a
  # branch no slide depends on must not be evaluated, let alone able to fail
  # the render.
  sects <- prune_sections(sects)

  env <- deck_eval_env(sects, render_err)

  labels <- id_labels(sects)
  slides <- list()

  for (i in slide_seq(sects)) {

    if (!isTRUE(sects$report[i]) || isTRUE(sects$pending[i])) next

    why <- NULL

    exhibit <- tryCatch(
      eval(parse(text = sect_output(sects, i)), envir = env),
      error = function(e) {
        why <<- conditionMessage(e)
        NULL
      }
    )

    # Same reporting as the deck's pptx half, and for the same reason: a
    # block that returns nothing (a base plot drawn to the device) is an
    # invisible failure, and a deck that comes back a slide short with
    # nothing anywhere to say so cannot be diagnosed on a deployment.
    if (is.null(exhibit)) {
      cat(
        "[deck] no slide for '", lab(sects$ids[i], labels), "': ",
        coal(why, paste("the block produced no exhibit (a plot drawn to the",
                        "device rather than returned?)")),
        "\n",
        sep = "", file = stderr()
      )
      next
    }

    slides[[length(slides) + 1L]] <- list(
      title = na_blank(sects$names[i]),
      body = deck_html_exhibit(exhibit)
    )
  }

  writeLines(deck_html_doc(slides, title), file)

  invisible(file)
}

# One exhibit as HTML tags.
#
# The table branch is the point of the whole file: html_exhibit() is what the
# table block's own HTML download calls, so a table on a slide and the same
# table downloaded from its block are the same markup with the same CSS.
#
# A rendered flextable arrives here whenever the projection's report call went
# through static_exhibit() outside knitr (which is where this runs), and
# static_table() leaves the frame it was built from on it -- so the HTML
# renderer gets the table rather than a screenshot of one.
deck_html_exhibit <- function(exhibit) {

  if (requireNamespace("blockr.viz", quietly = TRUE)) {

    data <- if (inherits(exhibit, "flextable")) {
      attr(exhibit, "exhibit_data")
    } else if (is.data.frame(exhibit)) {
      exhibit
    }

    if (!is.null(data)) {
      out <- tryCatch(
        blockr.viz::html_exhibit(
          data,
          # The slide's own title says it once already, two lines above.
          title = "",
          # A slide is not a scroll box with a budget: the deck's CSS gives
          # the exhibit the whole slide and scrolls it if it overruns, so the
          # renderer's own height rules have nothing to trade against.
          max_height = NULL,
          default_expanded = TRUE
        ),
        error = function(e) NULL
      )

      if (!is.null(out)) {
        return(out)
      }
    }
  }

  # Plots, widgets, gt tables, anything else: the Output preview's renderer
  # already knows how to draw each of them inline, and a slide wants exactly
  # what that preview wants.
  exhibit_html(exhibit)
}

# The document. Self-contained by construction: the stylesheet and the script
# are read out of the package and written into the file, and every dependency
# the exhibits carry is inlined the same way (see deck_inline_deps).
deck_html_doc <- function(slides, title) {

  deck_title <- if (is.character(title) && length(title) && nzchar(title[[1L]])) {
    title[[1L]]
  } else {
    "Deck"
  }

  n <- length(slides)

  body <- lapply(
    seq_along(slides),
    function(k) deck_html_slide(slides[[k]], k + 1L, n + 1L, deck_title)
  )

  rendered <- htmltools::renderTags(
    tags$div(class = "bd-stage", deck_html_title_slide(deck_title), body)
  )

  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", htmltools::htmlEscape(deck_title), "</title>"),
    paste0("<style>", deck_asset("css", "blockr-deck.css"), "</style>"),
    deck_inline_deps(rendered$dependencies),
    as.character(rendered$head),
    "</head>",
    "<body>",
    as.character(rendered$html),
    deck_html_nav(),
    paste0("<script>", deck_asset("js", "blockr-deck.js"), "</script>"),
    "</body>",
    "</html>"
  )
}

# The title and the rule under it, and nothing else -- the html half of the
# pptx title slide, which places the title into the template's "Title Slide"
# layout and leaves its subtitle box empty. A line under the title (this one
# carried the slide count) is something the deck says on every slide anyway,
# in the footer.
deck_html_title_slide <- function(title) {
  div(
    class = "bd-slide bd-slide--title",
    div(
      class = "bd-canvas",
      div(
        class = "bd-frame",
        h1(class = "bd-deck-title", title),
        div(class = "bd-rule")
      )
    )
  )
}

deck_html_slide <- function(slide, k, n, deck_title) {
  div(
    class = "bd-slide",
    div(
      class = "bd-canvas",
      div(
        class = "bd-frame",
        if (nzchar(coal(slide$title, ""))) {
          h2(class = "bd-title", slide$title)
        },
        div(class = "bd-body", slide$body)
      ),
      div(
        class = "bd-foot",
        span(deck_title),
        span(sprintf("%d / %d", k, n))
      )
    )
  )
}

# Reader chrome: two arrows, a counter and fullscreen. Outside the stage, so
# it does not scroll with the slides; hidden when printing.
deck_html_nav <- function() {
  as.character(
    div(
      class = "bd-nav",
      tags$button(class = "bd-prev", type = "button", title = "Previous slide",
                  `aria-label` = "Previous slide", HTML("&#8593;")),
      tags$button(class = "bd-next", type = "button", title = "Next slide",
                  `aria-label` = "Next slide", HTML("&#8595;")),
      span(class = "bd-count"),
      tags$button(class = "bd-full", type = "button",
                  title = "Fullscreen (f)", `aria-label` = "Fullscreen",
                  HTML("&#9974;"))
    )
  )
}

# An HTML dependency, written into the file rather than linked beside it.
#
# A deck is something you send, so a <script src="lib/..."> would be a file
# the download does not carry and a table that arrives unstyled on the machine
# that opens it. Anything that cannot be inlined (a CDN href, a missing file)
# is dropped with a note rather than emitted as a broken link.
deck_inline_deps <- function(deps) {

  if (!length(deps)) {
    return(character())
  }

  out <- character()

  for (dep in deps) {

    dir <- dep$src$file

    if (is.null(dir) || !nzchar(dir)) {
      cat("[deck] dependency '", coal(dep$name, "?"),
          "' is not a local file and was left out\n",
          sep = "", file = stderr())
      next
    }

    read_one <- function(f, tag) {
      p <- file.path(dir, f)
      if (!file.exists(p)) {
        return(NULL)
      }
      c(paste0("<", tag, ">"),
        readLines(p, warn = FALSE),
        paste0("</", tag, ">"))
    }

    for (f in coal(dep$stylesheet, character())) {
      out <- c(out, read_one(f, "style"))
    }
    for (f in coal(dep$script, character())) {
      out <- c(out, read_one(f, "script"))
    }
  }

  out
}

# A package asset as text, for inlining.
deck_asset <- function(kind, file) {
  p <- pkg_file("assets", kind, file)
  if (!nzchar(p) || !file.exists(p)) {
    return("")
  }
  paste(readLines(p, warn = FALSE), collapse = "\n")
}
