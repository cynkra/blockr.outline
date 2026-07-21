report_pdf_available <- function() {
  nzchar(Sys.which("pdflatex")) ||
    nzchar(Sys.which("xelatex")) ||
    (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex())
}

quarto_usable <- function() {
  requireNamespace("quarto", quietly = TRUE) &&
    !is.null(quarto::quarto_path())
}

# Server-side syntax highlighting via downlit (the blockr.code approach:
# chroma classes + a local stylesheet). NULL when downlit is missing or
# the text does not parse; callers fall back to a plain code tag.
highlight_r_code <- function(txt) {

  if (!requireNamespace("downlit", quietly = TRUE)) {
    return(NULL)
  }

  res <- tryCatch(
    downlit::highlight(
      txt,
      classes = downlit::classes_chroma(),
      pre_class = "chroma"
    ),
    error = function(e) NA_character_
  )

  if (is.na(res)) NULL else res
}

# Source highlighting for the Document (quarto) view.
#
# downlit only knows R. Its downlit_md_string() is the wrong shape here:
# it RENDERS prose and drops the YAML header, whereas this view shows qmd
# SOURCE, the way an editor would. So walk the document, delegate chunk
# bodies to the same downlit pass the R script view uses, and mark up the
# markdown scaffolding around them.
#
# Emits stock chroma classes rather than new ones, so the Document view
# inherits the R view's palette and its light/dark variants for free:
# gh/gu headings, gs strong, ge emphasis, c delimiters, s inline code,
# na cross-references.
highlight_qmd_code <- function(txt) {

  if (!requireNamespace("downlit", quietly = TRUE)) {
    return(NULL)
  }

  esc <- function(x) htmltools::htmlEscape(x)

  span <- function(cls, x) {
    paste0("<span class=\"", cls, "\">", x, "</span>")
  }

  md_line <- function(x) {

    if (grepl("^#{1,6}\\s", x)) {
      return(span(if (grepl("^#\\s", x)) "gh" else "gu", esc(x)))
    }

    y <- esc(x)

    # Inline code first: its delimiters are literal after escaping, and
    # nothing inserted below contains a backtick.
    y <- gsub("(`[^`]+`)", span("s", "\\1"), y)
    y <- gsub("([*]{2}[^*]+[*]{2})", span("gs", "\\1"), y)
    y <- gsub(
      "(?<![*])([*][^*]+[*])(?![*])", span("ge", "\\1"), y, perl = TRUE
    )
    y <- gsub("(@[a-z]+-[A-Za-z0-9_-]+)", span("na", "\\1"), y)

    y
  }

  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  n <- length(lines)

  if (!n) {
    return(NULL)
  }

  out <- character(n)
  i <- 1L

  # YAML front matter: only when it opens the document.
  if (grepl("^---\\s*$", lines[[1L]])) {

    out[[1L]] <- span("c", esc(lines[[1L]]))
    i <- 2L

    while (i <= n && !grepl("^---\\s*$", lines[[i]])) {
      out[[i]] <- sub(
        "^([^:]+)(:)(.*)$",
        paste0(span("na", "\\1"), span("c", "\\2"), span("s", "\\3")),
        esc(lines[[i]])
      )
      i <- i + 1L
    }

    if (i <= n) {
      out[[i]] <- span("c", esc(lines[[i]]))
      i <- i + 1L
    }
  }

  while (i <= n) {

    if (!startsWith(lines[[i]], "```")) {
      out[[i]] <- md_line(lines[[i]])
      i <- i + 1L
      next
    }

    open <- i
    close <- open + 1L

    while (close <= n && !grepl("^```\\s*$", lines[[close]])) {
      close <- close + 1L
    }

    out[[open]] <- span("c", esc(lines[[open]]))

    if (close > open + 1L) {

      body <- lines[(open + 1L):(close - 1L)]

      hl <- tryCatch(
        downlit::highlight(
          paste(body, collapse = "\n"),
          classes = downlit::classes_chroma()
        ),
        error = function(e) NA_character_
      )

      hl <- if (is.na(hl)) NULL else strsplit(hl, "\n", fixed = TRUE)[[1L]]

      # downlit emits one span per source line; if that ever fails to
      # hold, the chunk is still readable as escaped text rather than
      # silently misaligned against its neighbours.
      out[(open + 1L):(close - 1L)] <- if (length(hl) == length(body)) {
        hl
      } else {
        esc(body)
      }
    }

    if (close <= n) {
      out[[close]] <- span("c", esc(lines[[close]]))
    }

    i <- close + 1L
  }

  paste0("<pre class=\"chroma\">", paste(out, collapse = "\n"), "</pre>")
}

# Render the qmd (quarto) or the spin script (rmarkdown fallback) into
# `file`. Errors surface as a notification plus a console trace; quiet
# rendering makes failed downloads undebuggable.
render_report <- function(qmd_txt, spin_txt, fmt, file, title,
                          template = NULL) {

  dir <- tempfile("blockr-outline-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # macOS: tempfile() returns the /var/... symlink form; quarto resolves
  # to /private/var/... and then refuses its own cleanup. Hand it the
  # resolved path.
  dir <- normalizePath(dir)

  if (!quarto_usable() && !rmarkdown::pandoc_available()) {
    msg <- paste(
      "Cannot render: neither the quarto CLI nor pandoc is available to",
      "this R session. Install quarto (quarto.org) or make pandoc visible."
    )
    showNotification(msg, type = "error", duration = 10)
    stop(msg)
  }

  render_err <- function(e) {
    msg <- paste("Report render failed:", conditionMessage(e))
    showNotification(msg, type = "error", duration = 10)
    stop(msg)
  }

  out_name <- paste0("report.", fmt)

  if (quarto_usable()) {

    # A downloaded html report is a single file, so resources (plot pngs)
    # must be embedded. The first "\n---\n" is the yaml closing fence.
    if (identical(fmt, "html")) {
      qmd_txt <- sub(
        "\n---\n",
        "\nformat:\n  html:\n    embed-resources: true\n---\n",
        qmd_txt,
        fixed = TRUE
      )
    }

    # Reference template (gear -> Template): pandoc reference-doc, which
    # pptx and docx both honour. Injected the same way as embed-resources
    # so the one Document still drives the render.
    if (!is.null(template) && nzchar(template) && fmt %in% c("pptx", "docx")) {
      qmd_txt <- sub(
        "\n---\n",
        paste0(
          "\nformat:\n  ", fmt, ":\n    reference-doc: \"",
          normalizePath(template, mustWork = FALSE), "\"\n---\n"
        ),
        qmd_txt,
        fixed = TRUE
      )
    }

    qmd <- file.path(dir, "report.qmd")
    writeLines(qmd_txt, qmd)

    tryCatch(
      quarto::quarto_render(input = qmd, output_format = fmt, quiet = FALSE),
      error = render_err
    )

  } else {

    rmd <- file.path(dir, "report.Rmd")
    writeLines(c("---", paste0("title: \"", title, "\""), "---", ""), rmd)

    spin <- knitr::spin(text = spin_txt, knit = FALSE)
    cat(spin, file = rmd, sep = "\n", append = TRUE)

    out_fmt <- switch(
      fmt,
      html = "html_document",
      pptx = "powerpoint_presentation",
      pdf = "pdf_document"
    )

    rendered <- tryCatch(
      rmarkdown::render(rmd, output_format = out_fmt, quiet = FALSE),
      error = render_err
    )

    # rmarkdown writes report.<fmt> straight into `dir`, which is already
    # the out_name path -- copying it onto itself errors. Only move it
    # when render actually put it somewhere else.
    if (!identical(
      normalizePath(rendered, mustWork = FALSE),
      normalizePath(file.path(dir, out_name), mustWork = FALSE)
    )) {
      file.copy(rendered, file.path(dir, out_name), overwrite = TRUE)
    }
  }

  if (!file.exists(file.path(dir, out_name))) {
    render_err(simpleError(
      paste0("no ", out_name, " was produced (see the R console)")
    ))
  }

  file.copy(file.path(dir, out_name), file, overwrite = TRUE)
}
