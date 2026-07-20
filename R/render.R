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

# Render the qmd (quarto) or the spin script (rmarkdown fallback) into
# `file`. Errors surface as a notification plus a console trace; quiet
# rendering makes failed downloads undebuggable.
render_report <- function(qmd_txt, spin_txt, fmt, file, title) {

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

    file.copy(rendered, file.path(dir, out_name), overwrite = TRUE)
  }

  if (!file.exists(file.path(dir, out_name))) {
    render_err(simpleError(
      paste0("no ", out_name, " was produced (see the R console)")
    ))
  }

  file.copy(file.path(dir, out_name), file, overwrite = TRUE)
}
