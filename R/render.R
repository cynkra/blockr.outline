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

  if (is.na(res)) NULL else strip_downlit_links(res)
}

# downlit hyperlinks every known function to its rdrr.io docs. Useful in a
# rendered document, a nuisance in the live app: the code preview sits
# inside a Shiny session, so an incidental click navigates the tab away
# and tears the app down (back button = reload = lost state). Strip the
# anchors, keep the highlighting. The rendered report is unaffected -- it
# highlights through quarto, not this path.
strip_downlit_links <- function(x) {
  gsub("</?a[^>]*>", "", x, perl = TRUE)
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

      hl <- if (is.na(hl)) {
        NULL
      } else {
        strsplit(strip_downlit_links(hl), "\n", fixed = TRUE)[[1L]]
      }

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

# Usable table width (inches) for a pptx slide: the reference template's
# body/content placeholder width, or a 16:9 widescreen fallback. Read from
# the master's body placeholder `ext cx` (EMU, 914400 = 1in). Everything is
# guarded -- a missing / unreadable template just yields the fallback, never
# an error in the render path.
template_content_width <- function(template) {
  fallback <- 12.0

  if (is.null(template) || !nzchar(template) || !file.exists(template)) {
    return(fallback)
  }

  out <- tryCatch(
    {
      dir <- tempfile("bms-tmpl-")
      dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      utils::unzip(template, exdir = dir)

      master <- file.path(dir, "ppt", "slideMasters", "slideMaster1.xml")
      if (!file.exists(master)) return(fallback)
      xml <- paste(readLines(master, warn = FALSE), collapse = "")

      # The body placeholder's shape carries `type="body"` then, further on,
      # its `<a:ext cx=.. cy=..>`. Grab the first ext after the body ph.
      body <- regmatches(
        xml,
        regexpr("type=\"body\".*?<a:ext cx=\"[0-9]+\"", xml)
      )
      cx <- regmatches(body, regexpr("cx=\"[0-9]+\"", body))
      cx <- as.numeric(gsub("\\D", "", cx))
      if (length(cx) == 1L && is.finite(cx) && cx > 0) cx / 914400 else fallback
    },
    error = function(e) fallback
  )

  if (length(out) == 1L && is.finite(out) && out > 0) out else fallback
}

# Render the qmd (quarto) or the spin script (rmarkdown fallback) into
# `file`. Errors surface as a notification plus a console trace; quiet
# rendering makes failed downloads undebuggable.
#
# pptx is the exception: pandoc positions every table / image at its own
# fixed offset and ignores the reference template's placeholder geometry, so
# a deck built through quarto cannot place a table where the slide wants it.
# The deck is built with officer instead (render_pptx_officer), which honours
# each exhibit's `pptx_left` / `pptx_top`. html / pdf / docx stay on quarto.
render_report <- function(qmd_txt, spin_txt, fmt, file, title,
                          template = NULL, sects = NULL) {

  if (identical(fmt, "pptx")) {
    return(render_pptx_officer(sects, file, title, template))
  }

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

# Build the pptx deck with officer, one reported exhibit per slide, each
# placed at its own `pptx_left` / `pptx_top` (inches) on the reference
# template. This is the topline approach: officer honours explicit
# coordinates, which pandoc's pptx writer does not. `sects` is the
# outline_sections() projection; its export code is evaluated in a fresh
# environment (reproducing the report's computation), then each reported
# block's result is turned into an exhibit through the same output
# expression the qmd uses (sect_output -> ft_table() for table blocks).
render_pptx_officer <- function(sects, file, title, template = NULL) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Rendering a pptx deck needs the 'officer' package.", call. = FALSE)
  }
  if (is.null(sects)) {
    stop("render_pptx_officer() needs the sections projection.", call. = FALSE)
  }

  render_err <- function(e) {
    msg <- paste("Deck render failed:", conditionMessage(e))
    if (exists("showNotification")) {
      try(showNotification(msg, type = "error", duration = 10), silent = TRUE)
    }
    stop(msg)
  }

  # Prune to the reported closure, exactly like the qmd / spin exporters:
  # otherwise the deck evaluates EVERY block on the board -- including
  # branches no slide depends on -- so an unrelated block that errors (or a
  # pending block whose downstream references it) aborts a deck that never
  # needed it. After pruning, only the reported exhibits and their
  # ancestors remain.
  sects <- prune_sections(sects)

  # Size tables to the slide's usable width so they do not overflow: the
  # ft_table() default reads this option.
  fit_w <- template_content_width(template)
  old <- options(blockr.viz.ft_fit_width = fit_w)
  on.exit(options(old), add = TRUE)

  # Evaluate every exported block's code once, in order, in a fresh env --
  # the same computation the report chunk runs. A block id becomes a bound
  # variable; the reported ones are the exhibits. A pending block within the
  # closure has no code to run; a reported block downstream of it will fail
  # to resolve it, which surfaces as a render error rather than a silent
  # blank -- correct, since the deck cannot show an exhibit whose inputs
  # never evaluated.
  env <- new.env(parent = globalenv())
  for (i in seq_along(sects$ids)) {
    if (isTRUE(sects$pending[i])) next
    code <- sect_export_code(sects, i)
    tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) render_err(e)
    )
  }

  doc <- tryCatch(
    if (!is.null(template) && nzchar(template) && file.exists(template)) {
      officer::read_pptx(template)
    } else {
      officer::read_pptx()
    },
    error = function(e) render_err(e)
  )

  layouts <- officer::layout_summary(doc)
  layout <- if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- layouts$master[match(layout, layouts$layout)]

  n_slides <- 0L
  for (i in seq_along(sects$ids)) {
    if (!isTRUE(sects$report[i]) || isTRUE(sects$pending[i])) next

    # The exhibit object: the SAME expression the qmd prints -- a table
    # block resolves through ft_table(), a plot / raw flextable stays as
    # itself.
    exhibit <- tryCatch(
      eval(parse(text = sect_output(sects, i)), envir = env),
      error = function(e) NULL
    )
    if (is.null(exhibit)) next

    doc <- officer::add_slide(doc, layout = layout, master = master)
    nm <- sects$names[i]
    if (is.character(nm) && length(nm) == 1L && nzchar(nm)) {
      doc <- tryCatch(
        officer::ph_with(doc, nm,
                         location = officer::ph_location_type(type = "title")),
        error = function(e) doc
      )
    }
    doc <- place_exhibit(doc, exhibit)
    n_slides <- n_slides + 1L
  }

  if (n_slides == 0L) {
    # An empty deck is still a valid file, but warn: it usually means no
    # block is in the report.
    doc <- officer::add_slide(doc, layout = layout, master = master)
    doc <- tryCatch(
      officer::ph_with(doc, title,
                       location = officer::ph_location_type(type = "title")),
      error = function(e) doc
    )
  }

  tryCatch(print(doc, target = file), error = function(e) render_err(e))
  invisible(file)
}

# ---- in-app output preview -------------------------------------------
#
# The Outline view's Output mode shows each activated (reported) block's
# EXHIBIT inline instead of its generated code -- the deck-builder view.
# It reuses the officer path's model exactly: evaluate every exported
# block's code once, in order, in a fresh env, then render each reported
# block's `sect_output()` object to HTML. What you see is therefore the
# same object that lands on the pptx slide, one abstraction earlier.
#
# Returns TAG objects (not html strings) so a flextable's own htmlwidget
# dependency rides along through renderUI; a stringified table would lose
# its styling. The map is keyed by block id, mirroring outline_code_map().
outline_output_map <- function(sects) {

  env <- new.env(parent = globalenv())
  eval_ok <- rep(TRUE, length(sects$ids))

  for (i in seq_along(sects$ids)) {
    if (isTRUE(sects$pending[i]) || !isTRUE(sects$exported[i])) {
      next
    }
    ok <- tryCatch(
      {
        eval(parse(text = sect_export_code(sects, i)), envir = env)
        TRUE
      },
      error = function(e) FALSE
    )
    eval_ok[i] <- ok
  }

  setNames(
    lapply(
      seq_along(sects$ids),
      function(i) sect_output_html(sects, env, i, eval_ok[i])
    ),
    sects$ids
  )
}

# One block's Output-mode body. A reported, evaluated block resolves its
# output expression in `env` and renders the result; everything else is a
# muted note (the offchip already states WHY a block is excluded).
sect_output_html <- function(sects, env, i, eval_ok = TRUE) {

  if (isTRUE(sects$pending[i])) {
    return(div(class = "blockr-otl-pending", "Evaluating…"))
  }

  if (!isTRUE(sects$report[i])) {
    return("")
  }

  if (!isTRUE(eval_ok)) {
    return(div(class = "blockr-otl-outnote", "Could not evaluate this block"))
  }

  exhibit <- tryCatch(
    eval(parse(text = sect_output(sects, i)), envir = env),
    error = function(e) NULL
  )

  if (is.null(exhibit)) {
    return(div(class = "blockr-otl-outnote", "No output"))
  }

  div(class = "blockr-otl-exhibit", exhibit_html(exhibit))
}

# Render one exhibit object to inline HTML tags. Mirrors place_exhibit()'s
# type dispatch: flextables (ft_table and the topline block) keep their
# styling via htmltools_value; ggplots rasterize to a data-URI img at the
# aspect ratio the block chose; a bare data frame goes through ft_table so
# the preview matches the deck; anything else prints verbatim.
exhibit_html <- function(exhibit) {

  if (inherits(exhibit, "flextable") &&
        requireNamespace("flextable", quietly = TRUE)) {
    return(
      tryCatch(
        flextable::htmltools_value(exhibit),
        error = function(e) tags$pre(paste(utils::capture.output(exhibit),
                                            collapse = "\n"))
      )
    )
  }

  if (inherits(exhibit, c("gg", "ggplot"))) {
    return(gg_exhibit_img(exhibit))
  }

  if (is.data.frame(exhibit)) {
    if (requireNamespace("blockr.viz", quietly = TRUE) &&
          requireNamespace("flextable", quietly = TRUE)) {
      ft <- tryCatch(blockr.viz::ft_table(exhibit), error = function(e) NULL)
      if (!is.null(ft)) {
        return(exhibit_html(ft))
      }
    }
    if (requireNamespace("knitr", quietly = TRUE)) {
      return(HTML(paste(
        knitr::kable(utils::head(exhibit, 50L), format = "html"),
        collapse = "\n"
      )))
    }
  }

  tags$pre(paste(utils::capture.output(print(exhibit)), collapse = "\n"))
}

# A ggplot as an inline PNG data-URI. Sizes from the block's own pptx
# geometry (gg_chart carries pptx_width / pptx_height in inches) capped to
# a sensible on-screen width, so the preview keeps the deck's proportions
# without rendering an 12in-wide canvas into a narrow panel.
gg_exhibit_img <- function(p, dpi = 96) {

  w <- coal(attr(p, "pptx_width"), 8)
  h <- coal(attr(p, "pptx_height"), 4.5)
  if (!is.finite(w) || w <= 0) w <- 8
  if (!is.finite(h) || h <= 0) h <- 4.5

  scale <- min(1, 8 / w)
  w <- w * scale
  h <- h * scale

  out <- tryCatch(
    {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)
      grDevices::png(tmp, width = w * dpi, height = h * dpi, res = dpi)
      tryCatch(print(p), finally = grDevices::dev.off())
      uri <- base64enc::dataURI(file = tmp, mime = "image/png")
      tags$img(
        class = "blockr-otl-outimg",
        src = uri,
        style = "max-width:100%;height:auto;"
      )
    },
    error = function(e) {
      div(class = "blockr-otl-outnote", "Could not render this plot")
    }
  )

  out
}

# Place one exhibit on the current slide at its intended coordinates.
# Flextables carry `pptx_left` / `pptx_top` attributes (ft_table and the
# topline flextable block both set them); ggplots and anything else fall
# back to a sensible content box.
place_exhibit <- function(doc, exhibit) {

  left <- coal(attr(exhibit, "pptx_left"), 0.4)
  top <- coal(attr(exhibit, "pptx_top"), 1.1)

  if (inherits(exhibit, "flextable")) {
    dim <- tryCatch(flextable::flextable_dim(exhibit),
                    error = function(e) list(widths = 11.9, heights = 3))
    loc <- officer::ph_location(left = left, top = top,
                                width = dim$widths, height = dim$heights)
    return(officer::ph_with(doc, exhibit, location = loc))
  }

  if (inherits(exhibit, c("gg", "ggplot"))) {
    # gg_chart() sizes the plot from the chart's row geometry and carries
    # the result as attributes; a plain ggplot takes the default box.
    loc <- officer::ph_location(
      left = left, top = top,
      width = coal(attr(exhibit, "pptx_width"), 11.9),
      height = coal(attr(exhibit, "pptx_height"), 5.5)
    )
    return(officer::ph_with(doc, exhibit, location = loc))
  }

  if (is.data.frame(exhibit)) {
    ft <- blockr.viz::ft_table(exhibit)
    return(place_exhibit(doc, ft))
  }

  # Unknown exhibit: drop it into the body placeholder as-is rather than
  # error the whole deck.
  tryCatch(
    officer::ph_with(doc, exhibit,
                     location = officer::ph_location_type(type = "body")),
    error = function(e) doc
  )
}
