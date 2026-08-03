# Formats the outline offers to download. html and pptx, and nothing else
# until one is shown to work on the deployment that matters.
#
# pdf USED to be offered whenever pdflatex / xelatex / R's tinytex was on the
# PATH. That probe was wrong in the only place it mattered: quarto renders
# with lualatex by default and resolves TeX its own way, so the option could
# be offered on a machine where quarto can never produce a PDF -- which
# presents, from the browser, as a download that simply fails. Offering a
# format is a promise; this one could not be kept, and a probe that guesses at
# another program's toolchain was never going to keep it.
#
# The render path itself stays format-generic (render_report() takes `fmt`, and
# quarto is told the format on the command line), so restoring pdf is adding it
# to this vector -- once a deployment can be shown to render one.
#
# Named, because "revealjs" is quarto's word for it and "slides" is the user's.
# The value is what quarto is told; the name is what the picker shows.
report_formats <- function() {
  c(html = "html", slides = "revealjs", pptx = "pptx")
}

# The file extension a format produces, which is not always the format's own
# name: revealjs renders an html file (and a downloaded "report.revealjs"
# would be a file the browser refuses to open).
report_ext <- function(fmt) {
  switch(fmt, revealjs = "html", fmt)
}

# Formats that render as HTML slides. The qmd is built once and shown in the
# Document view, so it has to know: slides need explicit breaks, and a
# document must not carry them.
slide_format <- function(fmt) {
  identical(fmt, "revealjs")
}

# The scss layered over quarto's default revealjs theme: typography, the text
# greys blockr.ui's table CSS reads, and the accent taken from the chart
# palette, so a deck reads as the app rather than as reveal.js.
#
# `blockr.outline.revealjs_theme` swaps it wholesale -- a deployment with its
# own house deck styles it there, the same way `blockr.outline.template` names
# the pptx reference document. An unreadable path is dropped rather than
# failing the render: a deck in the stock theme is worth more than no deck.
revealjs_theme <- function() {

  opt <- getOption("blockr.outline.revealjs_theme", "")

  path <- if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    opt
  } else {
    pkg_file("revealjs", "blockr.scss")
  }

  if (!nzchar(path) || !file.exists(path)) {
    return("")
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

# Place the theme in the render directory and return the name to write into the
# yaml, or "" when there is no theme to place. See the call site for why the
# file has to travel rather than be pointed at.
copy_revealjs_theme <- function(dir) {

  src <- revealjs_theme()

  if (!nzchar(src)) {
    return("")
  }

  dest <- "blockr-theme.scss"

  if (!isTRUE(file.copy(src, file.path(dir, dest), overwrite = TRUE))) {
    return("")
  }

  dest
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

# One part of a pptx (which is a zip) as a single XML string, or NULL when the
# deck or the part cannot be read. Everything that introspects the reference
# template goes through here, and every caller has a fallback: a template we
# could not open must degrade to a default, never fail a render.
template_part <- function(template, part) {

  if (is.null(template) || !nzchar(template) || !file.exists(template)) {
    return(NULL)
  }

  dir <- tempfile("blockr-tmpl-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # A deck that does not carry this part is an answer, not a failure: unzip
  # warns about the file it could not find and the caller falls back.
  tryCatch(
    {
      suppressWarnings(utils::unzip(template, files = part, exdir = dir))

      f <- file.path(dir, part)
      if (!file.exists(f)) return(NULL)

      paste(readLines(f, warn = FALSE), collapse = "")
    },
    error = function(e) NULL
  )
}

# Usable table width (inches) for a pptx slide: the reference template's
# body/content placeholder width, or a 16:9 widescreen fallback. Read from
# the master's body placeholder `ext cx` (EMU, 914400 = 1in).
template_content_width <- function(template) {
  fallback <- 12.0

  xml <- template_part(template, "ppt/slideMasters/slideMaster1.xml")

  if (is.null(xml)) {
    return(fallback)
  }

  out <- tryCatch(
    {
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

# Point size of a title on this template's slides, from the master's own title
# style. `NA` when it cannot be read -- the caller then leaves the placeholder
# to inherit, which is what it did before this was asked.
#
# One number covers every title the master styles, the cover included: the BMS
# master says 24pt, which is right over a table and half the size a title page
# wants. Reading it is what lets the title slide be sized RELATIVE to the
# template instead of overruling a template that already sizes its own.
template_title_size <- function(template) {

  xml <- template_part(template, "ppt/slideMasters/slideMaster1.xml")

  if (is.null(xml)) {
    return(NA_real_)
  }

  out <- tryCatch(
    {
      style <- regmatches(xml, regexpr("<p:titleStyle>.*?</p:titleStyle>", xml))
      sz <- regmatches(style, regexpr("sz=\"[0-9]+\"", style))
      # OOXML states point sizes in hundredths.
      if (length(sz)) as.numeric(gsub("\\D", "", sz)) / 100 else NA_real_
    },
    error = function(e) NA_real_
  )

  if (length(out) == 1L && is.finite(out) && out > 0) out else NA_real_
}

# The deck's own body typeface: the theme's MINOR latin font (PowerPoint's
# "body font"; the major one is for titles). NULL when the template carries no
# readable font scheme.
#
# Why the render reads this at all. Everything the template draws itself -- the
# title placeholder, the footer -- is already set in the master's face, because
# PowerPoint resolves those against the theme. An exhibit does not: flextable
# writes an explicit `<a:latin typeface=..>` on every run, so a table lands on
# the slide in whatever font the renderer defaulted to, and it is the one thing
# on the slide that does not match its own master. That mismatch is also
# invisible where it is authored (the browser has the font; the machine that
# opens the pptx substitutes something else), which is why the deck is asked
# rather than the app.
template_body_font <- function(template) {

  xml <- template_part(template, "ppt/theme/theme1.xml")

  if (is.null(xml)) {
    return(NULL)
  }

  minor <- regmatches(
    xml,
    regexpr("<a:minorFont>.*?<a:latin typeface=\"[^\"]*\"", xml)
  )

  if (!length(minor)) {
    return(NULL)
  }

  face <- regmatches(minor, regexpr("typeface=\"[^\"]*\"", minor))
  face <- sub("\"$", "", sub("^typeface=\"", "", face))

  if (!length(face) || !nzchar(face)) NULL else face
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
# Wall-clock budget for one render, in seconds.
#
# A render evaluates the board's code in the Shiny process, so while it runs
# the process answers nothing -- and a process that stops answering gets
# terminated by the host. That is what "the download crashed the app" has
# been: not an error anywhere, just work that outlasted the host's patience.
# Observed kills landed between 3.5 and 5 minutes, so the default sits below
# the shortest of them: better a render that gives up with an explanation than
# one that takes every other session down with it.
render_timeout <- function() {
  n <- suppressWarnings(
    as.numeric(getOption("blockr.outline.render_timeout", 150))
  )
  if (!length(n) || !is.finite(n) || n <= 0) 150 else n[[1L]]
}

# Run a render so that no outcome can take the app with it.
#
# Bounded: setTimeLimit() aborts R-level work once the budget is spent, and
# the quarto CLI gets the same budget through system2(timeout=). Caught: an
# error, a timeout and an interrupt all become one condition the caller
# reports rather than something that escapes.
#
# What this cannot do is survive a signal. If the host kills the process there
# is no R left to catch anything -- which is exactly why the budget exists,
# to finish first.
with_render_guard <- function(expr) {

  limit <- render_timeout()

  setTimeLimit(elapsed = limit, transient = TRUE)
  on.exit(setTimeLimit(), add = TRUE)

  explain <- function(e) {
    msg <- conditionMessage(e)
    if (grepl("elapsed time limit|reached CPU time limit", msg)) {
      sprintf(
        paste(
          "The report did not finish within %.0f seconds and was stopped so",
          "the app stayed responsive. Reduce what the report includes, or",
          "raise blockr.outline.render_timeout."
        ),
        limit
      )
    } else {
      msg
    }
  }

  # What the renderer could not keep on one slide. blockr.viz signals one
  # `blockr_exhibit_split` message per table it had to break, carrying the
  # size that would have kept it whole; a deck of forty slides emits them
  # over the course of the render, so they are collected here and reported
  # once at the end rather than as forty notifications.
  #
  # Not muffled: the message still reaches the log, which is where the
  # per-table detail belongs.
  notes <- list()

  out <- tryCatch(
    withCallingHandlers(
      force(expr),
      blockr_exhibit_split = function(cnd) {
        notes[[length(notes) + 1L]] <<- cnd
      }
    ),
    error = function(e) {
      msg <- explain(e)
      notify_render_error(msg)
      stop(msg, call. = FALSE)
    },
    interrupt = function(e) {
      msg <- "The report render was interrupted."
      notify_render_error(msg)
      stop(msg, call. = FALSE)
    }
  )

  notify_split_tables(notes)

  out
}

# One notification for every table the export had to break, and the number
# that would have prevented it.
#
# A split table is not an error -- the deck is complete and correct -- but it
# is the thing a reader notices and cannot explain, and the warning blockr.viz
# writes to the log is a warning nobody deployed will ever see. Naming the
# tables and the size they fit at turns it into one action: lower "Smallest
# table font" in the board settings.
notify_split_tables <- function(notes) {

  if (!length(notes)) {
    return(invisible(NULL))
  }

  try(
    showNotification(split_tables_msg(notes), type = "warning", duration = 15),
    silent = TRUE
  )

  invisible(NULL)
}

# The text of that notification, apart from the showing of it: what a render
# says about the tables it broke is worth a test, and a notification is not
# testable outside a session.
split_tables_msg <- function(notes) {

  one <- function(cnd) {
    how <- c(
      if (isTRUE(cnd$pages > 1L)) paste0(cnd$pages, " slides"),
      if (isTRUE(cnd$sets > 1L)) paste0("columns over ", cnd$sets, " sets")
    )
    paste0(
      "'", coal(cnd$what, "a table"), "' (",
      paste(how, collapse = ", "),
      if (!is.null(cnd$fit_size)) paste0(", fits at ", cnd$fit_size, "pt"),
      ")"
    )
  }

  fits <- any(vapply(notes, function(n) !is.null(n$fit_size), logical(1L)))

  paste0(
    if (length(notes) == 1L) {
      "One table did not fit one slide: "
    } else {
      paste0(length(notes), " tables did not fit one slide: ")
    },
    paste(vapply(notes, one, character(1L)), collapse = ", "),
    ".",
    if (fits) {
      " Lower 'Smallest table font' in the board settings to keep them whole."
    }
  )
}

# Telling the user a render failed must never be what makes it fail. A
# download handler may run without a reactive domain, and an unguarded
# showNotification() then throws IN PLACE OF the render error, replacing the
# real cause with its own -- which is how a broken report used to reach the
# browser as a bare "an error has occurred" with nothing useful logged.
notify_render_error <- function(msg) {
  try(
    showNotification(msg, type = "error", duration = 10),
    silent = TRUE
  )
  invisible(NULL)
}

# Run `expr`, returning the error (or NULL) alongside its value. The renderer's
# own output is deliberately NOT captured: it streams straight to the app's log
# as it happens.
#
# An earlier version teed both streams to a file with sink(). That was wrong in
# production and in a way worth recording. sink() is process-wide, so for the
# length of a render the app logged NOTHING -- and a render is exactly when you
# want to watch it. Worse, if the process is killed mid-render (a Connect
# timeout, an interrupt) the sinks never unwind, the temp file goes with the
# process, and the whole window is lost: a five-minute silence followed by
# "Execution halted", which is strictly less than what was there before.
#
# It was also unnecessary. quarto's condition already carries the child
# session's failure -- rlang chains the CLI's output into the message, which is
# where "there is no package called ..." comes from -- so nothing had to be
# scraped off the stream to report it.
render_logged <- function(expr) {
  value <- NULL
  err <- tryCatch({
    value <- force(expr)
    NULL
  }, error = identity)

  list(error = err, log = character(), value = value)
}

# Where the report's R code runs.
#
#   "quarto"     (default) -- quarto executes the document, which means a
#                 FRESH R session. Clean-room and reproducible: the report
#                 depends on nothing but installed packages.
#   "in-process" -- knit here, in the app's own session, then format the
#                 result. Necessary when the board's code cannot be
#                 reproduced from installed packages alone: a package that
#                 app.R pkgload::load_all()s out of inst/ rather than
#                 installing, or a function reached through an option set at
#                 startup. Both are invisible to a fresh session, so quarto
#                 fails on documents that render perfectly in the app.
#
# Deliberately an OPTION rather than an argument on new_outline_extension():
# this is a property of the deployment (what that server can install), not of
# the report, and a constructor argument would serialise with the board and
# travel to a deployment where the answer is different.
execute_mode <- function() {
  mode <- getOption("blockr.outline.execute", "quarto")
  if (!is.character(mode) || length(mode) != 1L ||
        !mode %in% c("quarto", "in-process")) {
    warning("`blockr.outline.execute` must be \"quarto\" or \"in-process\"; ",
            "using \"quarto\".", call. = FALSE)
    return("quarto")
  }
  mode
}

# Knit the qmd in THIS session and return the markdown path. The chunk env
# mirrors render_pptx_officer(): a fresh environment whose parent is the
# global one, so board code cannot collide with our locals while options and
# load_all()'d namespaces (which live in the process, not the environment)
# stay reachable.
knit_in_process <- function(qmd, dir, fmt = "html") {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("In-process rendering needs the 'knitr' package.", call. = FALSE)
  }

  # Tell knitr what this document is being knitted TO. quarto sets this for
  # its own render session, so a chunk can ask knitr::is_html_output() and get
  # an answer; a bare knit() leaves it unset, and the exhibit renderers then
  # read "not html" and typeset an HTML report with the pptx table engine.
  old_knit <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = fmt)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old_knit), add = TRUE)

  # knitr resolves fig.path against the working directory, and the formatter
  # then resolves those links against the markdown. Both agree only from
  # inside `dir`.
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE)

  # knitr's chunk options are global to the session. Saved and restored so a
  # report cannot leave the app rendering everything else with its settings.
  old_chunk <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(old_chunk), add = TRUE)
  # error = FALSE so a failing chunk STOPS, matching quarto. knit() defaults
  # to TRUE, which would paste the error into the document and hand the user
  # a downloaded report with a traceback inside it and no other signal.
  #
  # echo follows quarto's own per-format default: shown in a document, hidden
  # in a presentation. It has to be stated because in this mode quarto never
  # executes anything -- it formats markdown knitr has already produced -- so
  # the format's execution defaults never apply, and knit()'s own `echo = TRUE`
  # stands. That put the R source on every slide of an in-process deck while a
  # quarto-CLI deck of the same document had none.
  knitr::opts_chunk$set(
    fig.path = "figure/",
    error = FALSE,
    echo = !slide_format(fmt)
  )

  md <- knitr::knit(
    input = basename(qmd),
    output = "report.md",
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )

  file.path(dir, basename(md))
}

# Run the quarto CLI directly and return its combined output, raising an error
# carrying that output when it fails.
#
# Not quarto::quarto_render(), because its error is frequently unusable: the
# package formats the failure through a cli template that references an
# undefined variable, so conditionMessage() comes back as
# "object 'captions' not found" while the actual cause ("no TeX distribution
# was found", "there is no package called ...") went to the CLI's stdout.
#
# system2() captures the CHILD's pipes. This is the distinction an earlier
# attempt got wrong by reaching for sink(): sink() diverts R's OWN streams
# process-wide, so the whole app went silent for the length of a render and a
# process killed mid-render took the window with it. Nothing here touches R's
# streams, so the app keeps logging throughout.
#
# The trade is that quarto's own progress is batched rather than streamed --
# it is re-emitted below the moment the render returns. Acceptable: the app's
# log keeps flowing either way, which is what actually matters when something
# hangs.
quarto_cli_render <- function(input, fmt) {

  bin <- quarto::quarto_path()

  if (is.null(bin) || !nzchar(bin)) {
    stop("The quarto CLI could not be located.", call. = FALSE)
  }

  # Bounded like the R-level work: setTimeLimit() cannot interrupt a blocked
  # subprocess call, so the CLI needs its own budget or a wedged quarto (a
  # LaTeX engine waiting on an interactive prompt, say) blocks the app until
  # the host loses patience.
  out <- suppressWarnings(
    system2(
      bin,
      c("render", shQuote(input), "--to", shQuote(fmt)),
      stdout = TRUE,
      stderr = TRUE,
      timeout = render_timeout()
    )
  )

  status <- attr(out, "status")
  out <- strip_ansi(out)

  if (length(out)) {
    message(paste(out, collapse = "\n"))
  }

  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    stop(
      paste(c(sprintf("quarto exited with status %s.", status),
              quarto_cli_detail(out)),
            collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(input)
}

# quarto colours its output; the codes are noise in a notification and in a log.
strip_ansi <- function(x) {
  gsub("\033\\[[0-9;]*[A-Za-z]", "", x, perl = TRUE)
}

# The useful tail of a failed render. quarto echoes the resolved document
# metadata before it gets to work, so a plain tail() is mostly YAML: the
# no-TeX failure buries "No TeX installation was detected" under nine lines of
# documentclass and papersize. Start at the first line that reads like a
# diagnosis when there is one, and fall back to the tail when nothing matches.
QUARTO_ERROR_HINT <- paste(
  "error", "warn", "not found", "no tex", "cannot", "failed", "unable",
  "^!",
  sep = "|"
)

quarto_cli_detail <- function(out, n = 15L) {
  lines <- out[nzchar(trimws(out))]

  if (!length(lines)) {
    return(character())
  }

  hit <- grep(QUARTO_ERROR_HINT, lines, ignore.case = TRUE, perl = TRUE)

  if (length(hit)) {
    lines <- lines[seq(min(hit), length(lines))]
  }

  utils::tail(lines, n)
}

# Format already-executed markdown. No chunks survive knitting, so neither
# quarto nor pandoc starts an R session here -- which is the whole point.
format_markdown <- function(md, fmt, dir) {
  if (quarto_usable()) {
    return(quarto_cli_render(md, fmt))
  }
  rmarkdown::pandoc_convert(
    input = normalizePath(md),
    to = if (identical(fmt, "html")) "html" else fmt,
    output = file.path(dir, paste0("report.", fmt)),
    options = c("--standalone", if (identical(fmt, "html")) "--embed-resources")
  )
}

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
    notify_render_error(msg)
    stop(msg)
  }

  render_err <- function(e) {
    # quarto chains the document session's failure into its own condition, so
    # conditionMessage() already carries the useful part ("there is no package
    # called ..."). What used to hide it was the notification below throwing
    # first; nothing needs to be scraped off the output stream.
    msg <- paste("Report render failed:", conditionMessage(e))
    notify_render_error(msg)
    stop(msg, call. = FALSE)
  }

  out_name <- paste0("report.", report_ext(fmt))

  in_process <- identical(execute_mode(), "in-process")

  if (quarto_usable() || in_process) {

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

    # Slides. embed-resources for the same reason as html (one file to walk
    # away with, reveal.js, the theme and all). `scrollable` keeps a long table
    # on its slide instead of letting it run off the bottom, and the 1200x800
    # canvas is 3:2, the shape the chart exhibits are drawn for.
    #
    # No `smaller`: it applies a blanket scale to the slide's contents, which
    # is a blunt instrument once the theme sets sizes per element. Sizes belong
    # in one place, and that place is the scss.
    if (identical(fmt, "revealjs")) {
      # Copied next to the qmd and named by basename: quarto resolves a theme
      # that is not one of its built-ins against the DOCUMENT's directory (an
      # absolute path lands under quarto's own themes dir, with `.scss`
      # appended, and the render dies on a file that was never there).
      scss <- copy_revealjs_theme(dir)
      qmd_txt <- sub(
        "\n---\n",
        paste(
          c(
            "",
            "format:",
            "  revealjs:",
            "    embed-resources: true",
            "    scrollable: true",
            "    width: 1200",
            "    height: 800",
            # A running head. The deck's own title on every slide is what a
            # deck that leaves the room needs: a slide screenshotted into an
            # email still says which study it came from.
            paste0("    footer: \"", yaml_dq(title), "\""),
            # Omitted rather than emitted empty: `theme: [default, ""]` is a
            # yaml error, and no theme is a working deck.
            if (nzchar(scss)) {
              paste0("    theme: [default, ", scss, "]")
            },
            "---",
            ""
          ),
          collapse = "\n"
        ),
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

    if (in_process) {
      # Two steps instead of one, and the same document either way: knit the
      # qmd HERE so the board's code sees this session, then hand the
      # already-executed markdown to the formatter, which runs no R at all.
      res <- render_logged(knit_in_process(qmd, dir, fmt))
      if (!is.null(res$error)) {
        render_err(res$error)
      }

      res <- render_logged(format_markdown(res$value, fmt, dir))
    } else {
      res <- render_logged(
        quarto_cli_render(qmd, fmt)
      )
    }

    if (!is.null(res$error)) {
      render_err(res$error)
    }

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

    res <- render_logged(
      rmarkdown::render(rmd, output_format = out_fmt, quiet = FALSE)
    )
    if (!is.null(res$error)) {
      render_err(res$error)
    }
    rendered <- res$value

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

# Empty a reference deck of its own slides, keeping masters, layouts and the
# theme (which is where the fonts and the slide size live).
#
# A pandoc reference document carries EXAMPLE slides -- "Presentation Title",
# "Hello, world.", a two-content demo -- because that is how pandoc learns the
# styles; it renders the layouts and never emits the examples. officer's
# read_pptx() has no such notion: it OPENS the deck, so every exhibit is
# appended after the examples and the download starts with four junk slides.
# Stripping them makes "reference document" mean the same thing on both render
# paths (quarto for html/docx, officer for pptx). Best effort: an officer
# version without remove_slide(), or a deck it refuses to shrink, leaves the
# slides in place rather than failing the render.
strip_slides <- function(doc) {
  tryCatch(
    {
      for (i in rev(seq_along(doc))) {
        doc <- officer::remove_slide(doc, i)
      }
      doc
    },
    error = function(e) doc
  )
}

# Build the pptx deck with officer, one reported exhibit per slide, each
# placed at its own `pptx_left` / `pptx_top` (inches) on the reference
# template. This is the topline approach: officer honours explicit
# coordinates, which pandoc's pptx writer does not. `sects` is the
# outline_sections() projection; its export code is evaluated in a fresh
# environment (reproducing the report's computation), then each reported
# block's result is turned into an exhibit through the same output
# expression the qmd uses (sect_output -> static_table() for table blocks).
# Evaluate every exported block's code once, in order, in a fresh env: a
# block id becomes a bound variable, and the reported ones are the exhibits.
#
# Shared by the deck's two writers, which agree about what a slide's exhibit
# IS and differ only in what they draw it into. A pending block within the
# closure has no code to run; a reported block downstream of it fails to
# resolve it, which surfaces as a render error rather than a silent blank --
# correct, since no deck can show an exhibit whose inputs never evaluated.
deck_eval_env <- function(sects, render_err) {

  env <- eval_env(sects)

  for (i in seq_along(sects$ids)) {
    if (isTRUE(sects$pending[i])) next
    code <- sect_export_code(sects, i)
    tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) {
        seed_failed(env, sects$ids[i], conditionMessage(e), id_labels(sects))
        render_err(e)
      }
    )
  }

  env
}

render_pptx_officer <- function(sects, file, title, template = NULL,
                                title_slide = TRUE) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Rendering a pptx deck needs the 'officer' package.", call. = FALSE)
  }
  if (is.null(sects)) {
    stop("render_pptx_officer() needs the sections projection.", call. = FALSE)
  }

  render_err <- function(e) {
    msg <- paste("Deck render failed:", conditionMessage(e))
    notify_render_error(msg)
    stop(msg, call. = FALSE)
  }

  # Prune to the reported closure, exactly like the qmd / spin exporters:
  # otherwise the deck evaluates EVERY block on the board -- including
  # branches no slide depends on -- so an unrelated block that errors (or a
  # pending block whose downstream references it) aborts a deck that never
  # needed it. After pruning, only the reported exhibits and their
  # ancestors remain.
  sects <- prune_sections(sects)

  # Size tables to the slide's usable width so they do not overflow: the
  # static_table() default reads this option.
  fit_w <- template_content_width(template)

  # ... and set them in the deck's own face, so the table matches the title
  # above it. Only when nothing else has: a theme that names an exhibit font
  # (`exhibits = list(ft_font = ..)`) has said what it wants, and the app is
  # then the more specific answer than the file.
  font <- if (is.null(getOption("blockr.viz.ft_font"))) {
    template_body_font(template)
  }

  old <- options(
    c(
      list(blockr.viz.ft_fit_width = fit_w),
      if (!is.null(font)) list(blockr.viz.ft_font = font)
    )
  )
  on.exit(options(old), add = TRUE)

  # Evaluate every exported block's code once, in order, in a fresh env --
  # the same computation the report chunk runs. A block id becomes a bound
  # variable; the reported ones are the exhibits. A pending block within the
  # closure has no code to run; a reported block downstream of it will fail
  # to resolve it, which surfaces as a render error rather than a silent
  # blank -- correct, since the deck cannot show an exhibit whose inputs
  # never evaluated.
  env <- deck_eval_env(sects, render_err)

  # Which deck this render is styling against, said out loud.
  #
  # A missing or unset template is not an error: officer falls back to its own
  # stock deck and the download succeeds, so "the house template was ignored"
  # and "the house template was applied" produce the same outcome apart from
  # the fonts. That is not something to diagnose by squinting at a slide,
  # especially on a deployment nobody can open a shell on.
  # No template named: the BUNDLED widescreen deck, not officer's stock one.
  #
  # officer's own is 10x7.5in (4:3) while every exhibit sizes itself to ~12in
  # of widescreen content -- so an unconfigured deck laid widescreen figures
  # onto a 4:3 slide and ran them off the right edge. The extension already
  # resolved this through effective_template(); a caller reaching this
  # function directly (a script, a test, another package) got the stock deck
  # and a different-looking export, which is the sort of difference nobody
  # traces back to a default.
  if (is.null(template) || !nzchar(coal(template, ""))) {
    template <- coal(default_template(), "")
  }

  usable <- !is.null(template) && nzchar(template) && file.exists(template)

  cat(
    "[deck] ",
    if (usable) {
      paste0("template: ", template)
    } else if (is.null(template) || !nzchar(template)) {
      "no template set -- officer stock deck"
    } else {
      paste0("template MISSING: ", template, " -- officer stock deck")
    },
    " | exhibit font: ",
    coal(getOption("blockr.viz.ft_font"), "renderer default"),
    "\n",
    sep = "", file = stderr()
  )

  doc <- tryCatch(
    if (usable) {
      strip_slides(officer::read_pptx(template))
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

  # The deck opens on its title, the way the html deck does. The template's
  # own "Title Slide" layout is what says how that looks -- the centred title
  # placeholder, the rule under it, the house colours -- so this places text
  # into it rather than drawing anything.
  n_title <- 0L
  if (isTRUE(title_slide)) {
    doc <- deck_add_title_slide(doc, title, layouts, layout, master, template)
    n_title <- 1L
  }

  # Pass 2 walks the SLIDE order, which the pass above could not: evaluation
  # has to follow the DAG, the deck does not. For an outline projection the
  # two are the same sequence (slide_seq falls back to document order); a
  # slide-builder projection carries the order the user dragged the list to.
  n_slides <- 0L
  for (i in slide_seq(sects)) {
    if (!isTRUE(sects$report[i]) || isTRUE(sects$pending[i])) next

    # The exhibit object: the SAME expression the qmd prints -- a table-shaped
    # result resolves through static_exhibit() (whatever block produced it), a
    # plot / raw flextable stays as itself.
    # Say which slide went missing and why.
    #
    # A block whose exhibit cannot be built is skipped rather than failing
    # the whole deck -- one broken exhibit should not cost the other nine
    # slides. But skipping SILENTLY means a deck comes back short with
    # nothing anywhere to say so, which on a deployment nobody can open a
    # shell on is indistinguishable from never having picked the block.
    #
    # Both halves need saying, and the quiet one is the one that bites: a
    # block that THROWS is at least a visible failure elsewhere in the app,
    # while a block that returns NULL looks entirely healthy. blockr.core's
    # plot blocks draw to the graphics device with base graphics and return
    # nothing, so they have no exhibit to place and vanish from a deck
    # without a single error anywhere.
    why <- NULL

    exhibit <- tryCatch(
      eval(parse(text = sect_output(sects, i)), envir = env),
      error = function(e) {
        why <<- conditionMessage(e)
        NULL
      }
    )

    if (is.null(exhibit)) {
      cat(
        "[deck] no slide for '", lab(sects$ids[i], id_labels(sects)), "': ",
        coal(why, "the block produced no exhibit (a plot drawn to the device rather than returned?)"),
        "\n",
        sep = "", file = stderr()
      )
      next
    }

    nm <- sects$names[i]
    nm <- if (is.character(nm) && length(nm) == 1L && nzchar(nm)) nm

    # A table gets as many slides as it needs. blockr.viz owns that
    # arithmetic -- measured column widths, the font step-down, where to
    # break the rows, the repeated header band, the "(2 of 8)" page titles --
    # and owns it for the block's own PowerPoint download too, so a table
    # exported from a block and the same table on a deck slide come out
    # identical. Before this the deck placed one flextable per slide and a
    # long table simply ran off the bottom.
    #
    # It needs the frame, not the rendering; static_table() stashes it on the
    # flextable, so an exhibit that arrives already rendered still pages.
    # Everything else -- plots, widgets, an unknown object -- keeps the
    # single-slide placement below.
    n_paged <- deck_add_table(doc, exhibit, nm, layout, master, template)

    if (!is.null(n_paged)) {
      doc <- n_paged$doc
      n_slides <- n_slides + n_paged$n
      next
    }

    doc <- officer::add_slide(doc, layout = layout, master = master)
    if (!is.null(nm)) {
      doc <- tryCatch(
        officer::ph_with(doc, nm,
                         location = officer::ph_location_type(type = "title")),
        error = function(e) doc
      )
    }
    doc <- place_exhibit(doc, exhibit)
    n_slides <- n_slides + 1L
  }

  if (n_slides == 0L && n_title == 0L) {
    # An empty deck is still a valid file, but warn: it usually means no
    # block is in the report. With a title slide there is already one.
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
#
# `links` is the board's link table. It is not needed to EVALUATE anything;
# it is what turns "no applicable method for 'filter' applied to an object
# of class \"function\"" into "nothing is connected to this block", which is
# the sentence the reader can act on.
outline_output_map <- function(sects, board_ids = character(), links = NULL) {

  # Ids are what the code says; NAMES are what the board shows. A note that
  # says only `hxjsagdg` is a lookup the reader cannot perform -- the id
  # appears nowhere in the UI.
  labs <- id_labels(sects)

  env <- eval_env(sects, board_ids, labs)

  # A block nothing feeds. Its chunk still names its input slot (`data`),
  # which no seed covers because no block declares it, so the raw condition
  # is about whatever function shares that name.
  fed <- if (is.null(links)) {
    rep(NA, length(sects$ids))
  } else {
    sects$ids %in% links$to
  }

  # NULL = evaluated cleanly; a list(head, msg) = the diagnosis. Kept rather
  # than a bare flag because the failure is almost never in the block you are
  # looking at: an ANCESTOR outside the report (its own row is not even
  # listed) throws, its variable never binds, and every dependent dies naming
  # a variable, not a block.
  notes <- vector("list", length(sects$ids))

  # What each id ended up holding, filled in as chunks succeed. A failing
  # chunk reports the CLASS of every name it reads, which is the one fact no
  # amount of message-wording gets at: an upstream that evaluated to
  # something unusable (a function, NULL) looks identical, from the error,
  # to one that never ran at all.
  held <- character()

  for (i in seq_along(sects$ids)) {
    if (isTRUE(sects$pending[i]) || !isTRUE(sects$exported[i])) {
      next
    }
    note <- tryCatch(
      {
        eval(parse(text = sect_export_code(sects, i)), envir = env)
        held[[sects$ids[i]]] <- paste(
          class(get0(sects$ids[i], envir = env, ifnotfound = NULL)),
          collapse = "/"
        )
        NULL
      },
      error = function(e) {
        note <- diagnose_chunk(conditionMessage(e),
                               sect_export_code(sects, i), env, fed[i], held,
                               labs)
        # Re-poison: the id stays unbound, so say WHY when a dependent
        # touches it, rather than letting it fall through to the search
        # path (see eval_env).
        seed_failed(env, sects$ids[i], note$msg, labs)
        note
      }
    )

    # Guarded: `notes[[i]] <- NULL` DELETES the slot rather than clearing
    # it, and every later index would then be off by one.
    if (!is.null(note)) {
      notes[[i]] <- note
    }
  }

  setNames(
    lapply(
      seq_along(sects$ids),
      function(i) sect_output_html(sects, env, i, notes[[i]])
    ),
    sects$ids
  )
}

# One block's Output-mode body. A reported, evaluated block resolves its
# output expression in `env` and renders the result; everything else is a
# muted note (the offchip already states WHY a block is excluded).
sect_output_html <- function(sects, env, i, note = NULL) {

  if (isTRUE(sects$pending[i])) {
    return(div(class = "blockr-otl-pending", "Evaluating\u2026"))
  }

  if (!isTRUE(sects$report[i])) {
    return("")
  }

  if (!is.null(note)) {
    return(outnote(note$head, note$msg))
  }

  # Two distinct failures, told apart: the transform ran but produced
  # nothing, or the EXHIBIT call (report_call / static_table wrapper) threw
  # -- a chart whose mapping names a column the data no longer has lands
  # here, and "No output" would read as an empty block.
  err <- NULL
  warn <- character()

  exhibit <- withCallingHandlers(
    tryCatch(
      eval(parse(text = sect_output(sects, i)), envir = env),
      error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }
    ),
    # A renderer that DEGRADES rather than fails is the quieter half of the
    # problem: static_chart() warns and hands back the chart's data when it
    # cannot draw the requested type from the block's state, so the preview
    # shows a table where a chart belongs and nothing says why. Carry the
    # warning under the exhibit.
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.null(err)) {
    return(outnote("Could not render this exhibit", err))
  }

  if (is.null(exhibit)) {
    return(div(class = "blockr-otl-outnote", "No output"))
  }

  div(
    class = "blockr-otl-exhibit",
    exhibit_html(exhibit),
    if (length(warn)) {
      div(class = "blockr-otl-outnote",
          tags$div(class = "blockr-otl-outerr",
                   paste(unique(warn), collapse = "\n")))
    }
  )
}

# The environment every block's chunk evaluates in, with each block id
# pre-bound to a promise that ERRORS when something reads it.
#
# The env chains to globalenv() -- it must, the code calls package
# functions -- so an id that never binds resolves UP THE SEARCH PATH
# instead of failing. A block called `data`, `filter` or `sub` silently
# becomes the base function of that name, and its dependent dies with
# "no applicable method for 'filter' applied to an object of class
# \"function\"": an error that names neither the block that never ran nor
# anything a user could act on. (Ids are usually random strings, where the
# same hole reads as the more honest "object 'xyz' not found" -- but a
# board that names its source block `data` is the normal case, not a
# corner one.)
#
# A local binding always beats the search path, so seeding every id closes
# the hole for good: reading an unevaluated block reports the block. Each
# successful chunk overwrites its own promise with the real value.
#
# `board_ids` covers the second way an id goes missing: a block whose
# expression is NULL this flush is DROPPED from the projection altogether
# (see the narrowing in outline_sections), while its dependents' chunks
# still name it. It has no row, no code and no section, so seeding
# sects$ids alone leaves that name to the search path.
eval_env <- function(sects, board_ids = character(), labs = character()) {

  env <- new.env(parent = globalenv())

  for (i in seq_along(sects$ids)) {
    seed_unbound(
      env, sects$ids[i], labs,
      if (isTRUE(sects$pending[i])) {
        "has not finished building yet"
      } else if (!isTRUE(sects$exported[i])) {
        "is not part of the report"
      } else {
        "did not evaluate"
      }
    )
  }

  for (id in setdiff(board_ids, sects$ids)) {
    seed_unbound(env, id, labs, "is not reporting any code right now")
  }

  env
}

# One failed chunk, as a headline plus a detail. The headline is the
# sentence to act on; the R condition rides underneath it, because on a
# deployment it is the only forensic trace anyone gets.
#
# Three diagnoses, in the order they are worth reading:
#
#   nothing is connected     the chunk names its input SLOT (`data`), which
#                            no block declares, because the block has no
#                            incoming link. This is a wiring problem, and
#                            the raw condition ("no applicable method for
#                            'filter' applied to an object of class
#                            \"function\"") is about utils::data, which has
#                            nothing to do with anything.
#   an upstream block failed the seeded promise already names the block and
#                            its root cause (see eval_env).
#   everything else          the chunk's own error, with any loose names
#                            listed: a removed block still named by a
#                            dependent lands here.
diagnose_chunk <- function(msg, code, env, fed = NA, held = character(),
                           labs = character()) {

  reads <- chunk_reads(code, held)

  # An upstream that holds a FUNCTION is not a strange coincidence, it is
  # this same bug one hop up: a pass-through block with nothing connected
  # evaluates its own input slot (`id <- data`), binds utils::data, and
  # SUCCEEDS. It gets no note of its own -- it did not fail -- so the first
  # visible symptom is here, in a dependent, as an error about a function
  # nobody wrote. Name the block that actually needs rewiring.
  fns <- names(reads)[reads == "function"]

  if (length(fns)) {
    who <- paste(vapply(fns, lab, character(1L), labs = labs), collapse = ", ")
    return(list(
      head = "An upstream block produced a function, not data",
      msg = paste0(
        who, " holds a function, which is what a block reads when nothing ",
        "is connected to it: its own code fell through to a function of the ",
        "same name. Connect a block to it, or take it out of the report.\n",
        msg, reads_line(reads)
      )
    ))
  }

  # Already names a block and its root cause. Appending a list of names to
  # that only buries the sentence that matters.
  if (grepl("^upstream block ", msg)) {
    return(list(head = "An upstream block could not be evaluated", msg = msg))
  }

  loose <- loose_names(code, env)
  msg <- paste0(msg, reads_line(reads))

  if (length(loose) && isFALSE(fed)) {
    return(list(
      head = "Nothing is connected to this block",
      msg = paste0(
        "its code reads ", fmt_names(loose),
        ", the input slot no block fills. Connect a block to it, or take ",
        "it out of the report.\n", msg
      )
    ))
  }

  list(
    head = "Could not evaluate this block",
    msg = if (length(loose)) {
      paste0(msg, "\n(this chunk reads ", fmt_names(loose),
             ", which no block on the board binds)")
    } else {
      msg
    }
  )
}

# What the block ids a chunk reads actually hold, once its own assignment
# target is dropped. "hxjsagdg = function" ends an argument that no wording
# can settle: a block that evaluated to something unusable and a block that
# never ran produce the same downstream error, and only the class tells them
# apart. Empty when the chunk reads no id that has a value yet, which is
# itself the answer.
chunk_reads <- function(code, held) {

  free <- tryCatch(all.vars(parse(text = code)), error = function(e) NULL)

  # The target is assigned BY this chunk, so it is not something it reads.
  tgt <- tryCatch(all.vars(parse(text = code)[[1L]][[2L]]),
                  error = function(e) NULL)

  held[intersect(setdiff(free, tgt), names(held))]
}

reads_line <- function(reads) {

  if (!length(reads)) {
    return("")
  }

  reads <- utils::head(reads, 5L)

  paste0("\nreads ",
         paste0(names(reads), " = ", reads, collapse = ", "))
}

# The names a chunk reads that hold no value here. Seeding covers every id
# the projection or the board knows; a chunk can still name something
# NEITHER knows -- a removed block, an input slot that was never substituted
# -- and such a name resolves up the search path to whatever function
# happens to share it.
#
# Only names that are absent or resolve to a FUNCTION count: a data frame or
# a pronoun reached from globalenv() is not this failure mode. NSE symbols
# (column names in a dplyr verb) can slip in; on a chunk that already
# failed, a short over-broad list beats none.
loose_names <- function(code, env) {

  free <- tryCatch(all.vars(parse(text = code)), error = function(e) NULL)

  # ls(env) holds every seeded id, so anything left is a name no seed
  # covers. Never get0() a seeded id: forcing the promise re-throws.
  free <- setdiff(free, ls(env, all.names = TRUE))

  # FUNCTIONS only. An absent name is usually an NSE symbol (a column inside
  # a dplyr verb) and listing those buries the one that matters, while a
  # genuinely absent BLOCK is already named by R's own "object 'xyz' not
  # found". A name that resolves to a function IS the shadowing hazard.
  Filter(
    function(v) is.function(get0(v, envir = env, ifnotfound = NULL)),
    free
  )
}

fmt_names <- function(x) {
  paste0("`", utils::head(x, 5L), "`", collapse = ", ")
}

# One unbound block id, as a promise that stops with `why` when forced.
seed_unbound <- function(env, id, labs, why) {
  poison(env, id, paste0("upstream block ", lab(id, labs), " ", why))
}

# A block, as the reader can find it: its NAME, with the id it goes by in
# the code. Falls back to the bare id for a block the projection does not
# carry a name for (one dropped from it, say).
lab <- function(id, labs = character()) {

  nm <- if (id %in% names(labs)) labs[[id]] else NA_character_

  if (!is.na(nm) && nzchar(nm) && !identical(nm, id)) {
    paste0("`", nm, "` (", id, ")")
  } else {
    paste0("`", id, "`")
  }
}

id_labels <- function(sects) {
  nms <- sects$names
  if (is.null(nms)) {
    return(stats::setNames(character(), character()))
  }
  stats::setNames(as.character(nms), sects$ids)
}

# The failure a block hands its dependents. A message that ALREADY names an
# upstream block passes through untouched: down a four-block chain, nesting
# one inside the next would bury the root cause under three wrappers, and
# the root is the only thing to act on.
seed_failed <- function(env, id, msg, labs = character()) {
  poison(
    env, id,
    if (grepl("^upstream block ", msg)) {
      msg
    } else {
      paste0("upstream block ", lab(id, labs), " failed to evaluate: ", msg)
    }
  )
}

poison <- function(env, id, msg) {
  # force() before the promise: `msg` is itself a promise reaching back into
  # the caller's loop frame, and the binding below is only read LATER -- by
  # which time the loop variable has moved on and every block would report
  # the last one's reason.
  force(id)
  force(msg)
  delayedAssign(id, stop(msg, call. = FALSE), eval.env = environment(),
                assign.env = env)
  invisible(NULL)
}

# A preview failure note: the headline plus the R condition verbatim. The
# message is the whole point (see eval_err in outline_output_map), so it is
# shown, not logged -- the outline commonly runs on a deployment where
# nobody can read the console.
outnote <- function(headline, msg) {
  div(
    class = "blockr-otl-outnote",
    headline,
    tags$div(class = "blockr-otl-outerr", msg),
    # The version that WROTE this note. A failure note is the one thing that
    # reliably gets screenshotted off a deployment, and "is this build the
    # one I just pushed" was, for three rounds of a real debug, unanswerable
    # from the note alone.
    tags$div(class = "blockr-otl-outver", paste("blockr.outline",
                                                pkg_version()))
  )
}

# Render one exhibit object to inline HTML tags. Mirrors place_exhibit()'s
# type dispatch: flextables (static_table and the topline block) keep their
# styling via htmltools_value; ggplots rasterize to a data-URI img at the
# aspect ratio the block chose; a bare data frame goes through static_table so
# the preview matches the deck; anything else prints verbatim. The data-frame
# branch is a backstop -- the emitted output line already wraps table-shaped
# results in static_exhibit() -- and it keeps a directly-supplied exhibit
# (blockr.viz absent from the chunk) rendering as a table.
exhibit_html <- function(exhibit) {

  if (inherits(exhibit, "flextable") &&
        requireNamespace("flextable", quietly = TRUE)) {
    return(
      tryCatch(
        flextable::htmltools_value(exhibit),
        error = function(e) {
          tags$pre(paste(utils::capture.output(exhibit), collapse = "\n"))
        }
      )
    )
  }

  if (inherits(exhibit, c("gg", "ggplot"))) {
    return(gg_exhibit_img(exhibit))
  }

  if (is.data.frame(exhibit)) {
    if (requireNamespace("blockr.viz", quietly = TRUE) &&
          requireNamespace("flextable", quietly = TRUE)) {
      ft <- tryCatch(blockr.viz::static_table(exhibit), error = function(e) NULL)
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
# geometry (static_chart carries pptx_width / pptx_height in inches) capped to
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

# The deck's opening slide: its title, and nothing else.
#
# Placed into the template's "Title Slide" layout, whose placeholder is
# `ctrTitle` rather than the `title` every other slide uses -- that layout is
# where a house template says what a title page looks like, and using it is
# the difference between a deck that opens like the rest of the deck and one
# that opens on a content slide with a heading. This is exactly what pandoc
# writes from a document's `title` (blockr.md's decks), placeholder for
# placeholder.
#
# NOTHING GOES IN THE SUBTITLE. A date under the title read as orphaned on
# the BMS master: its title box sits a third of the way down and left, its
# subtitle box more than half way down and CENTRED, so a short line under a
# long title lands in the middle of the slide with nothing around it. The
# layout is stock Office geometry with house chrome on the master, not a
# designed title page, and a deck built by an app has nothing to say there
# that the title has not said.
#
# Every placement is guarded: a template may name its layouts something else
# entirely (a translated master), and a title slide is never worth failing a
# download over. The fallbacks walk down: the title layout, then the content
# one, then the plain title placeholder, then a slide with nothing on it.
deck_add_title_slide <- function(doc, title, layouts, layout, master,
                                 template = NULL) {

  title <- if (is.character(title) && length(title) && nzchar(title[[1L]])) {
    title[[1L]]
  } else {
    "Deck"
  }

  use <- if ("Title Slide" %in% layouts$layout) "Title Slide" else layout
  use_master <- layouts$master[match(use, layouts$layout)]

  doc <- tryCatch(
    officer::add_slide(doc, layout = use, master = coal(use_master, master)),
    error = function(e) {
      officer::add_slide(doc, layout = layout, master = master)
    }
  )

  # A cover title is not a slide heading. The master styles ALL its titles at
  # one size (24pt on the BMS deck, which is right over a table), and a title
  # page set at that size reads as a slide that lost its content. So the size
  # is raised for this one slide -- and only raised: a template whose own
  # titles are already larger keeps them.
  # `ctrTitle` is the centred title of a title layout; `title` is what every
  # other layout calls the same thing. Whichever the slide has takes the text,
  # and a slide with neither still exists rather than erroring.
  for (type in c("ctrTitle", "title")) {
    placed <- tryCatch(
      {
        doc <- officer::ph_with(
          doc, title, location = officer::ph_location_type(type = type)
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (isTRUE(placed)) break
  }

  deck_set_title_size(doc, deck_title_size(doc, use, title, template))
}

# How big the cover title is set, in points.
#
# A cover title is not a slide heading, and a master states ONE size for all
# of its titles -- 24pt on the BMS deck, which is right over a table and half
# of what a title page wants. So the size is raised for this one slide, and
# only raised: a template whose own titles are already larger keeps them.
#
# Then stepped back down until it fits the placeholder. A deck called "Adverse
# events by system organ class and preferred term" at 40pt is four lines in a
# box that holds two, and PowerPoint does not shrink text it was handed rather
# than typed. Estimated, not measured: half the point size per character is
# the usual rule of thumb for a proportional face, and being a size or two
# conservative on a long title costs nothing a reader will notice.
deck_title_size <- function(doc, layout, title, template = NULL,
                            floor = 24) {

  own <- template_title_size(template)
  want <- max(
    getOption("blockr.outline.deck_title_size", 40),
    if (is.finite(own)) own else 0
  )

  box <- tryCatch(
    {
      ph <- officer::layout_properties(doc, layout = layout)
      ph <- ph[ph$type %in% c("ctrTitle", "title"), ]
      if (!nrow(ph)) NULL else c(ph$cx[[1L]], ph$cy[[1L]]) * 72
    },
    error = function(e) NULL
  )

  if (is.null(box) || any(!is.finite(box)) || any(box <= 0)) {
    return(want)
  }

  n <- max(1L, nchar(title))

  for (size in seq(want, floor, by = -2)) {
    lines <- max(1, ceiling(n * 0.5 * size / box[[1L]]))
    if (lines * size * 1.2 <= box[[2L]]) {
      return(size)
    }
  }

  floor
}

# Set the point size of the text just placed on the current slide.
#
# By patching the run's own `<a:rPr>` rather than handing officer a formatted
# paragraph. `fp_text()` would carry its defaults -- Arial, black, 10pt --
# onto the run and take the title out of the template's face and colour to
# change one number, and `fp_text_lite()`, which exists to set one property
# and inherit the rest, writes nothing at all through officer's pptx path
# (0.7.3). The empty `<a:rPr/>` that `ph_with()` leaves is exactly the hook:
# one attribute on it, everything else still inherited from the layout.
#
# Guarded end to end. It reaches into the document's slide object, which is
# not officer's public surface, and a title at the master's own size is a
# perfectly good slide -- not something to fail a download over.
deck_set_title_size <- function(doc, size) {

  tryCatch(
    {
      sld <- doc$slide$get_slide(doc$cursor)$get()
      # The title slide holds one shape carrying one run, so every run
      # property on it belongs to the title.
      for (rpr in xml2::xml_find_all(sld, ".//a:rPr")) {
        xml2::xml_set_attr(rpr, "sz", format(round(size * 100)))
      }
      doc
    },
    error = function(e) doc
  )
}

# One block's table, paged over as many slides as it needs by blockr.viz's
# own paginator -- the one the table block's PowerPoint button uses, so the
# two exports agree slide for slide.
#
# Returns NULL for anything it will not page, which is the caller's signal to
# fall back to the single-slide placement: a plot, a widget, a hand-built
# flextable carrying no source frame, a blockr.viz too old to export the
# entry point, or a paginator that threw. A deck that loses its pagination is
# a worse deck; a deck that loses a slide is a broken one.
deck_add_table <- function(doc, exhibit, title, layout, master, template) {

  if (!deck_pageable(exhibit)) {
    return(NULL)
  }

  add <- tryCatch(
    utils::getFromNamespace("pptx_add_exhibit", "blockr.viz"),
    error = function(e) NULL
  )

  if (!is.function(add)) {
    return(NULL)
  }

  before <- length(doc)

  out <- tryCatch(
    add(doc, exhibit, title = title, template = template,
        layout = layout, master = master),
    error = function(e) {
      cat("[deck] could not page '", coal(title, "table"), "': ",
          conditionMessage(e), "\n", sep = "", file = stderr())
      NULL
    }
  )

  if (is.null(out)) {
    return(NULL)
  }

  list(doc = out, n = max(1L, length(out) - before))
}

# Is this exhibit a table the paginator can rebuild page by page? A rendered
# flextable qualifies only when static_table() left the frame on it; a plot,
# a gt table or a widget never does.
deck_pageable <- function(x) {

  # An exhibit that brings its own pptx renderer answers for itself. That is
  # how the summarize table reaches a slide: its marks cannot be text runs in
  # a DrawingML cell, so blockr.viz paints it and appends the pages here,
  # through the same pptx_add_exhibit() seam a flextable uses.
  if (inherits(x, "blockr_exhibit")) {
    return(TRUE)
  }

  if (inherits(x, "flextable")) {
    return(!is.null(attr(x, "exhibit_data")))
  }

  # A ggplot goes through the same seam, when blockr.viz has a method for it.
  # Not because a plot needs paging -- it is one slide by definition -- but
  # because the chart block's own PowerPoint download calls that method, and a
  # chart placed here by different code would be a different picture on the
  # slide than the one the block hands you. One placement rule, one result.
  if (inherits(x, c("gg", "ggplot"))) {
    return(!is.null(pptx_exhibit_method("gg")))
  }

  if (inherits(x, c("patchwork", "trellis", "gt_tbl", "gt_group",
                    "htmlwidget"))) {
    return(FALSE)
  }

  is.data.frame(x)
}

# Does blockr.viz carry a pptx_add_exhibit() method for this class? Asked
# rather than assumed: the method arrived in blockr.viz 0.2.54, and an older
# one installed beside this package must keep placing plots the way it did.
pptx_exhibit_method <- function(cls) {
  tryCatch(
    utils::getS3method("pptx_add_exhibit", cls, envir = asNamespace("blockr.viz")),
    error = function(e) NULL
  )
}

# Place one exhibit on the current slide at its intended coordinates.
# Flextables carry `pptx_left` / `pptx_top` attributes (static_table and the
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
    # static_chart() sizes the plot from the chart's row geometry and carries
    # the result as attributes; a plain ggplot takes the default box.
    loc <- officer::ph_location(
      left = left, top = top,
      width = coal(attr(exhibit, "pptx_width"), 11.9),
      height = coal(attr(exhibit, "pptx_height"), 5.5)
    )
    return(officer::ph_with(doc, exhibit, location = loc))
  }

  if (is.data.frame(exhibit)) {
    ft <- blockr.viz::static_table(exhibit)
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
