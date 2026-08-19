# The slide's HTML painter: the block preview, and eventually the HTML deck's
# rendering of a picked slide block. Everything inside the canvas is laid out
# in px at the logical 1280x720 (the inch rects times 96), so the whole slide
# scales with one transform and nothing reflows -- the same trick the HTML
# deck uses (R/deck-html.R).

SLIDE_PPI <- 96

px <- function(inches) {
  paste0(round(inches * SLIDE_PPI, 1L), "px")
}

slot_style <- function(r, extra = "") {
  paste0(
    "position:absolute;overflow:hidden;",
    "left:", px(r$x), ";top:", px(r$y), ";",
    "width:", px(r$w), ";height:", px(r$h), ";",
    extra
  )
}

# Inline runs -> escaped HTML with <strong>/<em>.
runs_html <- function(runs) {
  paste0(chr_ply(runs, function(r) {
    txt <- htmltools::htmlEscape(r$text)
    if (isTRUE(r$bold)) txt <- paste0("<strong>", txt, "</strong>")
    if (isTRUE(r$italic)) txt <- paste0("<em>", txt, "</em>")
    txt
  }), collapse = "")
}

# The canvas-scoped stylesheet, inlined rather than shipped as an asset: the
# preview re-renders with the block and an inline tag cannot go stale against
# an installed copy (the load_all / inst cache trap).
slide_html_css <- function() {
  "
.bslide-canvas { position:relative; width:1280px; height:720px; flex:0 0 auto;
  background:#fff; overflow:hidden; transform-origin:top left;
  font-family:Inter,'Inter var',system-ui,-apple-system,'Segoe UI',Roboto,
    'Helvetica Neue',Arial,sans-serif;
  color:#111827; font-variant-numeric:tabular-nums;
  -webkit-font-smoothing:antialiased; }
.bslide-title { font-size:30px; font-weight:600; line-height:1.2;
  letter-spacing:-.01em; }
.bslide-subtitle { font-size:17px; color:#6b7280; line-height:1.35; }
.bslide-foot { display:flex; justify-content:space-between; font-size:13px;
  color:#9ca3af; }
.bslide-table { width:100%; border-collapse:collapse; font-size:17px; }
.bslide-table th { text-align:left; font-weight:600; padding:7px 10px;
  border-bottom:1.5px solid #111827; white-space:nowrap; }
.bslide-table td { padding:7px 10px; border-bottom:1px solid #e5e7eb; }
.bslide-table .num { text-align:right; }
.bslide-cut { margin-top:8px; font-size:14px; color:#b45309; }
.bslide-cut--paged { color:#6b7280; }
.bslide-bullets { margin:0; padding:0 0 0 22px; font-size:20px;
  line-height:1.5; }
.bslide-bullets li { margin-bottom:14px; }
.bslide-callout { display:flex; align-items:center; height:100%;
  box-sizing:border-box; padding:16px 20px; background:#f2f7fb;
  border-left:4px solid #0072b2; border-radius:4px; font-size:19px;
  line-height:1.4; }
.bslide-kicker { font-size:17px; font-weight:600; letter-spacing:.08em;
  text-transform:uppercase; color:#0072b2; }
.bslide-section-title { margin:14px 0 0; font-size:54px; font-weight:600;
  line-height:1.1; letter-spacing:-.015em; }
.bslide-rule { width:84px; height:4px; margin-top:26px; background:#0072b2; }
.bslide-empty { height:100%; display:flex; align-items:center;
  justify-content:center; border:1px dashed #d1d5db; border-radius:4px;
  color:#9ca3af; font-size:16px; }
"
}

# A data frame (or anything coercible to one) drawn into a slot: the rows
# that fit, and how many did not. Truncate-and-say-so (spec req. 21) -- the
# arithmetic mirrors the stylesheet above (42px header, 40px rows, 30px
# marker), so the preview never scrolls and never lies.
slide_table_html <- function(x, r, paginate = FALSE) {

  df <- as.data.frame(x)

  avail <- r$h * SLIDE_PPI
  fit <- floor((avail - 42) / 40)
  if (fit < nrow(df)) {
    fit <- floor((avail - 42 - 30) / 40)
  }
  fit <- max(1L, min(fit, nrow(df)))
  cut <- nrow(df) - fit

  num <- vapply(df, is.numeric, logical(1L))
  fmt <- function(v, i) {
    cls <- if (num[[i]]) " class=\"num\"" else ""
    paste0("<td", cls, ">", htmltools::htmlEscape(format(v)), "</td>")
  }

  head_cells <- paste0(chr_ply(seq_along(df), function(i) {
    cls <- if (num[[i]]) " class=\"num\"" else ""
    paste0("<th", cls, ">", htmltools::htmlEscape(names(df)[[i]]), "</th>")
  }), collapse = "")

  body <- paste0(chr_ply(seq_len(fit), function(rw) {
    paste0("<tr>",
           paste0(chr_ply(seq_along(df), function(i) fmt(df[[i]][[rw]], i)),
                  collapse = ""),
           "</tr>")
  }), collapse = "")

  htmltools::HTML(paste0(
    "<table class=\"bslide-table\"><thead><tr>", head_cells,
    "</tr></thead><tbody>", body, "</tbody></table>",
    if (cut > 0L && paginate) {
      # The download pages; the preview is page one and says so.
      paste0("<div class=\"bslide-cut bslide-cut--paged\">", cut,
             " more rows continue on further slides</div>")
    } else if (cut > 0L) {
      paste0("<div class=\"bslide-cut\">", cut, " more rows do not fit</div>")
    }
  ))
}

slide_bullets_html <- function(text) {
  lines <- md_lines(text)
  if (!length(lines)) {
    return(NULL)
  }
  htmltools::HTML(paste0(
    "<ul class=\"bslide-bullets\">",
    paste0(chr_ply(lines, function(l) paste0("<li>", runs_html(l), "</li>")),
           collapse = ""),
    "</ul>"
  ))
}

slide_para_html <- function(text) {
  lines <- md_lines(text)
  if (!length(lines)) {
    return(NULL)
  }
  htmltools::HTML(
    paste0(chr_ply(lines, runs_html), collapse = "<br>")
  )
}

slide_slot_html <- function(s, x) {

  r <- s$rect

  content <- switch(
    s$kind,
    title = htmltools::div(class = "bslide-title", x$title),
    subtitle = htmltools::div(class = "bslide-subtitle", x$subtitle),
    footnote = htmltools::div(
      class = "bslide-foot",
      htmltools::span(x$footnote)
    ),
    exhibit = {
      i <- as.integer(sub("exhibit", "", s$key))
      val <- if (i <= length(x$exhibits)) x$exhibits[[i]]
      if (is.null(val)) {
        htmltools::div(class = "bslide-empty", "not linked")
      } else {
        slide_table_html(
          val, r,
          paginate = isTRUE(x$paginate) && slide_single_exhibit(x)
        )
      }
    },
    bullets = slide_bullets_html(x$text),
    callout = htmltools::div(
      class = "bslide-callout",
      htmltools::div(slide_para_html(x$text))
    ),
    section = htmltools::tagList(
      htmltools::div(class = "bslide-kicker", x$text),
      htmltools::h2(class = "bslide-section-title", x$title),
      htmltools::div(class = "bslide-rule")
    )
  )

  htmltools::div(style = slot_style(r), content)
}

# The slide at its logical size. `slide_html_fit()` wraps it in a stage that
# scales the fixed canvas to whatever width the block panel offers.
slide_html <- function(x, frame = slide_template_frame()) {

  has_sub <- nzchar(x$subtitle)
  slots <- slide_slots(x$layout, has_subtitle = has_sub, frame = frame)

  keep <- vapply(slots, function(s) {
    switch(
      s$kind,
      title = nzchar(x$title),
      subtitle = has_sub,
      footnote = nzchar(x$footnote),
      TRUE
    )
  }, logical(1L))

  htmltools::div(
    class = "bslide-canvas",
    htmltools::tags$style(htmltools::HTML(slide_html_css())),
    lapply(slots[keep], slide_slot_html, x = x)
  )
}

slide_html_fit <- function(x) {
  htmltools::div(
    class = "bslide-stage",
    style = paste0(
      "position:relative;overflow:hidden;border:1px solid #e5e7eb;",
      "border-radius:6px;background:#fff;"
    ),
    slide_html(x),
    # Scale the fixed canvas to the stage's width; height follows 16:9. The
    # deck's own scaler works the same way.
    htmltools::tags$script(htmltools::HTML(
      "(function() {
         var stages = document.querySelectorAll('.bslide-stage');
         var st = stages[stages.length - 1];
         function fit() {
           var c = st.querySelector('.bslide-canvas');
           if (!c) return;
           var s = st.clientWidth / 1280;
           c.style.transform = 'scale(' + s + ')';
           st.style.height = (720 * s) + 'px';
         }
         fit();
         if (window.ResizeObserver) new ResizeObserver(fit).observe(st);
       })();"
    ))
  )
}


#' Write a composed slide to a standalone HTML file
#'
#' The HTML sibling of [write_slide_pptx()]: the same canvas the block
#' preview paints -- the same inch geometry at 96px per inch -- written as
#' one self-contained file, scaled to the window like the HTML deck's
#' slides. Something you can send.
#'
#' @param x A `blockr_slide`.
#' @param file Path to write the `.html` to.
#'
#' @return `file`, invisibly.
#'
#' @export
write_slide_html <- function(x, file) {

  canvas <- htmltools::renderTags(slide_html(x))

  writeLines(c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    paste0("<title>", htmltools::htmlEscape(coal(x$title, "Slide")),
           "</title>"),
    "<style>",
    "html, body { margin: 0; height: 100%; background: #eef0f3; }",
    "body { display: flex; align-items: center; justify-content: center; }",
    ".bslide-stage { overflow: hidden; border-radius: 6px;",
    "  box-shadow: 0 1px 2px rgba(15,23,42,.06), 0 12px 32px rgba(15,23,42,.10); }",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"bslide-stage\">",
    as.character(canvas$html),
    "</div>",
    "<script>",
    "(function() {",
    "  var st = document.querySelector('.bslide-stage');",
    "  var c = st.querySelector('.bslide-canvas');",
    "  function fit() {",
    "    var s = Math.min((window.innerWidth - 48) / 1280,",
    "                     (window.innerHeight - 48) / 720);",
    "    c.style.transform = 'scale(' + s + ')';",
    "    st.style.width = (1280 * s) + 'px';",
    "    st.style.height = (720 * s) + 'px';",
    "  }",
    "  fit();",
    "  window.addEventListener('resize', fit);",
    "})();",
    "</script>",
    "</body>",
    "</html>"
  ), file)

  invisible(file)
}
