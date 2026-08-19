# The slide's officer painter: the same slide_slots() rects the preview drew,
# handed to ph_location() in inches. No layout arithmetic happens here.

#' Write a composed slide to PowerPoint
#'
#' Renders one [slide()] against the effective reference template (the
#' `blockr.outline.template` option, falling back to the bundled widescreen
#' deck) -- one slide, placed from the same inch geometry the block preview
#' painted, which is the whole contract.
#'
#' @param x A `blockr_slide`.
#' @param file Path to write the `.pptx` to.
#' @param template Path to a reference `.pptx`, or `NULL` for the effective
#'   template.
#'
#' @return `file`, invisibly.
#'
#' @export
write_slide_pptx <- function(x, file, template = NULL) {

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("write_slide_pptx() needs the 'officer' package.", call. = FALSE)
  }

  template <- coal(template, effective_template())

  doc <- if (nzchar(template) && file.exists(template)) {
    strip_slides(officer::read_pptx(template))
  } else {
    officer::read_pptx()
  }

  doc <- slide_pptx_add(doc, x, template = template)

  print(doc, target = file)
  invisible(file)
}

# Append one composed slide to an open rpptx. Shared by the block's own
# download and (through the exhibit protocol, below) the deck.
slide_pptx_add <- function(doc, x, template = NULL) {

  layouts <- officer::layout_summary(doc)
  lay <- if ("Title and Content" %in% layouts$layout) {
    "Title and Content"
  } else {
    layouts$layout[[1L]]
  }
  master <- layouts$master[match(lay, layouts$layout)]

  fnt <- coal(tryCatch(template_body_font(template), error = function(e) NULL),
              "Inter")

  has_sub <- nzchar(x$subtitle)
  frame <- slide_template_frame(template)

  # The paginating single-exhibit slide: blockr.viz's paginator makes the
  # slides (repeated header, title marked "(2 of 3)"), and the slide's own
  # subtitle / footnote chrome is stamped onto every page afterwards, so
  # each slide pulled out of the deck stands alone. Anything that keeps
  # this from working falls through to the fixed-canvas painter below --
  # which is also why the branch runs BEFORE add_slide(): the paginator
  # makes its own slides, and a slide made here first would stay blank.
  if (isTRUE(x$paginate) && slide_single_exhibit(x) &&
      length(x$exhibits) >= 1L) {
    paged <- slide_pptx_paged(doc, x, fnt, frame, template, lay, master)
    if (!is.null(paged)) {
      return(paged)
    }
  }

  doc <- officer::add_slide(doc, layout = lay, master = master)

  for (s in slide_slots(x$layout, has_subtitle = has_sub, frame = frame)) {
    doc <- slide_pptx_slot(doc, s, x, fnt, template, frame)
  }

  slide_strip_placeholders(doc)
}

# The single-exhibit slide through blockr.viz's paginator. NULL when the
# exhibit is not a table the paginator can rebuild (a chart, an unknown
# object) or when it fits its slot anyway -- then the fixed canvas is the
# simpler, identical answer.
slide_pptx_paged <- function(doc, x, fnt, frame, template, lay, master) {

  val <- x$exhibits[[1L]]

  df <- tryCatch(as.data.frame(val), error = function(e) NULL)
  if (is.null(df)) {
    return(NULL)
  }

  has_sub <- nzchar(x$subtitle)
  slots <- slide_slots(x$layout, has_subtitle = has_sub, frame = frame)
  r <- NULL
  for (s in slots) {
    if (s$kind == "exhibit") r <- s$rect
  }

  # The same fit estimate the fixed painter uses: only an OVERFLOWING table
  # is worth the paginator, a fitting one comes out the same either way.
  if (nrow(df) <= floor((r$h - 0.40) / 0.28)) {
    return(NULL)
  }

  if (!requireNamespace("blockr.viz", quietly = TRUE)) {
    return(NULL)
  }

  before <- length(doc)

  out <- tryCatch(
    {
      # The deck render sets this pair globally around a whole render; a
      # block's own download reaches the paginator directly, so it states
      # them here: columns sized to the slot, the template's face.
      old <- options(
        c(
          list(blockr.viz.ft_fit_width = r$w),
          if (is.null(getOption("blockr.viz.ft_font"))) {
            list(blockr.viz.ft_font = fnt)
          }
        )
      )
      on.exit(options(old), add = TRUE)
      blockr.viz::pptx_add_exhibit(
        doc, val,
        title = if (nzchar(x$title)) x$title,
        template = template, layout = lay, master = master,
        top = r$y
      )
    },
    error = function(e) NULL
  )

  if (is.null(out) || length(out) <= before) {
    return(NULL)
  }

  # The slide's own chrome, on every page: a slide pulled out of the deck
  # has to say what it shows and where the numbers came from, same rule the
  # paginator applies to the title and the header band.
  for (k in seq(before + 1L, length(out))) {
    out <- tryCatch(
      {
        cur <- officer::on_slide(out, index = k)
        for (s in slots) {
          if (s$kind %in% c("subtitle", "footnote")) {
            cur <- slide_pptx_slot(cur, s, x, fnt, template, frame)
          }
        }
        slide_strip_placeholders(cur)
      },
      error = function(e) out
    )
  }

  tryCatch(
    officer::on_slide(out, index = length(out)),
    error = function(e) out
  )
}

loc <- function(r, ...) {
  officer::ph_location(
    left = r$x, top = r$y, width = r$w, height = r$h, ...
  )
}

# Inline runs -> an officer paragraph. fp_text() rather than fp_text_lite(),
# which writes nothing through the pptx path in officer 0.7.3 (see the deck
# title notes in R/render.R); the face and colour are therefore stated
# explicitly on every run.
runs_fpar <- function(runs, font, size, color = "#111827", prefix = NULL) {

  fp <- function(bold = FALSE, italic = FALSE) {
    officer::fp_text(
      font.size = size, font.family = font, color = color,
      bold = bold, italic = italic
    )
  }

  txts <- lapply(runs, function(r) {
    officer::ftext(r$text, fp(isTRUE(r$bold), isTRUE(r$italic)))
  })

  if (!is.null(prefix)) {
    txts <- c(list(officer::ftext(prefix, fp())), txts)
  }

  do.call(officer::fpar, txts)
}

# The Details field as an officer block: a `- ` line takes the bullet dot,
# a `1. ` line its renumbered position (the counter follows consecutive
# runs, resetting when a plain line or a bullet breaks the list -- markdown
# semantics), a plain line takes none. Same classification as the HTML
# painter's slide_details_html().
lines_block <- function(text, font, size, color = "#111827") {
  lines <- md_lines(text)
  if (!length(lines)) {
    return(NULL)
  }
  counter <- 0L
  do.call(officer::block_list, lapply(lines, function(l) {
    prefix <- if (identical(l$marker, "bullet")) {
      counter <<- 0L
      "\u2022  "
    } else if (identical(l$marker, "number")) {
      counter <<- counter + 1L
      paste0(counter, ".  ")
    } else {
      counter <<- 0L
      NULL
    }
    runs_fpar(l$runs, font, size, color, prefix = prefix)
  }))
}

slide_pptx_slot <- function(doc, s, x, fnt, template = NULL,
                            frame = NULL) {

  r <- s$rect

  place <- function(value, ...) {
    tryCatch(
      officer::ph_with(doc, value, location = loc(r, ...)),
      error = function(e) doc
    )
  }

  switch(
    s$kind,

    title = {
      if (!nzchar(x$title)) return(doc)
      # Into the template's own title placeholder, so the house style
      # decides how a title looks; the geometry box is the fallback.
      tryCatch(
        officer::ph_with(
          doc, x$title,
          location = officer::ph_location_type(type = "title")
        ),
        error = function(e) place(x$title)
      )
    },

    subtitle = {
      if (!nzchar(x$subtitle)) return(doc)
      blk <- lines_block(x$subtitle, fnt, 13, color = "#6b7280")
      if (is.null(blk)) doc else place(blk)
    },

    footnote = {
      if (!nzchar(x$footnote)) return(doc)
      # NOT the layout's ftr placeholder, though the template has one: the
      # BMS master DRAWS its footer line (logo, division, classification) as
      # master chrome, and a filled ftr placeholder lands on top of it at
      # the master's own text size. The footnote is ours: a small grey box
      # between the body bottom and the chrome (the template frame puts
      # foot_y there).
      blk <- lines_block(x$footnote, fnt, 9, color = "#9ca3af")
      if (is.null(blk)) doc else place(blk)
    },

    exhibit = {
      i <- as.integer(sub("exhibit", "", s$key))
      val <- if (i <= length(x$exhibits)) x$exhibits[[i]]
      if (is.null(val)) return(doc)
      slide_pptx_exhibit(doc, val, r, fnt, template)
    },

    bullets = {
      blk <- lines_block(x$text, fnt, 15)
      if (is.null(blk)) doc else place(blk)
    },

    text = {
      blk <- lines_block(x$text, fnt, 14, color = "#374151")
      if (is.null(blk)) doc else place(blk)
    },

    panelhead = {
      lab <- panel_labels(x$labels, 2L)[[coal(s$panel, 1L)]]
      if (!nzchar(lab)) return(doc)
      place(do.call(officer::block_list, list(runs_fpar(
        list(list(text = lab, bold = TRUE, italic = FALSE)),
        fnt, 14, color = "#374151"
      ))))
    },

    callout = {
      blk <- lines_block(x$text, fnt, 14)
      if (is.null(blk)) return(doc)
      # The tinted panel and its accent bar are the location's background --
      # two boxes, no image (spec: the callout carries no icon). The bar
      # carries a space: an empty paragraph left officer emitting no shape
      # at all, which is why the first BMS render had no bar.
      doc <- tryCatch(
        officer::ph_with(
          doc, officer::block_list(officer::fpar(officer::ftext(" "))),
          location = officer::ph_location(
            left = r$x, top = r$y, width = 0.08, height = r$h, bg = "#0072b2"
          )
        ),
        error = function(e) doc
      )
      doc <- tryCatch(
        officer::ph_with(
          doc, blk,
          location = officer::ph_location(
            left = r$x + 0.08, top = r$y, width = r$w - 0.08, height = r$h,
            bg = "#f2f7fb"
          )
        ),
        error = function(e) doc
      )
      slide_anchor_middle(doc, l_ins = 0.2)
    },

    section = {
      doc <- if (nzchar(x$text)) {
        place(lines_block(x$text, fnt, 13, color = "#0072b2"))
      } else {
        doc
      }
      if (!nzchar(x$title)) return(doc)
      blk <- lines_block(x$title, fnt, 40)
      tryCatch(
        officer::ph_with(
          doc, blk,
          location = officer::ph_location(
            left = r$x, top = r$y + 0.45, width = r$w, height = r$h - 0.45
          )
        ),
        error = function(e) doc
      )
    },

    doc
  )
}

# An exhibit into its slot: the rows that fit, then the marker. The same
# truncate-and-say-so the preview shows -- a composed slide never paginates,
# because a second slide would duplicate the other slots (spec req. 21).
#
# The table is typeset by the SAME routine the deck render uses --
# blockr.viz::static_exhibit() under the deck's two options -- so a slide
# block's table under a house theme (BMS bands, Trebuchet) is byte-for-byte
# the deck's table, only sized to the SLOT's width rather than the slide's
# content width. That substitution is the whole difference, and it is why
# nothing here styles anything.
slide_pptx_exhibit <- function(doc, val, r, fnt, template = NULL) {

  if (inherits(val, c("gg", "ggplot"))) {
    # A chart that states its own size (static_chart's pptx_width/height)
    # keeps it, centred in the slot; capped at the slot, never stretched.
    w <- min(coal(attr(val, "pptx_width"), r$w), r$w)
    h <- min(coal(attr(val, "pptx_height"), r$h), r$h)
    return(tryCatch(
      officer::ph_with(
        doc, val,
        location = officer::ph_location(
          left = r$x + (r$w - w) / 2, top = r$y, width = w, height = h
        )
      ),
      error = function(e) doc
    ))
  }

  df <- tryCatch(as.data.frame(val), error = function(e) NULL)
  if (is.null(df)) {
    return(doc)
  }

  # Estimated rather than measured for now: static_table's measured heights
  # are the follow-on once this route and blockr.viz share the fit pass.
  head_in <- 0.40
  row_in <- 0.28
  mark_in <- 0.25

  fit <- floor((r$h - head_in) / row_in)
  if (fit < nrow(df)) {
    fit <- floor((r$h - head_in - mark_in) / row_in)
  }
  fit <- max(1L, min(fit, nrow(df)))
  cut <- nrow(df) - fit

  shown <- utils::head(df, fit)

  # The deck render's option pair (render_pptx_officer, render.R ~1010),
  # scoped to this one slot: width = the slot, face = the template's body
  # font unless a theme has already named one (the theme is the more
  # specific answer -- same precedence as the deck).
  old <- options(
    c(
      list(blockr.viz.ft_fit_width = r$w),
      if (is.null(getOption("blockr.viz.ft_font"))) {
        font <- tryCatch(template_body_font(template), error = function(e) NULL)
        if (!is.null(font)) list(blockr.viz.ft_font = font)
      }
    )
  )
  on.exit(options(old), add = TRUE)

  ft <- if (requireNamespace("blockr.viz", quietly = TRUE)) {
    tryCatch(blockr.viz::static_exhibit(shown), error = function(e) shown)
  } else {
    shown
  }

  if (!inherits(ft, "flextable") &&
      requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(shown)
  }

  # Centred in the slot, the way the block's own PowerPoint button centres
  # its table on the slide: a content-sized table hard against the slot's
  # left edge reads as misplaced (the first BMS render).
  left <- r$x
  w <- NULL
  if (inherits(ft, "flextable")) {
    w <- tryCatch(sum(flextable::flextable_dim(ft)$widths),
                  error = function(e) NULL)
    if (!is.null(w) && is.finite(w) && w < r$w) {
      left <- r$x + (r$w - w) / 2
    }
  }

  doc <- tryCatch(
    officer::ph_with(
      doc, ft,
      location = officer::ph_location(
        left = left, top = r$y, width = min(r$w, coal(w, r$w)), height = r$h
      )
    ),
    error = function(e) doc
  )

  if (cut > 0L) {
    doc <- tryCatch(
      officer::ph_with(
        doc,
        officer::block_list(officer::fpar(officer::ftext(
          paste(cut, "more rows do not fit"),
          officer::fp_text(font.size = 10, font.family = fnt,
                           color = "#b45309")
        ))),
        location = officer::ph_location(
          left = r$x, top = r$y + r$h - mark_in, width = r$w, height = mark_in
        )
      ),
      error = function(e) doc
    )
  }

  doc
}

# Free-placed text shapes must NOT be placeholders. ph_location() emits an
# EMPTY <p:ph/> (no type, no idx) on every shape it places, and PowerPoint
# resolves that against the layout's body placeholder and re-inherits its
# geometry and insets -- every authored text box drifted to the slide edge,
# the takeaway rendering BEHIND its own accent bar. LibreOffice honours the
# explicit xfrm, which is why the container-side render never showed it:
# found by Christoph opening the file in PowerPoint itself. Stripping the
# bare <p:ph/> turns them into plain positioned text boxes; the title keeps
# its typed placeholder (that inheritance is wanted -- it IS the house
# style), and the flextable is a graphicFrame with no placeholder at all.
slide_strip_placeholders <- function(doc) {
  tryCatch(
    {
      sl <- doc$slide$get_slide(doc$cursor)
      nodes <- xml2::xml_find_all(
        sl$get(), "//p:sp//p:nvPr/p:ph[not(@type)]"
      )
      for (n in nodes) {
        # The shape's geometry came from the placeholder it no longer is:
        # without an explicit <a:prstGeom> a plain sp has nothing to fill,
        # and the callout's tint vanished. State the rectangle.
        sp <- xml2::xml_find_first(n, "ancestor::p:sp")
        sppr <- xml2::xml_find_first(sp, ".//p:spPr")
        if (!inherits(sppr, "xml_missing") &&
            !length(xml2::xml_find_all(sppr, ".//a:prstGeom"))) {
          xml2::xml_add_child(
            sppr,
            xml2::read_xml(paste0(
              "<a:prstGeom xmlns:a=\"http://schemas.openxmlformats.org/",
              "drawingml/2006/main\" prst=\"rect\"><a:avLst/></a:prstGeom>"
            ))
          )
        }
        # Zero the text insets unless the slot set its own (the callout's
        # lIns): the box edge IS the stated geometry. The OOXML default is
        # 0.1in, which put every free text 0.1in right of the title -- the
        # BMS master's title placeholder says lIns="0", and the HTML
        # preview draws text at the box edge too, so zero is what aligns
        # all three.
        bp <- xml2::xml_find_first(sp, ".//a:bodyPr")
        if (!inherits(bp, "xml_missing") &&
            is.na(xml2::xml_attr(bp, "lIns"))) {
          xml2::xml_set_attr(bp, "lIns", "0")
          xml2::xml_set_attr(bp, "rIns", "0")
        }
        xml2::xml_remove(n)
      }
      doc
    },
    error = function(e) doc
  )
}

# Vertically centre the text of the shape placed LAST on the current slide:
# the callout's text sat on the top edge of its tinted box. `anchor` lives on
# <a:bodyPr>, which officer 0.7.3 exposes no argument for -- patched in the
# slide XML directly, the same internal surface (and the same tryCatch
# wrapping) as deck_title_size() in R/render.R.
slide_anchor_middle <- function(doc, l_ins = NULL) {
  tryCatch(
    {
      sl <- doc$slide$get_slide(doc$cursor)
      nodes <- xml2::xml_find_all(sl$get(), "//p:sp//a:bodyPr")
      if (length(nodes)) {
        node <- nodes[[length(nodes)]]
        xml2::xml_set_attr(node, "anchor", "ctr")
        if (!is.null(l_ins)) {
          xml2::xml_set_attr(node, "lIns", as.character(round(l_ins * 914400)))
        }
      }
      doc
    },
    error = function(e) doc
  )
}

# The deck's seam: a picked slide block is an exhibit that claims its whole
# slide, so the extension needs no new plumbing (spec req. 25). Registered
# dynamically because blockr.viz is a Suggests.
#' @exportS3Method blockr.viz::pptx_add_exhibit
pptx_add_exhibit.blockr_slide <- function(doc, x, ...) {
  slide_pptx_add(doc, x)
}
