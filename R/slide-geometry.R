# The slide block's layout arithmetic, and nothing else.
#
# One geometry, two painters (spec: _blockr.design/open/slide-block/3-design.md).
# Every slot on a composed slide is a rectangle stated in INCHES against the
# 13.333 x 7.5in widescreen slide. The HTML preview multiplies these numbers
# by 96 (px per inch); the officer painter hands them to ph_location()
# unchanged. Neither painter owns any layout arithmetic of its own -- that is
# the property that makes the preview an honest statement about the pptx
# rather than CSS that resembles it.

slide_size <- function() {
  c(w = 13.333, h = 7.5)
}

# The frame every layout sits in. Ported from the mockup's slide-kit.js;
# change it there and here together.
slide_frame <- function() {
  list(
    x = 0.85,            # left margin
    x_right = 0.85,      # right margin (a template frame may differ)
    title_y = 0.52, title_h = 0.62,
    sub_y = 1.16, sub_h = 0.34,
    body_top_sub = 1.76, # body starts here when a subtitle is shown
    body_top_bare = 1.32,
    body_bottom = 6.78,
    foot_y = 6.96, foot_h = 0.30,
    gap = 0.30           # between two content slots
  )
}

slide_content_width <- function(frame = slide_frame()) {
  slide_size()[["w"]] - frame$x - frame$x_right
}

# The frame a house template implies: the reference deck's own "Title and
# Content" layout says where content lives (the BMS master puts the title at
# 0.4in and the body from 1.70 to 6.70), and a slide that ignores that fights
# its own master -- the first BMS render had the subtitle indented past the
# title and the footnote printed over the master's footer chrome. Falls back
# to the bundled-deck frame when the template has no usable layout. Memoised
# per path+mtime: officer::read_pptx() is disk I/O on every preview repaint.
slide_frame_cache <- new.env(parent = emptyenv())

slide_template_frame <- function(template = NULL) {

  template <- coal(template, effective_template())

  if (!nzchar(template) || !file.exists(template) ||
      !requireNamespace("officer", quietly = TRUE)) {
    return(slide_frame())
  }

  key <- paste0(template, "|", file.mtime(template))
  if (!is.null(slide_frame_cache[[key]])) {
    return(slide_frame_cache[[key]])
  }

  fr <- tryCatch(
    {
      doc <- officer::read_pptx(template)
      lays <- officer::layout_summary(doc)
      lay <- if ("Title and Content" %in% lays$layout) {
        "Title and Content"
      } else {
        lays$layout[[1L]]
      }
      p <- officer::layout_properties(doc, layout = lay)
      ttl <- p[p$type == "title", ][1L, ]
      bdy <- p[p$type == "body", ][1L, ]

      if (any(is.na(ttl$offx)) || any(is.na(bdy$offx))) {
        slide_frame()
      } else {
        base <- slide_frame()
        gap <- base$gap
        sub_h <- base$sub_h
        base$x <- ttl$offx
        base$x_right <- slide_size()[["w"]] - (ttl$offx + ttl$cx)
        base$title_y <- ttl$offy
        base$title_h <- ttl$cy
        # The subtitle hangs from the title's TEXT, not from the body box: a
        # house master's title placeholder is tall (BMS: 1.25in for one line
        # of 24pt, text anchored top), and a subtitle at the body top floats
        # a half-slide below the words it belongs to. One line of the
        # master's own title size says where the title text ends; a clamp
        # keeps a short title box from pushing the subtitle below the body.
        tsz <- template_title_size(template)
        if (!is.finite(tsz)) tsz <- 28
        base$sub_y <- min(ttl$offy + tsz / 72 * 1.35 + 0.10, bdy$offy)
        # Content still starts no higher than the master's own body top --
        # the subtitle moving up must not drag the exhibits into the title
        # zone of the house layout.
        base$body_top_sub <- max(bdy$offy, base$sub_y + sub_h + gap)
        base$body_top_bare <- bdy$offy
        base$body_bottom <- bdy$offy + bdy$cy
        # Tucked between the body bottom and the master's own footer
        # chrome, which on a house deck starts right below.
        base$foot_y <- bdy$offy + bdy$cy + 0.02
        base$foot_h <- 0.22
        base
      }
    },
    error = function(e) slide_frame()
  )

  slide_frame_cache[[key]] <- fr
  fr
}

rect <- function(x, y, w, h) {
  list(x = x, y = y, w = w, h = h)
}

slot <- function(kind, key, rect, ...) {
  c(list(kind = kind, key = key, rect = rect), list(...))
}

# The layout registry: the six starting layouts (spec req. 4). `inputs` is
# the layout's exhibit capacity; `chrome = "none"` drops title / subtitle /
# footnote (the divider carries its own type). `text` names the layout's one
# authored text field -- `kind` says how the painters read it ("text" = a
# paragraph of runs, "bullets" = one bullet per line).
slide_layouts <- function() {
  list(

    "exhibit-full" = list(
      name = "Full exhibit", inputs = 1L, chrome = "full", text = NULL,
      build = function(b) {
        list(slot("exhibit", "exhibit1", b))
      }
    ),

    "two-up-h" = list(
      name = "Two-up, side by side", inputs = 2L, chrome = "full",
      text = NULL,
      build = function(b) {
        gap <- slide_frame()$gap
        w <- (b$w - gap) / 2
        list(
          slot("exhibit", "exhibit1", rect(b$x, b$y, w, b$h)),
          slot("exhibit", "exhibit2", rect(b$x + w + gap, b$y, w, b$h))
        )
      }
    ),

    "exhibit-callout" = list(
      name = "Exhibit + takeaway", inputs = 1L, chrome = "full",
      text = list(key = "callout", label = "Details", kind = "text"),
      build = function(b) {
        gap <- slide_frame()$gap
        ch <- 1.05
        list(
          slot("exhibit", "exhibit1", rect(b$x, b$y, b$w, b$h - ch - gap)),
          slot("callout", "callout", rect(b$x, b$y + b$h - ch, b$w, ch))
        )
      }
    ),

    "exhibit-notes" = list(
      name = "Exhibit + notes", inputs = 1L, chrome = "full",
      text = list(key = "notes", label = "Details", kind = "bullets"),
      build = function(b) {
        gap <- slide_frame()$gap
        w <- b$w * 0.64
        list(
          slot("exhibit", "exhibit1", rect(b$x, b$y, w, b$h)),
          slot("bullets", "notes",
               rect(b$x + w + gap, b$y, b$w - w - gap, b$h))
        )
      }
    ),

    "bullets" = list(
      name = "Bullets", inputs = 0L, chrome = "full",
      text = list(key = "bullets", label = "Details", kind = "bullets"),
      build = function(b) {
        list(slot("bullets", "bullets", b))
      }
    ),

    "compare-2" = list(
      name = "Compare", inputs = 2L, chrome = "full", text = NULL,
      labels = TRUE,
      build = function(b) {
        gap <- slide_frame()$gap
        w <- (b$w - gap) / 2
        hh <- 0.42
        et <- b$y + hh + 0.12
        list(
          slot("panelhead", "label1", rect(b$x, b$y, w, hh), panel = 1L),
          slot("panelhead", "label2", rect(b$x + w + gap, b$y, w, hh),
               panel = 2L),
          slot("exhibit", "exhibit1", rect(b$x, et, w, b$h - hh - 0.12)),
          slot("exhibit", "exhibit2",
               rect(b$x + w + gap, et, w, b$h - hh - 0.12))
        )
      }
    ),

    "lead-exhibit" = list(
      name = "Lead + exhibit", inputs = 1L, chrome = "full",
      text = list(key = "lead", label = "Details", kind = "text"),
      build = function(b) {
        gap <- slide_frame()$gap
        lh <- 0.80
        list(
          slot("text", "lead", rect(b$x, b$y, b$w, lh)),
          slot("exhibit", "exhibit1",
               rect(b$x, b$y + lh + gap, b$w, b$h - lh - gap))
        )
      }
    ),

    "full-bleed" = list(
      name = "Full bleed", inputs = 1L, chrome = "none", text = NULL,
      build = function(b) {
        sz <- slide_size()
        list(slot("exhibit", "exhibit1",
                  rect(0.4, 0.4, sz[["w"]] - 0.8, sz[["h"]] - 0.8)))
      }
    ),

    "agenda" = list(
      name = "Agenda", inputs = 0L, chrome = "full",
      text = list(key = "agenda", label = "Details", kind = "bullets"),
      build = function(b) {
        list(slot("bullets", "agenda",
                  rect(b$x, b$y + 0.2, b$w * 0.7, b$h - 0.2),
                  numbered = TRUE))
      }
    ),

    "section" = list(
      name = "Section divider", inputs = 0L, chrome = "none",
      text = list(key = "kicker", label = "Details", kind = "text"),
      build = function(b) {
        # The body box arrives even for chrome = "none" layouts; the divider
        # centres itself in the frame it was handed.
        list(slot("section", "section",
                  rect(b$x, 2.4, b$w * 0.8, 2.6)))
      }
    )
  )
}

slide_layout_spec <- function(layout) {
  spec <- slide_layouts()[[layout]]
  if (is.null(spec)) {
    stop("unknown slide layout '", layout, "'", call. = FALSE)
  }
  spec
}

# Every slot on the slide -- chrome first, then the layout's body slots --
# with its rect resolved. The one entry point both painters iterate.
slide_slots <- function(layout, has_subtitle = TRUE, frame = NULL) {

  spec <- slide_layout_spec(layout)
  fr <- coal(frame, slide_frame())
  cw <- slide_content_width(fr)

  chrome <- list()

  if (identical(spec$chrome, "full")) {
    chrome <- c(
      list(slot("title", "title", rect(fr$x, fr$title_y, cw, fr$title_h))),
      if (has_subtitle) {
        list(slot("subtitle", "subtitle", rect(fr$x, fr$sub_y, cw, fr$sub_h)))
      },
      list(slot("footnote", "footnote", rect(fr$x, fr$foot_y, cw, fr$foot_h)))
    )
  }

  top <- if (has_subtitle) fr$body_top_sub else fr$body_top_bare
  body <- rect(fr$x, top, cw, fr$body_bottom - top)

  c(chrome, spec$build(body))
}


# Which layouts show which optional controls -- computed from the registry,
# so a new layout brings its visibility along. Space-separated id lists for
# the picker's client-side show / hide (see slide_layout_picker()).
slide_layout_features <- function() {
  lays <- slide_layouts()
  ids <- names(lays)
  has <- function(p) paste(ids[vapply(ids, p, logical(1L))], collapse = " ")
  list(
    text = has(function(i) !is.null(lays[[i]]$text)),
    labels = has(function(i) isTRUE(lays[[i]]$labels)),
    paginate = has(function(i) {
      kinds <- chr_ply(lays[[i]]$build(rect(0, 0, 1, 1)), `[[`, "kind")
      identical(kinds, "exhibit")
    }),
    numbered = has(function(i) {
      slots <- lays[[i]]$build(rect(0, 0, 1, 1))
      any(vapply(slots, function(s) isTRUE(s$numbered), logical(1L)))
    })
  )
}
