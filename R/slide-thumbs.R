# Layout schematics for the slide block's picker: icon tiles (the chart
# type picker's pattern), with the glyph drawn
# FROM the layout's own slot rects -- a new layout costs no artwork, and the
# thumbnail cannot drift from the geometry because it IS the geometry.

# One layout as an inline SVG schematic. The viewBox is the slide's inches
# times ten, so every rect below is the geometry's numbers verbatim.
slide_layout_thumb <- function(layout) {

  spec <- slide_layout_spec(layout)
  fr <- slide_frame()
  sz <- slide_size()
  cw <- slide_content_width(fr)
  bare <- identical(spec$chrome, "none")

  acc <- new.env(parent = emptyenv())
  acc$rects <- list()
  # `ink` is a class, not a colour: blockr-slide-block.css paints each kind
  # of stroke from the tokens, so the schematic follows the scheme.
  add <- function(x, y, w, h, ink, rx = 1.5) {
    acc$rects[[length(acc$rects) + 1L]] <- sprintf(
      '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f" class="slb-%s"/>',
      x * 10, y * 10, w * 10, h * 10, rx, ink
    )
  }

  if (!bare) {
    add(fr$x, fr$title_y + 0.1, cw * 0.55, 0.42, "ink")
    add(fr$x, fr$sub_y + 0.05, cw * 0.38, 0.26, "line")
  }

  top <- if (bare) fr$body_top_bare else fr$body_top_sub
  body <- rect(fr$x, top, cw, fr$body_bottom - top)

  for (s in spec$build(body)) {
    r <- s$rect
    if (s$kind == "exhibit") {
      add(r$x, r$y, r$w, r$h, "exhibit")
    } else if (s$kind == "panelhead") {
      add(r$x, r$y + 0.08, r$w * 0.5, 0.2, "ink")
    } else if (s$kind == "text") {
      add(r$x, r$y + 0.05, r$w * 0.94, 0.14, "line")
      add(r$x, r$y + 0.35, r$w * 0.72, 0.14, "line")
    } else if (s$kind == "callout") {
      add(r$x, r$y, r$w, r$h, "callout")
    } else if (s$kind == "section") {
      add(r$x, r$y, r$w * 0.28, 0.22, "accent")
      add(r$x, r$y + 0.5, r$w * 0.82, 0.62, "ink")
      add(r$x, r$y + 1.4, r$w * 0.18, 0.12, "accent")
    } else {
      # bullets / notes: ruled lines, alternating length
      lh <- 0.34
      n <- max(2L, min(5L, floor(r$h / lh / 1.9)))
      for (i in seq_len(n) - 1L) {
        add(r$x, r$y + i * lh * 1.9, r$w * if (i %% 2) 0.74 else 0.94,
            0.16, "line")
      }
    }
  }

  if (!bare) {
    add(fr$x, fr$foot_y + 0.04, cw * 0.3, 0.18, "faint")
  }

  htmltools::HTML(paste0(
    '<svg class="slb-thumb" viewBox="0 0 ', sz[["w"]] * 10, " ",
    sz[["h"]] * 10, '" preserveAspectRatio="none" aria-hidden="true">',
    paste0(unlist(acc$rects), collapse = ""),
    "</svg>"
  ))
}

# The picker: icon tiles, one per layout, the pick in the accent tint. A
# Shiny input (blockr-panels.js, kind "tiles"), so the block server's
# observeEvent(input$layout) reads the layout id as before; the same pick
# shows and hides the layout-dependent fields client-side.
slide_layout_picker <- function(input_id, selected) {

  tiles <- lapply(names(slide_layouts()), function(id) {
    name <- slide_layouts()[[id]]$name
    htmltools::tags$button(
      type = "button",
      class = "slb-tile",
      `data-value` = id,
      `aria-label` = name,
      `data-blockr-tooltip` = name,
      `data-blockr-tooltip-overflow` = NA,
      slide_layout_thumb(id),
      htmltools::tags$span(class = "slb-tile-label", name)
    )
  })

  htmltools::div(
    id = input_id,
    class = "blockr-otl-field slb-layouts",
    role = "group",
    `aria-label` = "Layout",
    `data-kind` = "tiles",
    `data-value` = selected,
    tiles
  )
}
